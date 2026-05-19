package coupledL2

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink.TLMessages._
import coupledL2.utils._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLPermissions._

// IN: a, c
// OUT: a, c
// -----------------------
// | IN | OPCODE  | OUT |
// =======================
// | A  | Acquire | A   |
// | A  | Get     | A   |
// | A  | Put     | C   |
// | C  | Release | C   |  higher priv
// -----------------------
class SinkMX(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    // in
    val a = Flipped(DecoupledIO(new TLBundleA(edgeIn.bundle)))
    val c = Flipped(DecoupledIO(new TLBundleC(edgeIn.bundle)))
    // out
    val out_a = DecoupledIO(new TLBundleA(edgeIn.bundle))
    val out_c = DecoupledIO(new TLBundleC(edgeIn.bundle))
  })

  // Default passthrough
  io.out_a <> io.a
  io.c.ready := false.B

  val matrix_key = io.a.bits.user.lift(MatrixKey).getOrElse(0.U)
  val isMatrix = MatrixInfo.isMatrix(matrix_key)
  val isRMW = MatrixInfo.isRMW(matrix_key)

  def isMatrixPut(a: TLBundleA): Bool = {
    (a.opcode === PutFullData || a.opcode === PutPartialData) && isMatrix
  }

  def isMatrixGet(a: TLBundleA): Bool = {
    a.opcode === Get && isMatrix
  }

  val a = io.a.bits
  val matrixPutC = WireDefault(0.U.asTypeOf(new TLBundleC(edgeIn.bundle)))
  val outCLock = RegInit(false.B)
  val lockedIsMatrixPut = RegInit(false.B)
  val outCMultibeatActive = RegInit(false.B)
  val matrixPutValid = io.a.valid && isMatrixPut(a)
  val (outCFirst, outCLast, _) = edgeIn.firstlast(io.out_c.bits, io.out_c.fire)
  val lockedTxn = RegEnable(io.out_c.bits, 0.U.asTypeOf(io.out_c.bits), io.out_c.fire && outCFirst)

  def matchesLockedTxn(c: TLBundleC): Bool = {
    c.opcode === lockedTxn.opcode &&
    c.param === lockedTxn.param &&
    c.size === lockedTxn.size &&
    c.source === lockedTxn.source &&
    c.address === lockedTxn.address
  }

  val nativeCMatchesLock = io.c.valid && matchesLockedTxn(io.c.bits)
  val matrixPutMatchesLock = matrixPutValid && matchesLockedTxn(matrixPutC)
  val idleSelectNativeC = io.c.valid
  val idleSelectMatrixPut = matrixPutValid && !io.c.valid
  val selectNativeC = Mux(outCLock, !lockedIsMatrixPut, idleSelectNativeC)
  val selectMatrixPut = Mux(outCLock, lockedIsMatrixPut, idleSelectMatrixPut)
  val lockedNativeCValid = outCLock && !lockedIsMatrixPut && nativeCMatchesLock
  val lockedMatrixPutValid = outCLock && lockedIsMatrixPut && matrixPutMatchesLock
  val outCNativeValid = Mux(outCLock, lockedNativeCValid, idleSelectNativeC)
  val outCMatrixValid = Mux(outCLock, lockedMatrixPutValid, idleSelectMatrixPut)
  val matrixPutBlocked = matrixPutValid && !outCMatrixValid

  when(outCMultibeatActive && io.out_c.fire) {
    assert(matchesLockedTxn(io.out_c.bits),
      "SinkMX out_c metadata changed within multibeat operation")
  }

  when(outCLock && lockedIsMatrixPut && io.a.valid) {
    assert(isMatrixPut(a), "SinkMX locked matrix put ingress changed away from matrix put")
    assert(matrixPutMatchesLock, "SinkMX locked matrix put tuple changed within multibeat operation")
  }

  when(outCLock && !lockedIsMatrixPut && io.c.valid) {
    assert(nativeCMatchesLock, "SinkMX locked native C tuple changed within multibeat operation")
  }

  when(io.out_c.fire) {
    when(outCFirst && !outCLast) {
      outCLock := true.B
      lockedIsMatrixPut := selectMatrixPut
      outCMultibeatActive := true.B
    }.elsewhen(outCLast) {
      outCLock := false.B
      lockedIsMatrixPut := false.B
      outCMultibeatActive := false.B
    }
  }

  // ======== the following handles special cases ========
  // Native C keeps priority when idle.
  // Once out_c accepts the first beat of a multibeat transaction, hold the same source until the last beat.
  matrixPutC.opcode := ReleaseData
  matrixPutC.param := TtoN
  matrixPutC.data := a.data
  matrixPutC.address := a.address
  matrixPutC.size := a.size
  matrixPutC.source := a.source
  matrixPutC.corrupt := a.corrupt
  matrixPutC.user.lift(MatrixKey).foreach(_ := matrix_key)

  io.out_c.valid := outCMatrixValid || outCNativeValid
  io.out_c.bits := Mux(selectMatrixPut, matrixPutC, io.c.bits)
  io.c.ready := outCNativeValid && io.out_c.ready

  when(selectMatrixPut) {
    io.out_a.valid := false.B
    io.a.ready := outCMatrixValid && io.out_c.ready
  }

  when(matrixPutBlocked) {
    io.out_a.valid := false.B
    io.a.ready := false.B
  }

  when(isMatrixGet(a) && io.a.valid) {
    io.out_a.bits.param := Mux(isRMW, NtoT, NtoB)
  }

}
