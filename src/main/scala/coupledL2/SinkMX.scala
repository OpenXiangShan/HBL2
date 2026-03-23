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
  io.out_c <> io.c

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
  val c = io.c.bits

  // ======== the following handles special cases ========
  // If it's a matrix put and C channel is not valid, convert it to ReleaseData on C channel
  when(io.a.valid && isMatrixPut(a) && !io.c.valid) {
    io.out_c.bits.opcode := ReleaseData
    io.out_c.bits.param := TtoN
    io.out_c.bits.data := a.data
    io.out_c.bits.address := a.address
    io.out_c.bits.size := a.size
    io.out_c.bits.source := a.source
    io.out_c.bits.corrupt := a.corrupt
    // io.out_c.bits.user(VaddrKey) := a.address
    io.out_c.bits.user.lift(MatrixKey).foreach(_ := matrix_key)

    io.out_a.valid := false.B
    io.out_c.valid := true.B
    io.a.ready := io.out_c.ready
    io.c.ready := false.B
  }

  when(io.a.valid && isMatrixPut(a) && io.c.valid) {
    io.out_a.valid := false.B
    io.out_c.valid := true.B
    io.a.ready := false.B
    io.c.ready := io.out_c.ready
  }

  when(isMatrixGet(a) && io.a.valid) {
    io.out_a.bits.param := Mux(isRMW, NtoT, NtoB)
  }


}
