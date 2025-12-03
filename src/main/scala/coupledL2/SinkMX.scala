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

  val matrix_key = io.a.bits.user.lift(MatrixKey).getOrElse(0.U)
  val isMatrix = MatrixInfo.isMatrix(matrix_key)
  val isRMW = MatrixInfo.isRMW(matrix_key)

  def isMatrixPut(a: TLBundleA): Bool = {
    (a.opcode === PutFullData || a.opcode === PutPartialData) && isMatrix
  }

  def isMatrixGet(a: TLBundleA): Bool = {
    a.opcode === Get && isMatrix
  }

  val out_a = WireInit(io.a)
  val out_c = WireInit(io.c)

  val a = io.a.bits
  val c = io.c.bits

  // Determine out_a.valid:
  // 1. if a is not a matrix put, flow io.a to out_a
  // 2. if a is a matrix put, and in_c is not valid, flow io.a to out_c, handled above
  // 3. if a is a matrix put, and in_c is valid, stall a

  // Handle MatrixGet
  when(isMatrixGet(a) && io.a.valid) {
    out_a.bits.opcode := io.a.bits.opcode//Get Or AcquireBlock
    // although TileLink requires Get's param fixed 0 (NtoB),
    // here we specifically design Get with NtoT, which also triggers needT in L2
    // but when sending to L3, we will use Acquire NtoT
    out_a.bits.param := Mux(isRMW, NtoT, NtoB)
  }

  // Handle MatrixPut
  when(isMatrixPut(a) && io.a.valid) {
    // By anycase, CoupledL2 cannot accept Put from A channel.
    // Wait for C channel be spare.
    out_a.valid := false.B

    when(!io.c.valid) {
      // If c is not valid, convert the request to ReleaseData
      out_c.bits.opcode := ReleaseData
      out_c.bits.param := TtoN // Set appropriate parameters for ReleaseData
      out_c.bits.data := a.data // Use data from the A channel
      out_c.bits.address := a.address // Use address from the A channel
      out_c.bits.size := a.size
      out_c.bits.source := a.source
      out_c.bits.corrupt := a.corrupt
      // out_c.bits.user(VaddrKey) := a.address
      out_c.valid := true.B
      out_c.bits.user.lift(MatrixKey).foreach(_ := matrix_key)
    }
  }

  // Connect output signals
  io.out_a <> out_a
  io.out_c <> out_c

  // Handle ready signals
  // io.a.ready := out_a.ready
  io.c.ready := out_c.ready
  // Bypass channel A matrix put to C, but stalled for original C channel requests.
  // TODO: it is not recommended to use input valid to drive input ready,
  // might cause longer path, unfriendly to timing
  io.a.ready := Mux(isMatrixPut(a), io.out_c.ready && !io.c.valid, io.out_a.ready)
}
