package coupledL2

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink.TLMessages._
import coupledL2.utils._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLPermissions._

// SourceMX: merge sourceA and sourceC.
// Default: sourceA -> out_a, sourceC -> out_c.
// If sourceC issues PutFullData, route it to out_a instead (suppress out_c).
class SourceMX(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    // inputs from local sources
    val a = Flipped(DecoupledIO(new TLBundleA(edgeOut.bundle)))
    val c = Flipped(DecoupledIO(new TLBundleC(edgeOut.bundle)))
    // outputs to next-level
    val out_a = DecoupledIO(new TLBundleA(edgeOut.bundle))
    val out_c = DecoupledIO(new TLBundleC(edgeOut.bundle))
  })

  val out_a = WireInit(io.a)
  val out_c = WireInit(io.c)

  val a = io.a.bits
  val c = io.c.bits

  def isCPutFull(cbits: TLBundleC): Bool = cbits.opcode === PutFullData

  // When c issues PutFullData, transform it into an A-channel PutFullData and send to out_a
  when(isCPutFull(c) && io.c.valid) {
    out_c.valid := false.B

    when(!io.a.valid) {
        out_a.bits.opcode := PutFullData
        out_a.bits.param := 0.U
        out_a.bits.data := c.data
        out_a.bits.address := c.address
        out_a.bits.size := c.size
        out_a.bits.source := c.source
        out_a.bits.corrupt := c.corrupt
        out_a.valid := true.B
        // propagate user fields if present
        out_a.bits.user := c.user
        out_a.bits.echo := c.echo
    }
  }

  // Connect outputs
  io.out_a <> out_a
  io.out_c <> out_c

  // Ready signals:
  // - a ready follows out_a.ready normally
  // - c ready follows out_c.ready normally; but if c is PutFullData, drive by out_a.ready and avoid conflict with a.valid
  io.a.ready := out_a.ready
  io.c.ready := Mux(isCPutFull(c), io.out_a.ready && !io.a.valid, io.out_c.ready)
}
