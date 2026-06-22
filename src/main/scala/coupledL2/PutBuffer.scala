package coupledL2

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import org.chipsalliance.cde.config.Parameters

class PutBufState(implicit p: Parameters) extends L2Bundle {
  val entryIdx = UInt(bufIdxBits.W)
}

class PutBufWrite(implicit p: Parameters) extends L2Bundle {
  val data  = UInt((beatBytes * 8).W)
  val beat  = UInt(beatBits.W)
  val first = Bool()
  val last  = Bool()
}

class PutBufRead(implicit p: Parameters) extends L2Bundle {
  val id = UInt(bufIdxBits.W)
}

class PutBufResp(implicit p: Parameters) extends L2Bundle {
  val data = new DSBlock
}

class PutBuffer(implicit p: Parameters) extends L2Module {
  // ------------------------------------------ IO Declaration ------------------------------------------ //
  val io = IO(new Bundle {
    // Read and write requests
    val w     = Flipped(DecoupledIO(new PutBufWrite)) // write request from SinkA
    val r     = Flipped(ValidIO(new PutBufRead))      // read request from RequestArb

    val state = Output(new PutBufState) // PutBuffer state exposed to SinkA for task allocation

    val resp  = Output(new PutBufResp)  // Block-level read response to MainPipe s3
    
  })

  // ------------------------------------------ Wire/Reg Declaration ------------------------------------ //
  private val buffer = RegInit(
    VecInit(
      Seq.fill(bufBlocks)
        (Valid(Vec(beatSize, UInt((beatBytes * 8).W))).Lit(_.valid -> false.B))
    )
  )

  private val valids   = VecInit(buffer.map(_.valid))
  private val full     = valids.asUInt.andR
  private val freeMask = ~valids.asUInt

  // In PutBuffer, the selected entry MUST BE held in a register.
  // It cannot dynamically compute sel with PriorityEncoder every cycle like RequestBuffer.
  // This is because PutBuffer performs multi-beat writes. If sel is dynamically computed every cycle,
  // different beats of the same write may be written into different entries. For example:
  // cycle 0: sel = 1, write beat 0 to entry 1
  // also in cycle 0: data in entry 0 is consumed, entry 0 becomes free
  // cycle 1: Because of the priority of entry 0 is higher than entry 1, sel = 0, write beat 1 to entry 0 -> Data Mismatch!
  private val sel_r    = RegInit(0.U(bufIdxBits.W))
  private val sel_nxt  = PriorityEncoder(freeMask)

  private val sel = Mux(io.w.bits.first, sel_nxt, sel_r)

  // ------------------------------------------ Main Logic ----------------------------------------- /
  /* Write logic */
  buffer.zipWithIndex.foreach { case (entry, i) =>
    val wen = io.w.fire && sel === i.U
    val readThis = io.r.valid && io.r.bits.id === i.U

    when (wen) {
      entry.bits(io.w.bits.beat) := io.w.bits.data

      when (io.w.bits.last) {
        entry.valid := true.B
      }
    }
  }

  // Store the selected entry when the **FIRST** beat fires.
  // DO NOT update sel_r on the last beat: the current entry may be the last free entry,
  // so selecting a new entry at that point may produce an invalid or meaningless result
  // and cause subsequent writes to use the wrong entry.
  when (io.w.fire && io.w.bits.first) {
    sel_r := sel_nxt
  }
  
  // read logic
  when (io.r.valid) {
    buffer(io.r.bits.id).valid := false.B
  }

  private val rdata = RegEnable(buffer(io.r.bits.id).bits.asUInt, io.r.valid)

  // ------------------------------------------ IO Assignment ------------------------------------- //
  io.state.entryIdx := sel_r
  io.w.ready        := !full
  io.resp.data.data := rdata

  // ------------------------------------------ Dont Touch ------------------------------------- //
  dontTouch(freeMask)
  dontTouch(full)
  dontTouch(sel_r)
  dontTouch(sel_nxt)
  dontTouch(sel)
}
