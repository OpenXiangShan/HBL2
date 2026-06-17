package coupledL2

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import org.chipsalliance.cde.config.Parameters

class PutBufState(implicit p: Parameters) extends L2Bundle {
  val entryIdx = UInt(bufIdxBits.W)
}

class PutBufWrite(implicit p: Parameters) extends L2Bundle {
  val data = UInt((beatBytes * 8).W)
  val beat = UInt(beatBits.W)
  val last = Bool()
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
  private val sel      = PriorityEncoder(freeMask)

  // ------------------------------------------ Main Logic ----------------------------------------- /
  // Write logic
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
  
  // read logic
  when (io.r.valid) {
    buffer(io.r.bits.id).valid := false.B
  }

  private val rdata = RegEnable(
    buffer(io.r.bits.id).bits.asUInt,
    0.U(blockBits.W),
    io.r.valid
  )

  // ------------------------------------------ IO Assignment ------------------------------------- //
  io.state.entryIdx := sel
  io.w.ready        := !full
  io.resp.data.data := rdata

  // ------------------------------------------ Dont Touch ------------------------------------- //
  dontTouch(freeMask)
  dontTouch(full)
  dontTouch(sel)
}
