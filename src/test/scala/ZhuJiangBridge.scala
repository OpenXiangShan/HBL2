package zhujiang

import chisel3._
import chisel3.util.Cat
import freechips.rocketchip.amba.axi4.AXI4Bundle
import org.chipsalliance.cde.config.Parameters
import xijiang.{Node, NodeType}
import coupledL2.tl2chi.{CHIDAT, CHIREQ, CHIRSP, CHISNP, DecoupledPortIO, MemAttr}
import zhujiang.axi.ExtAxiBundle
import zhujiang.chi.{DataFlit, ReqFlit, RespFlit, SnoopFlit}
import zhujiang.device.socket.{SocketDevSide, SocketIcnSideBundle}

trait HasZhuJiangAXI4Bridge {
  protected def connectZJToAXI4(zjAxi: ExtAxiBundle, axi: AXI4Bundle): Unit = {
    require(zjAxi.params.addrBits <= axi.params.addrBits)
    require(zjAxi.params.dataBits == axi.params.dataBits)
    require(zjAxi.params.idBits <= axi.params.idBits)
    require(zjAxi.params.lenBits == axi.params.lenBits)
    require(zjAxi.params.sizeBits == axi.params.sizeBits)
    require(zjAxi.params.burstBits == axi.params.burstBits)
    require(zjAxi.params.lockBits == axi.params.lockBits)
    require(zjAxi.params.cacheBits == axi.params.cacheBits)
    require(zjAxi.params.qosBits == axi.params.qosBits)

    axi.aw.valid := zjAxi.awvalid
    zjAxi.awready := axi.aw.ready
    axi.aw.bits.id := zjAxi.awid
    axi.aw.bits.addr := zjAxi.awaddr
    axi.aw.bits.len := zjAxi.awlen
    axi.aw.bits.size := zjAxi.awsize
    axi.aw.bits.burst := zjAxi.awburst
    axi.aw.bits.lock := zjAxi.awlock
    axi.aw.bits.cache := zjAxi.awcache
    axi.aw.bits.prot := zjAxi.awprot
    axi.aw.bits.qos := zjAxi.awqos
    axi.aw.bits.user := DontCare
    axi.aw.bits.echo := DontCare

    axi.ar.valid := zjAxi.arvalid
    zjAxi.arready := axi.ar.ready
    axi.ar.bits.id := zjAxi.arid
    axi.ar.bits.addr := zjAxi.araddr
    axi.ar.bits.len := zjAxi.arlen
    axi.ar.bits.size := zjAxi.arsize
    axi.ar.bits.burst := zjAxi.arburst
    axi.ar.bits.lock := zjAxi.arlock
    axi.ar.bits.cache := zjAxi.arcache
    axi.ar.bits.prot := zjAxi.arprot
    axi.ar.bits.qos := zjAxi.arqos
    axi.ar.bits.user := DontCare
    axi.ar.bits.echo := DontCare

    axi.w.valid := zjAxi.wvalid
    zjAxi.wready := axi.w.ready
    axi.w.bits.data := zjAxi.wdata
    axi.w.bits.strb := zjAxi.wstrb
    axi.w.bits.last := zjAxi.wlast.asBool
    axi.w.bits.user := DontCare

    zjAxi.bvalid := axi.b.valid
    axi.b.ready := zjAxi.bready
    zjAxi.bid := axi.b.bits.id
    zjAxi.bresp := axi.b.bits.resp
    zjAxi.buser := 0.U

    zjAxi.rvalid := axi.r.valid
    axi.r.ready := zjAxi.rready
    zjAxi.rid := axi.r.bits.id
    zjAxi.rdata := axi.r.bits.data
    zjAxi.rresp := axi.r.bits.resp
    zjAxi.rlast := axi.r.bits.last
    zjAxi.ruser := 0.U
  }
}

trait HasCHIToZhuJiangBridge {
  protected def connectCHIToZhuJiang(
    chi: DecoupledPortIO,
    socketIO: SocketIcnSideBundle,
    ccNode: Node,
    zjParams: Parameters
  ): Unit = {
    implicit val p: Parameters = zjParams
    require(ccNode.nodeType == NodeType.CC)
    require(ccNode.socket == "sync")
    require(
      chi.tx.dat.bits.dataCheck.isEmpty && chi.rx.dat.bits.dataCheck.isEmpty,
      "ZhuJiang bridge requires CHI DataCheck disabled"
    )
    require(
      chi.tx.dat.bits.poison.isEmpty && chi.rx.dat.bits.poison.isEmpty,
      "ZhuJiang bridge requires CHI Poison disabled"
    )

    val socket = Module(new SocketDevSide(ccNode))
    socketIO <> socket.io.socket

    val txreqBits = Wire(new ReqFlit)
    mapReq(txreqBits, chi.tx.req.bits)
    socket.io.icn.rx.req.get.valid := chi.tx.req.valid
    socket.io.icn.rx.req.get.bits := txreqBits
    chi.tx.req.ready := socket.io.icn.rx.req.get.ready

    val txrspBits = Wire(new RespFlit)
    mapRsp(txrspBits, chi.tx.rsp.bits)
    socket.io.icn.rx.resp.get.valid := chi.tx.rsp.valid
    socket.io.icn.rx.resp.get.bits := txrspBits
    chi.tx.rsp.ready := socket.io.icn.rx.resp.get.ready

    val txdatBits = Wire(new DataFlit)
    mapDat(txdatBits, chi.tx.dat.bits)
    socket.io.icn.rx.data.get.valid := chi.tx.dat.valid
    socket.io.icn.rx.data.get.bits := txdatBits
    chi.tx.dat.ready := socket.io.icn.rx.data.get.ready

    socket.io.icn.rx.hpr.foreach { hpr =>
      hpr.valid := false.B
      hpr.bits := 0.U.asTypeOf(hpr.bits)
    }
    socket.io.icn.rx.snoop.foreach { snp =>
      snp.valid := false.B
      snp.bits := 0.U.asTypeOf(snp.bits)
    }
    socket.io.icn.rx.debug.foreach { dbg =>
      dbg.valid := false.B
      dbg.bits := 0.U.asTypeOf(dbg.bits)
    }

    val rxrspBits = Wire(new CHIRSP)
    mapRsp(rxrspBits, socket.io.icn.tx.resp.get.bits.asTypeOf(new RespFlit))
    chi.rx.rsp.valid := socket.io.icn.tx.resp.get.valid
    chi.rx.rsp.bits := rxrspBits
    socket.io.icn.tx.resp.get.ready := chi.rx.rsp.ready

    val rxdatBits = Wire(new CHIDAT)
    mapDat(rxdatBits, socket.io.icn.tx.data.get.bits.asTypeOf(new DataFlit))
    chi.rx.dat.valid := socket.io.icn.tx.data.get.valid
    chi.rx.dat.bits := rxdatBits
    socket.io.icn.tx.data.get.ready := chi.rx.dat.ready

    val rxsnpBits = Wire(new CHISNP)
    mapSnp(rxsnpBits, socket.io.icn.tx.snoop.get.bits.asTypeOf(new SnoopFlit))
    chi.rx.snp.valid := socket.io.icn.tx.snoop.get.valid
    chi.rx.snp.bits := rxsnpBits
    socket.io.icn.tx.snoop.get.ready := chi.rx.snp.ready

    socket.io.icn.tx.req.foreach(_.ready := false.B)
    socket.io.icn.tx.hpr.foreach(_.ready := false.B)
    socket.io.icn.tx.debug.foreach(_.ready := false.B)
  }

  protected def mapReq(dst: ReqFlit, src: CHIREQ): Unit = {
    dst := 0.U.asTypeOf(dst)
    dst.QoS := src.qos
    dst.TgtID := src.tgtID
    dst.SrcID := src.srcID
    dst.TxnID := src.txnID
    dst.Opcode := src.opcode
    dst.Size := src.size
    dst.Addr := src.addr
    dst.Order := src.order
    dst.MemAttr := Cat(src.memAttr.allocate, src.memAttr.cacheable, src.memAttr.device, src.memAttr.ewa)
    dst.SnpAttr := src.snpAttr
    dst.Excl := src.excl
    dst.ExpCompAck := src.expCompAck
    dst.RSVDC := src.rsvdc
    dst.PBHA := src.mpam.map(_.partID).getOrElse(0.U)
    dst.MPAM := src.mpam.map(_.partID).getOrElse(0.U)
    dst.ReturnTxnID.foreach(_ := src.returnTxnID)
    dst.ReturnNID.foreach(_ := src.returnNID)
  }

  protected def mapRsp(dst: RespFlit, src: CHIRSP): Unit = {
    dst := 0.U.asTypeOf(dst)
    dst.QoS := src.qos
    dst.TgtID := src.tgtID
    dst.SrcID := src.srcID
    dst.TxnID := src.txnID
    dst.Opcode := src.opcode
    dst.RespErr := src.respErr
    dst.Resp := src.resp
    dst.FwdState := src.fwdState
    dst.CBusy := src.cBusy.getOrElse(0.U)
    dst.DBID := src.dbID
  }

  protected def mapRsp(dst: CHIRSP, src: RespFlit): Unit = {
    dst := 0.U.asTypeOf(dst)
    dst.qos := src.QoS
    dst.srcID := src.SrcID
    dst.txnID := src.TxnID
    dst.opcode := src.Opcode
    dst.respErr := src.RespErr
    dst.resp := src.Resp
    dst.fwdState := src.FwdState
    dst.cBusy.foreach(_ := src.CBusy)
    dst.dbID := src.DBID
    dst.pCrdType := 0.U
  }

  protected def mapDat(dst: DataFlit, src: CHIDAT): Unit = {
    dst := 0.U.asTypeOf(dst)
    dst.QoS := src.qos
    dst.TgtID := src.tgtID
    dst.SrcID := src.srcID
    dst.TxnID := src.txnID
    dst.Opcode := src.opcode
    dst.RespErr := src.respErr
    dst.Resp := src.resp
    dst.FwdState := src.fwdState
    dst.DataSource := src.dataSource
    dst.CBusy := src.cBusy.getOrElse(0.U)
    dst.DBID := src.dbID
    dst.DataID := src.dataID
    dst.Data := src.data
    dst.BE := src.be
  }

  protected def mapDat(dst: CHIDAT, src: DataFlit): Unit = {
    dst := 0.U.asTypeOf(dst)
    dst.qos := src.QoS
    dst.tgtID := src.TgtID
    dst.srcID := src.SrcID
    dst.txnID := src.TxnID
    dst.homeNID := src.HomeNID
    dst.opcode := src.Opcode
    dst.respErr := src.RespErr
    dst.resp := src.Resp
    dst.dataSource := src.DataSource
    dst.cBusy.foreach(_ := src.CBusy)
    dst.dbID := src.DBID
    dst.ccID := 0.U
    dst.dataID := src.DataID
    dst.data := src.Data
    dst.be := src.BE
    dst.poison.foreach(_ := 0.U)
  }

  protected def mapSnp(dst: CHISNP, src: SnoopFlit): Unit = {
    dst := 0.U.asTypeOf(dst)
    dst.qos := src.QoS
    dst.srcID := src.SrcID
    dst.txnID := src.TxnID
    dst.fwdNID := src.FwdNID
    dst.fwdTxnID := src.FwdTxnID
    dst.opcode := src.Opcode
    dst.addr := src.Addr
    dst.ns := false.B
    dst.doNotGoToSD := src.DoNotGoToSD
    dst.retToSrc := src.RetToSrc
  }
}
