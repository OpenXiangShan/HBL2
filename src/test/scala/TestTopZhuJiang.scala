package zhujiang

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
import _root_.circt.stage.{ChiselStage, FirtoolOption}
import dongjiang.DJParam
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, LazyModule, LazyModuleImp, TransferSizes, ValName}
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import huancun.{AliasField, PrefetchField}
import org.chipsalliance.cde.config.{Config, Parameters}
import xijiang.{NodeParam, NodeType}
import coupledL2.{EdgeInKey, EnableCHI, EnableL2DecoupledDownstreamCHI, L1Param, L2ParamKey, L2ToL1Hint}
import coupledL2.tl2chi.{CHIIssue, HasCHIMsgParameters, Issue, TL2CHICoupledL2}
import zhujiang.device.AxiDeviceParams
import utility.{ChiselDB, FileRegisters, LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSLog, XSPerfLevel}
import xs.utils.debug.{HardwareAssertionKey, HwaParams}
import xs.utils.perf.{DebugOptions, DebugOptionsKey}
import xs.utils.perf.{LogUtilsOptions => ZJLogUtilsOptions, LogUtilsOptionsKey => ZJLogUtilsOptionsKey}
import xs.utils.perf.{PerfCounterOptions => ZJPerfCounterOptions, PerfCounterOptionsKey => ZJPerfCounterOptionsKey, XSPerfLevel => ZJXSPerfLevel}

class TestTopZhuJiang(
  numCores: Int = 1,
  numULAgents: Int = 0,
  banks: Int = 1,
  issue: String = Issue.Eb
)(implicit p: Parameters) extends LazyModule with HasCHIMsgParameters {
  override lazy val desiredName: String = "TestTop"

  require(numCores >= 1, "TestTopZhuJiang requires at least one core")

  val l2Params = p(L2ParamKey)
  private val ccNodes = p(ZJParametersKey).island.filter(_.nodeType == NodeType.CC)
  require(ccNodes.size >= numCores, s"ZhuJiang config has ${ccNodes.size} CC nodes, but $numCores cores are requested")

  def createClientNode(name: String, sources: Int): TLClientNode = TLClientNode(Seq(
    TLMasterPortParameters.v2(
      masters = Seq(TLMasterParameters.v1(
        name = name,
        sourceId = IdRange(0, sources),
        supportsProbe = freechips.rocketchip.diplomacy.TransferSizes(l2Params.blockBytes)
      )),
      channelBytes = TLChannelBeatBytes(l2Params.blockBytes),
      minLatency = 1,
      echoFields = Nil,
      requestFields = Seq(AliasField(2), PrefetchField()),
      responseKeys = l2Params.respKey
    )
  ))(ValName(name))

  val l1d_nodes = (0 until numCores).map(i => createClientNode(s"l1d$i", 64))
  val l1i_nodes = (0 until numCores).map { i =>
    (0 until numULAgents).map { j =>
      TLClientNode(Seq(
        TLMasterPortParameters.v1(
          clients = Seq(TLMasterParameters.v1(
            name = s"l1i${i}_$j",
            sourceId = IdRange(0, 32)
          ))
        )
      ))(ValName(s"l1i${i}_$j"))
    }
  }

  val l2_nodes = (0 until numCores).map(i => LazyModule(new TL2CHICoupledL2()(new Config((site, here, up) => {
    case EnableCHI => true
    case EnableL2DecoupledDownstreamCHI => true
    case L2ParamKey => l2Params.copy(
      name = s"L2_$i",
      hartId = i
    )
    case CHIIssue => issue
    case huancun.BankBitsKey => log2Ceil(banks)
    case MaxHartIdBits => log2Up(numCores)
    case LogUtilsOptionsKey => LogUtilsOptions(
      false,
      here(L2ParamKey).enablePerf,
      here(L2ParamKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(L2ParamKey).enablePerf && !here(L2ParamKey).FPGAPlatform,
      here(L2ParamKey).enableRollingDB && !here(L2ParamKey).FPGAPlatform,
      XSPerfLevel.withName("VERBOSE"),
      i
    )
  }))))

  val bankBinders = (0 until numCores).map(_ => BankBinder(banks, 64))

  val mem = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    slaves = Seq(AXI4SlaveParameters(
      address = Seq(AddressSet(0, 0xffff_ffff_ffffL)),
      supportsWrite = TransferSizes(1, 64),
      supportsRead = TransferSizes(1, 64)
    )),
    beatBytes = 32
  )))
  val zjMemMaster = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "zhujiang-ddr",
      id = IdRange(0, 8),
      aligned = true,
      maxFlight = Some(1)
    ))
  )))

  mem := AXI4Xbar() := zjMemMaster

  l1d_nodes.zip(l2_nodes).zipWithIndex.foreach { case ((l1d, l2), i) =>
    val l1xbar = TLXbar()
    l1xbar := TLBuffer() := l1d

    l1i_nodes(i).foreach { l1i =>
      l1xbar := TLBuffer() := l1i
    }

    l2.managerNode := TLXbar() :=* bankBinders(i) :*= l2.node :*= l1xbar

    val mmioClientNode = TLClientNode(Seq(
      TLMasterPortParameters.v1(
        clients = Seq(TLMasterParameters.v1(
          "uncache"
        ))
      )
    ))(ValName(s"mmio_dummy_$i"))
    l2.mmioBridge.mmioNode := mmioClientNode
  }

  lazy val module = new LazyModuleImp(this)
    with chisel3.RequireAsyncReset
    with HasZhuJiangAXI4Bridge
    with HasCHIToZhuJiangBridge {
    val time_sim = IO(Input(UInt(64.W)))

    val log = IO(new Bundle {
      val dump = Input(Bool())
      val clean = Input(Bool())
    })

    val cycle = RegInit(0.U(64.W))
    cycle := cycle + 1.U

    val timer = WireDefault(time_sim)
    val logEnable = WireDefault(true.B)
    val clean = WireDefault(log.clean)
    val dump = WireDefault(log.dump)

    val difftest_timer = IO(Output(UInt(64.W)))
    val difftest_log_enable = IO(Output(Bool()))
    val difftest_perfCtrl_clean = IO(Output(Bool()))
    val difftest_perfCtrl_dump = IO(Output(Bool()))

    difftest_timer := timer
    difftest_log_enable := logEnable
    difftest_perfCtrl_clean := clean
    difftest_perfCtrl_dump := dump

    val io_l1 = l2_nodes.map { l2_node =>
      IO(new Bundle {
        val l2Hint = Valid(new L2ToL1Hint()(p.alterPartial { case EdgeInKey => l2_node.node.in.head._2 }))
      })
    }

    val zj = withClockAndReset(clock, reset) { Module(new Zhujiang) }

    require(zj.ccnIO.size >= numCores, s"ZhuJiang exposes ${zj.ccnIO.size} CCN IOs, but $numCores cores are requested")
    l2_nodes.zipWithIndex.foreach { case (l2, i) =>
      connectCHIToZhuJiang(l2.module.io_decoupled_chi.get, zj.ccnIO(i), ccNodes(i), p)

      l2.module.io_nodeID := ccNodes(i).nodeId.U
      l2.module.io.hartId := i.U
      l2.module.io.pfCtrlFromCore := 0.U.asTypeOf(l2.module.io.pfCtrlFromCore)
      l2.module.io.l2_tlb_req := DontCare
      l2.module.io.debugTopDown.robTrueCommit := 0.U
      l2.module.io.debugTopDown.robHeadPaddr.valid := false.B
      l2.module.io.debugTopDown.robHeadPaddr.bits := 0.U
      l2.module.io.l2_hint <> io_l1(i).l2Hint
      l2.module.io.l2Flush.foreach(_ := false.B)
      l2.module.io_cpu_wfi.foreach(_ := false.B)
      l2.module.io.dft.foreach(_ := DontCare)
      l2.module.io.dft_reset.foreach(_ := DontCare)
    }

    zj.io.ci := 0.U
    zj.io.dft := DontCare
    zj.io.ramctl := DontCare
    val (zjMemAxi, _) = zjMemMaster.out.head
    require(zj.ddrIO.nonEmpty, "ZhuJiang test top expects at least one DDR AXI port")
    connectZJToAXI4(zj.ddrIO.head, zjMemAxi)
    mem.makeIOs()(ValName("mem_axi"))
    zj.ddrIO.drop(1).foreach { axi => axi := DontCare }
    zj.cfgIO.foreach { axi => axi := DontCare }
    zj.dmaIO.foreach { axi => axi := DontCare }
    zj.hwaIO.foreach { axi => axi := DontCare }

    l1d_nodes.zipWithIndex.foreach { case (node, i) =>
      node.makeIOs()(ValName(s"master_port_$i"))
    }
    l1i_nodes.zipWithIndex.foreach { case (core, i) =>
      core.zipWithIndex.foreach { case (node, j) =>
        node.makeIOs()(ValName(s"master_ul_port_${i}_$j"))
      }
    }

    XSLog.collect(difftest_timer, difftest_log_enable, difftest_perfCtrl_clean, difftest_perfCtrl_dump)
  }
}

object ZhuJiangNocConfig {
  def SingleCoreNocConfig: ZJParameters = ZJParameters(
    nodeNidBits = 8,
    nodeAidBits = 3,
    nodeParams = singleCoreNodeParams,
    djParamsOpt = Some(hnfParams),
    tfbParams = None
  )

  def DualCoreNocConfig: ZJParameters = ZJParameters(
    nodeNidBits = 8,
    nodeAidBits = 3,
    nodeParams = dualCoreNodeParams,
    djParamsOpt = Some(hnfParams),
    tfbParams = None
  )

  private def hnfParams: DJParam = DJParam(
    llcSizeInB = 8 * 4 * 64,
    llcWays = 4,
    sfWays = 4
  )

  private def singleCoreNodeParams: Seq[NodeParam] = Seq(
    NodeParam(nodeType = NodeType.HF, bankId = 0, hfpId = 0),
    NodeParam(nodeType = NodeType.CC, socket = "sync"),
    NodeParam(nodeType = NodeType.HF, bankId = 1, hfpId = 0),
    NodeParam(nodeType = NodeType.RI, axiDevParams = Some(AxiDeviceParams(wrapper = "east", attr = "main"))),
    NodeParam(nodeType = NodeType.HI, axiDevParams = Some(AxiDeviceParams(wrapper = "east", attr = "main")), defaultHni = true),
    NodeParam(nodeType = NodeType.HF, bankId = 1, hfpId = 1),
    NodeParam(nodeType = NodeType.S, axiDevParams = Some(AxiDeviceParams(wrapper = "south", attr = "mem_0"))),
    NodeParam(nodeType = NodeType.HF, bankId = 0, hfpId = 1),
    NodeParam(nodeType = NodeType.M, axiDevParams = Some(AxiDeviceParams(wrapper = "misc", attr = "hwa"))),
    NodeParam(nodeType = NodeType.P)
  )

  private def dualCoreNodeParams: Seq[NodeParam] = Seq(
    NodeParam(nodeType = NodeType.HF, bankId = 0, hfpId = 0),
    NodeParam(nodeType = NodeType.CC, socket = "sync"),
    NodeParam(nodeType = NodeType.HF, bankId = 1, hfpId = 0),
    NodeParam(nodeType = NodeType.CC, socket = "sync"),
    NodeParam(nodeType = NodeType.RI, axiDevParams = Some(AxiDeviceParams(wrapper = "east", attr = "main"))),
    NodeParam(nodeType = NodeType.HI, axiDevParams = Some(AxiDeviceParams(wrapper = "east", attr = "main")), defaultHni = true),
    NodeParam(nodeType = NodeType.HF, bankId = 1, hfpId = 1),
    NodeParam(nodeType = NodeType.S, axiDevParams = Some(AxiDeviceParams(wrapper = "south", attr = "mem_0"))),
    NodeParam(nodeType = NodeType.HF, bankId = 0, hfpId = 1),
    NodeParam(nodeType = NodeType.M, axiDevParams = Some(AxiDeviceParams(wrapper = "misc", attr = "hwa"))),
    NodeParam(nodeType = NodeType.P)
  )
}

object TestTopZhuJiangConfig {
  private def base(numCores: Int, zjParameters: ZJParameters): Config = new Config((site, _, up) => {
    case EnableCHI => true
    case EnableL2DecoupledDownstreamCHI => true
    case CHIIssue => Issue.Eb
    case huancun.BankBitsKey => 0
    case MaxHartIdBits => log2Up(numCores)
    case ZJParametersKey => zjParameters
    case L2ParamKey => up(L2ParamKey).copy(
      ways = 4,
      sets = 128,
      clientCaches = Seq.fill(numCores)(L1Param(aliasBitsOpt = Some(2))),
      dataCheck = None,
      enablePoison = false,
      sam = Seq(AddressSet.everything -> 0)
    )
    case LogUtilsOptionsKey => LogUtilsOptions(
      false,
      site(L2ParamKey).enablePerf,
      site(L2ParamKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      site(L2ParamKey).enablePerf && !site(L2ParamKey).FPGAPlatform,
      site(L2ParamKey).enableRollingDB && !site(L2ParamKey).FPGAPlatform,
      XSPerfLevel.withName("VERBOSE"),
      0
    )
    case HardwareAssertionKey => HwaParams(enable = false)
    case ZJLogUtilsOptionsKey => ZJLogUtilsOptions(
      enableDebug = false,
      enablePerf = site(L2ParamKey).enablePerf,
      fpgaPlatform = site(L2ParamKey).FPGAPlatform
    )
    case ZJPerfCounterOptionsKey => ZJPerfCounterOptions(
      enablePerfPrint = site(L2ParamKey).enablePerf && !site(L2ParamKey).FPGAPlatform,
      enablePerfDB = site(L2ParamKey).enableRollingDB && !site(L2ParamKey).FPGAPlatform,
      perfLevel = ZJXSPerfLevel.withName("VERBOSE"),
      perfDBHartID = 0
    )
    case DebugOptionsKey => DebugOptions(
      FPGAPlatform = site(L2ParamKey).FPGAPlatform,
      EnableDifftest = false,
      AlwaysBasicDiff = false,
      EnableDebug = false,
      EnablePerfDebug = false,
      UseDRAMSim = false,
      EnableTopDown = false,
      EnableChiselDB = false,
      AlwaysBasicDB = false,
      EnableRollingDB = false,
      EnableHWMoniter = false
    )
  })

  def singleCore: Config = base(1, ZhuJiangNocConfig.SingleCoreNocConfig)

  def dualCore: Config = base(2, ZhuJiangNocConfig.DualCoreNocConfig)

  def gen(config: Config)(top: Parameters => TestTopZhuJiang)(args: Array[String]): Unit = {
    ChiselDB.init(false)

    val lazyTop = LazyModule(top(config))
    (new ChiselStage).execute(
      args,
      Seq(
        ChiselGeneratorAnnotation(() => lazyTop.module),
        FirtoolOption("--disable-annotation-unknown")
      )
    )

    ChiselDB.addToFileRegisters
    FileRegisters.write("./build")
  }
}

object TestTopZhuJiang_SingleCore extends App {
  TestTopZhuJiangConfig.gen(TestTopZhuJiangConfig.singleCore)(p => new TestTopZhuJiang(
    numCores = 1,
    numULAgents = 2,
    banks = 1
  )(p))(args)
}

object TestTopZhuJiang_DualCore extends App {
  TestTopZhuJiangConfig.gen(TestTopZhuJiangConfig.dualCore)(p => new TestTopZhuJiang(
    numCores = 2,
    numULAgents = 2,
    banks = 1
  )(p))(args)
}
