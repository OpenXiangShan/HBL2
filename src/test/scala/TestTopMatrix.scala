package coupledL2

import chisel3._
import circt.stage.{ChiselStage, FirtoolOption}
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile.MaxHartIdBits
import huancun._
import coupledL2.prefetch._
import coupledL2.tl2tl._
import utility._

object TestTopMatrixParams {
  val l2Sets: Int = 64
  val l2Ways: Int = 8
  val l2Banks: Int = 1
  val l3CDirSets: Int = 128
  val l3CDirWays: Int = 6
  val l3Sets: Int = 512
  val l3Ways: Int = 8
  val l3Banks: Int = 1
}

case object L2BanksKey extends Field[Int]
case object L3BanksKey extends Field[Int]
case object MNumKey extends Field[Int]

object baseConfigMatrix {
  def apply(maxHartIdBits: Int, l2Banks: Int, l3Banks: Int, mNum: Int) = {
    new Config((_, _, _) => {
      case MaxHartIdBits => maxHartIdBits
      case EnableMatrix => true
      case L2BanksKey => l2Banks
      case L3BanksKey => l3Banks
      case MNumKey => mNum
    })
  }
}

object TestTopMatrixFirtoolOptions {
  def apply() = Seq(
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--repl-seq-mem"),
    FirtoolOption("--repl-seq-mem-file=TestTop.sv.conf"),
    FirtoolOption("--lowering-options=explicitBitcast")
  )
}

class TestTop_L2L3_Matrix()(implicit p: Parameters) extends LazyModule {

  /* L1D   L1I   Matrix
   *   \    |    /
   *        L2
   *         |
   *        L3
   */

  override lazy val desiredName: String = "TestTop"
  val cacheParams = p(L2ParamKey)
  val l2Banks = p(L2BanksKey)
  val l3Banks = p(L3BanksKey)
  val mNum = p(MNumKey)

  def createClientNode(name: String, sources: Int) = {
    implicit val valName: ValName = ValName(name)
    TLClientNode(Seq(
      TLMasterPortParameters.v2(
        masters = Seq(
          TLMasterParameters.v1(
            name = name,
            sourceId = IdRange(0, sources),
            supportsProbe = TransferSizes(cacheParams.blockBytes)
          )
        ),
        channelBytes = TLChannelBeatBytes(cacheParams.blockBytes),
        minLatency = 1,
        echoFields = Nil,
        requestFields = Seq(AliasField(2), PrefetchField()),
        responseKeys = cacheParams.respKey
      )
    ))
  }

  val l1d = createClientNode("l1d", 32)
  val l1i = TLClientNode(Seq(
    TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = "l1i",
        sourceId = IdRange(0, 32)
      ))
    )
  ))

  val matrixNodes = (0 until mNum).map { i =>
    implicit val valName: ValName = ValName(s"matrix_$i")
    TLClientNode(Seq(
      TLMasterPortParameters.v2(
        masters = Seq(
          TLMasterParameters.v1(
            name = s"matrix_$i",
            sourceId = IdRange(0, 32)
          )
        ),
        channelBytes = TLChannelBeatBytes(cacheParams.blockBytes),
        minLatency = 1,
        requestFields = Seq(MatrixField(2), AmeIndexField())
      )
    ))
  }

  val cNodes = Seq(l1d)
  val l1iNodes = Seq(l1i)

  val l2 = LazyModule(new TL2TLCoupledL2()(baseConfigMatrix(1, l2Banks, l3Banks, mNum).alter((site, here, up) => {
    case L2ParamKey => L2Param(
      name = "l2",
      ways = TestTopMatrixParams.l2Ways,
      sets = TestTopMatrixParams.l2Sets,
      channelBytes = TLChannelBeatBytes(32),
      blockBytes = 64,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField()),
      enableTagECC = false,
      enableDataECC = false,
      enableMCP2 = false,
      enableRollingDB = false,
      enablePoison = false
    )
    case BankBitsKey => log2Ceil(l2Banks)
    case LogUtilsOptionsKey => LogUtilsOptions(
      false,
      here(L2ParamKey).enablePerf,
      here(L2ParamKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(L2ParamKey).enablePerf && !here(L2ParamKey).FPGAPlatform,
      false,
      XSPerfLevel.withName("VERBOSE"),
      0
    )
  })))

  val l3 = LazyModule(new HuanCun()(baseConfigMatrix(1, l2Banks, l3Banks, mNum).alter((site, here, up) => {
    case HCCacheParamsKey => HCCacheParameters(
      name = "l3",
      level = 3,
      ways = TestTopMatrixParams.l3Ways,
      sets = TestTopMatrixParams.l3Sets,
      inclusive = false,
      clientCaches = Seq(
        CacheParameters(
          name = "l2",
          sets = TestTopMatrixParams.l3CDirSets,
          ways = TestTopMatrixParams.l3CDirWays,
          blockGranularity = log2Ceil(TestTopMatrixParams.l3CDirSets)
        )
      ),
      echoField = Seq(DirtyField()),
      simulation = true
    )
    case LogUtilsOptionsKey => LogUtilsOptions(
      here(HCCacheParamsKey).enableDebug,
      here(HCCacheParamsKey).enablePerf,
      here(HCCacheParamsKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(HCCacheParamsKey).enablePerf && !here(HCCacheParamsKey).FPGAPlatform,
      false,
      XSPerfLevel.withName("VERBOSE"),
      0
    )
  })))

  val l1xbar = LazyModule(new TLXbar()).suggestName("L1_xbar").node
  val l2xbar = LazyModule(new TLXbar()).suggestName("L2_xbar").node
  val l3xbar = LazyModule(new TLXbar()).suggestName("L3_xbar").node
  val memxbar = LazyModule(new TLXbar()).suggestName("MEM_xbar").node

  val l2bankBinders = BankBinder(l2Banks, 64)
  val l3bankBinders = BankBinder(l3Banks, 64)
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xffff_ffffL), beatBytes = 32))

  cNodes.zipWithIndex.foreach { case (node, i) =>
    l1xbar := TLBuffer() := TLLogger(s"L2_L1D[${i}]", true) := node
  }

  l1iNodes.zipWithIndex.foreach { case (node, i) =>
    l1xbar := TLBuffer() := TLLogger(s"L2_L1I[${i}]", true) := node
  }

  matrixNodes.zipWithIndex.foreach { case (node, i) =>
    l1xbar := TLLogger(s"L2_Matrix[${i}]", true) := node
  }

  l2xbar :=* l2bankBinders :*= TLLogger("L3_L2", true) :*= l2.node :*= l1xbar

  ram.node :=
    memxbar :=
    TLFragmenter(32, 64) :=
    TLCacheCork() :=
    TLClientsMerger() :=
    TLLogger("MEM_L3", true) :=
    l3xbar :=*
    l3bankBinders :*=
    l3.node :*=
    l2xbar

  lazy val module = new LazyModuleImp(this) {
    val timer = IO(Input(UInt(64.W)))
    val logEnable = IO(Input(Bool()))
    val clean = IO(Input(Bool()))
    val dump = IO(Input(Bool()))

    dontTouch(timer)
    dontTouch(logEnable)
    dontTouch(clean)
    dontTouch(dump)

    cNodes.zipWithIndex.foreach { case (node, i) =>
      node.makeIOs()(ValName(s"master_port_$i"))
    }

    l1iNodes.zipWithIndex.foreach { case (node, i) =>
      node.makeIOs()(ValName(s"master_ul_port_0_${i}"))
    }

    matrixNodes.zipWithIndex.foreach { case (node, i) =>
      node.makeIOs()(ValName(s"master_m_port_0_${i}"))

      require(node.out.size == 1)
      val logm = Module(new utility.TLLoggerM(s"L2_Matrix[${i}]", node.out.head._2, true))
      logm.io.a.valid := node.out.head._1.a.valid
      logm.io.a.bits := node.out.head._1.a.bits
      logm.io.m.valid := l2.module.io.matrixDataOut.get(i).valid
      logm.io.m.bits.source := l2.module.io.matrixDataOut.get(i).bits.sourceId
      logm.io.m.bits.data := l2.module.io.matrixDataOut.get(i).bits.data.asUInt
    }

    l2.module.io.hartId := DontCare
    l2.module.io.l2_tlb_req <> DontCare
    l2.module.io.pfCtrlFromCore := DontCare
    l2.module.io.debugTopDown <> DontCare

    val matrixDataOut = IO(Vec(1, Vec(l2Banks, DecoupledIO(new MatrixDataBundle()))))
    matrixDataOut.head <> l2.module.io.matrixDataOut.get
  }
}

object TestTop_Matrix extends App {
  val l2Banks = TestTopMatrixParams.l2Banks
  val l3Banks = TestTopMatrixParams.l3Banks
  val mNum = l2Banks

  val config = baseConfigMatrix(1, l2Banks, l3Banks, mNum).alterPartial {
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField())
    )
    case HCCacheParamsKey => HCCacheParameters(
      echoField = Seq(DirtyField())
    )
  }

  ChiselDB.init(true)
  Constantin.init(false)

  val top = DisableMonitors(p => LazyModule(new TestTop_L2L3_Matrix()(p)))(config)
  (new ChiselStage).execute(args,
    ChiselGeneratorAnnotation(() => top.module) +: TestTopMatrixFirtoolOptions()
  )

  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write("./build")
}
