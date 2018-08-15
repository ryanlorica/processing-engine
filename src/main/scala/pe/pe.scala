package pe

import chisel3._

class PEConfig(
                val weightRFConfig: RFConfig,
                val actvtnRFConfig: RFConfig,
                val scratchRFConfig: RFConfig,
                val ipuConfig: IPUConfig,
                val aluConfig: ALUConfig,
                val nluConfig: NLUConfig,
                val smWidth: Int,
                val decodeWeightRF: (UInt, RFConfig) => Data,
                val decodeActvtnRF: (UInt, RFConfig) => Data,
                val decodeScratchRF: (UInt, RFConfig) => Data,
                val decodeIPU: (UInt, IPUConfig) => Data,
                val decodeALU: (UInt, ALUConfig) => Data,
                val decodeNLU: (UInt, NLUConfig) => Data) {

  require(ipuConfig.width == weightRFConfig.numIntOutputs,
    "IPU input width not equal to Weight RF Internal Output width.\n")
  require(ipuConfig.width == actvtnRFConfig.numIntOutputs,
    "IPU input width not equal to Activation RF Internal Output width.\n")

  if(ipuConfig.bpFirm) {
    require(aluConfig.addSupp || aluConfig.maxSupp,
      "Incompatible ALU and IPU Configurations")
  }
}

class PE(c: PEConfig) extends Module {

  val cw = c.weightRFConfig
  val ca = c.actvtnRFConfig
  val cs = c.scratchRFConfig

  val io = IO(new Bundle {
    val stateCtrl = Input(UInt(c.smWidth.W))
    val toWeightRF = Input(Vec(cw.numExtInputs, SInt(cw.dataWidth.W)))
    val toActvtnRF = Input(Vec(ca.numExtInputs, SInt(ca.dataWidth.W)))
    val toScratchRF = Input(Vec(cs.numExtInputs, SInt(cs.dataWidth.W)))
    val fromWeightRF = Output(Vec(cw.numExtOutputs, SInt(cw.dataWidth.W)))
    val fromActvtnRF = Output(Vec(ca.numExtOutputs, SInt(ca.dataWidth.W)))
    val fromScratchRF = Output(Vec(cs.numExtOutputs, SInt(cs.dataWidth.W)))
    val totalOutput = Output(SInt(c.nluConfig.outBitWidth.W))
  })

  val decoder = Module(new Decoder(c))
  decoder.io.state := io.stateCtrl

  val weightRF = Module(new RF(cw))
  weightRF.io.control <> decoder.io.mem.weightRF
  weightRF.io.wExternal := io.toWeightRF
  io.fromWeightRF := weightRF.io.rExternal

  val actvtnRF = Module(new RF(ca))
  actvtnRF.io.control <> decoder.io.mem.actvtnRF
  actvtnRF.io.wExternal := io.toActvtnRF
  io.fromActvtnRF := actvtnRF.io.rExternal

  val ipu = Module(new IPU(c.ipuConfig))
  if (ipu.io.bpSel.isDefined) {
    ipu.io.bpSel.get := decoder.io.proc.ipuBpSel.get
  }
  ipu.io.weightIn := weightRF.io.rInternal
  ipu.io.actvtnIn := actvtnRF.io.rInternal

  val alu = Module(new ALU(c.aluConfig))
  alu.io.fSel <> decoder.io.proc.aluFSel
  alu.io.ipu <> ipu.io.out

  val scratchRF = Module(new RF(cs))
  scratchRF.io.control <> decoder.io.mem.scratchRF
  scratchRF.io.wExternal := io.toScratchRF
  scratchRF.io.wInternal(0) := alu.io.out // TODO: Add Req. for this
  io.fromScratchRF := scratchRF.io.rExternal
  if(alu.io.rf.isDefined) {
    alu.io.rf.get := scratchRF.io.rInternal(0) // TODO: Add Req. for this
  }

  val nlu = Module(new NLU(c.nluConfig))
  nlu.io.fSel <> decoder.io.proc.nluFSel
  nlu.io.in := scratchRF.io.rInternal(0) // TODO: Add Req. for this

  io.totalOutput := nlu.io.out
}
