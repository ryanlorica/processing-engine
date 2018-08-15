package pe

import chisel3._

class MemoryControl(c: PEConfig) extends Bundle {

  override def cloneType = (new MemoryControl(c)).asInstanceOf[this.type]

  val weightRF = new RFControl(c.weightRFConfig)
  val actvtnRF = new RFControl(c.actvtnRFConfig)
  val scratchRF = new RFControl(c.scratchRFConfig)
}

class ProcessControl(c: PEConfig) extends Bundle {

  override def cloneType = (new ProcessControl(c)).asInstanceOf[this.type]

  val aluFSel = Output(new ALUFSel(c.aluConfig))
  val nluFSel = Output(new NLUFSel(c.nluConfig))

  val ipuBpSel = if (c.ipuConfig.bpFirm) Some(Output(Vec(c.ipuConfig.width, Bool()))) else None
}

class Decoder(c: PEConfig) extends Module {

  val io = IO(new Bundle {
    val state = Input(UInt(c.smWidth.W))
    val mem = Output(new MemoryControl(c))
    val proc = Output(new ProcessControl(c))
  })

  io.mem.weightRF <> c.decodeWeightRF(io.state, c.weightRFConfig)
  io.mem.actvtnRF <> c.decodeActvtnRF(io.state, c.actvtnRFConfig)
  io.mem.scratchRF <> c.decodeScratchRF(io.state, c.scratchRFConfig)

  if (c.ipuConfig.bpFirm) {
    io.proc.ipuBpSel.get := c.decodeIPU(io.state, c.ipuConfig)
  }

  io.proc.aluFSel <> c.decodeALU(io.state, c.aluConfig)
  io.proc.nluFSel <> c.decodeNLU(io.state, c.nluConfig)
}