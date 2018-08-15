package pe

import chisel3.util.PriorityMux
import chisel3._

import scala.math.pow

class PartialRFConfig(
                       val numInputs: Int,
                       val numOutputs: Int,
                       val numCrossInputs: Int,
                       val addrWidth: Int,
                       val bpSoft: Boolean,
                       val bpFirm: Boolean)

class PartialRFControl(c: PartialRFConfig) extends Bundle {
  val wEnable = Vec(c.numInputs, Bool())
  val rEnable = Vec(c.numOutputs, Bool())
  val wAddr = if (!c.bpFirm) Some(Vec(c.numInputs, UInt(c.addrWidth.W))) else None
  val rAddr = if (!c.bpFirm) Some(Vec(c.numOutputs, UInt(c.addrWidth.W))) else None
  // Each output can select which input of the opposite bus to bypass from
  val bpSel = if (c.bpSoft || c.bpFirm) Some(Vec(c.numOutputs, Vec(c.numCrossInputs, Bool()))) else None
}

class RFConfig(
                val numIntInputs: Int,
                val numExtInputs: Int,
                val numIntOutputs: Int,
                val numExtOutputs: Int,
                val addrWidth: Int,
                val dataWidth: Int,
                val bpType: String) {

  val bpNone = (bpType == "None")
  val bpSoft = (bpType == "Soft")
  val bpFirm = (bpType == "Firm")

  require(bpNone || bpSoft || bpFirm, "Invalid Bypass type.\n")
  require(numIntInputs > 0 || numExtInputs > 0, "Must have at least one input.\n")
  require(numIntOutputs > 0 || numExtOutputs > 0, "Must have at least one output.\n")
  require(dataWidth > 0, "Data bitwidth must be at least one.\n")
  if (bpFirm) { require(addrWidth == 0, "Address width must be 0 when Firm Bypassing.\n") }

  val intConfig = new PartialRFConfig(
    numIntInputs, numIntOutputs, numExtOutputs, addrWidth, bpSoft, bpFirm)

  val extConfig = new PartialRFConfig(
    numExtInputs, numExtOutputs, numIntOutputs, addrWidth, bpSoft, bpFirm)
}

class RFControl(c: RFConfig) extends Bundle {

  override def cloneType = (new RFControl(c)).asInstanceOf[this.type]

  val internal = if (c.numIntInputs > 0 || c.numIntOutputs > 0)
    Some(new PartialRFControl(c.intConfig)) else None
  val external = if (c.numExtInputs > 0 || c.numExtOutputs > 0)
    Some(new PartialRFControl(c.extConfig)) else None
}

class RF(c: RFConfig) extends Module {

  val io = IO(new Bundle {
    val control = Input(new RFControl(c))
    val wInternal = Input(Vec(c.numIntInputs, SInt(c.dataWidth.W)))
    val wExternal = Input(Vec(c.numExtInputs, SInt(c.dataWidth.W)))
    val rInternal = Output(Vec(c.numIntOutputs, SInt(c.dataWidth.W)))
    val rExternal = Output(Vec(c.numExtOutputs, SInt(c.dataWidth.W)))
  })

  val dataRegister = if (!c.bpFirm)
    Some(RegInit(Vec.fill(pow(2, c.addrWidth).toInt){0.S(c.dataWidth.W)})) else None

  // Need to bypass through a register to prevent combinational loops
  val bpAny = c.bpSoft || c.bpFirm
  val bpRegisterInt = if (bpAny && c.numIntInputs > 0)
    Some(RegInit(Vec.fill(c.numIntInputs){0.S(c.dataWidth.W)})) else None
  val bpRegisterExt = if (bpAny && c.numExtInputs > 0)
    Some(RegInit(Vec.fill(c.numExtInputs){0.S(c.dataWidth.W)})) else None

  for (i <- 0 until c.numIntInputs) {
    when (io.control.internal.get.wEnable(i)) {
      if (!c.bpFirm) { dataRegister.get(io.control.internal.get.wAddr.get(i)) := io.wInternal(i) }
      if (bpRegisterInt.isDefined) { bpRegisterInt.get(i) := io.wInternal(i) }
    }
  }

  for (i <- 0 until c.numExtInputs) {
    when (io.control.external.get.wEnable(i)) {
      if (!c.bpFirm) { dataRegister.get(io.control.external.get.wAddr.get(i)) := io.wExternal(i) }
      if (bpRegisterExt.isDefined) { bpRegisterExt.get(i) := io.wExternal(i) }
    }
  }

  for (i <- 0 until c.numIntOutputs) {
    when (io.control.internal.get.rEnable(i)) {
      if (c.bpFirm) {
        io.rInternal(i) := PriorityMux(io.control.internal.get.bpSel.get(i), bpRegisterExt.get)
      } else if (c.bpSoft) {
        when (io.control.internal.get.bpSel.get(i).contains(true.B)) {
          // External write bypasses to Internal read
          io.rInternal(i) := PriorityMux(io.control.internal.get.bpSel.get(i), bpRegisterExt.get)
        } .otherwise {
          io.rInternal(i) := dataRegister.get(io.control.internal.get.rAddr.get(i))
        }
      } else {
        io.rInternal(i) := dataRegister.get(io.control.internal.get.rAddr.get(i))
      }
    } .otherwise {
      io.rInternal(i) := 0.S
    }
  }

  for (i <- 0 until c.numExtOutputs) {
    when (io.control.external.get.rEnable(i)) {
      if (c.bpFirm) {
        io.rExternal(i) := PriorityMux(io.control.external.get.bpSel.get(i), bpRegisterInt.get)
      } else if (c.bpSoft) {
        when (io.control.external.get.bpSel.get(i).contains(true.B)) {
          // Internal write bypasses to External read
          io.rExternal(i) := PriorityMux(io.control.external.get.bpSel.get(i), bpRegisterInt.get)
        } .otherwise {
          io.rExternal(i) := dataRegister.get(io.control.external.get.rAddr.get(i))
        }
      } else {
        io.rExternal(i) := dataRegister.get(io.control.external.get.rAddr.get(i))
      }
    } .otherwise {
      io.rExternal(i) := 0.S
    }
  }
}
