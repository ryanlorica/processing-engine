import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

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

class IPUConfig(val width: Int, val inBitWidth: Int, val outBitWidth: Int, val bpType: String) {

    require(width >= 1, "Width must be at least one.\n")
    require(List("None", "Firm").contains(bpType), "Bypass must be \"None\" or \"Firm\".\n")
    require(inBitWidth > 0 && outBitWidth > 0, "Data bitwidth must be greater than 0\n")

    val bpFirm = (bpType == "Firm")
}

class IPUOutput(outBitWidth: Int, bp: Boolean) extends Bundle {

    override def cloneType = (new IPUOutput(outBitWidth, bp)).asInstanceOf[this.type]

    val innerProd = SInt(outBitWidth.W)
    // Extending the bitwidths for consistency
    val bpWeight = if (bp) Some(SInt(outBitWidth.W)) else None
    val bpActvtn = if (bp) Some(SInt(outBitWidth.W)) else None
}


class IPU(c: IPUConfig) extends Module {

    val io = IO(new Bundle {
        val bpSel = if (c.bpFirm) Some(Input(Vec(c.width, Bool()))) else None
        val weightIn = Input(Vec(c.width, SInt(c.inBitWidth.W)))
        val actvtnIn = Input(Vec(c.width, SInt(c.inBitWidth.W)))
        val out = Output(new IPUOutput(c.outBitWidth, c.bpFirm))
    })

    private class PMult extends Module {
        val io = IO(new Bundle {
            val weightVec = Input(Vec(c.width, SInt(c.inBitWidth.W)))
            val actvtnVec = Input(Vec(c.width, SInt(c.inBitWidth.W)))
            val pairwiseProd = Output(Vec(c.width, SInt(c.outBitWidth.W)))
        })
        io.pairwiseProd := (io.weightVec zip io.actvtnVec).map { case(a, b) => a * b }
    }

    private class SumTree extends Module {
        val io = IO(new Bundle {
            val inVec = Input(Vec(c.width, SInt(c.outBitWidth.W)))
            val sum = Output(SInt(c.outBitWidth.W))
        })

        // Recursively creates a balanced syntax tree
        private def adjReduce[A](xs: List[A], op: (A, A) => A): A = xs match {
            case List(single) => single
            case default => {
                val grouped = default.grouped(2).toList
                val result = for (g <- grouped) yield { g match {
                    case List(a, b) => op(a, b)
                    case List(x) => x
                }}
                adjReduce(result, op)
            }
        }

        io.sum := adjReduce(io.inVec.toList, (x: SInt, y: SInt) => x + y)
    }

    private val pMult = Module(new PMult)
    pMult.io.weightVec := io.weightIn
    pMult.io.actvtnVec := io.actvtnIn

    private val sumTree = Module(new SumTree)
    sumTree.io.inVec := pMult.io.pairwiseProd

    io.out.innerProd := sumTree.io.sum

    if (c.bpFirm) {
    io.out.bpWeight.get := PriorityMux(io.bpSel.get, io.weightIn)
    io.out.bpActvtn.get := PriorityMux(io.bpSel.get, io.actvtnIn)
    }
}

class ALUConfig(val dataWidth: Int, val funcs: List[String]) {

    require(funcs.length > 0, "Must support at least one function.")
    for(x <- funcs) {
        require(List("Identity", "Add", "Max", "Accumulate").contains(x), "Unsupported function.")
    }

    val idnSupp = funcs.contains("Identity")
    val addSupp = funcs.contains("Add")
    val maxSupp = funcs.contains("Max")
    val accSupp = funcs.contains("Accumulate")
    val addBypassIn = addSupp || maxSupp
}

class ALUFSel(c: ALUConfig) extends Bundle {

    override def cloneType = (new ALUFSel(c)).asInstanceOf[this.type]

    // Priority is given from top to bottom
    val idnEnable = if (c.idnSupp) Some(Bool()) else None
    val addEnable = if (c.addSupp) Some(Bool()) else None
    val maxEnable = if (c.maxSupp) Some(Bool()) else None
    val accEnable = if (c.accSupp) Some(Bool()) else None
}

class ALU(c: ALUConfig) extends Module {

    val io = IO(new Bundle {
        val fSel = Input(new ALUFSel(c))
        val ipu = Input(new IPUOutput(c.dataWidth, c.addBypassIn))
        val rf = if (c.accSupp) Some(Input(SInt(c.dataWidth.W))) else None
        val out = Output(SInt(c.dataWidth.W))
    })

    // The inner "OrElse" clauses are logically unnecessary,
    // but Chisel can't infer that.
    when (io.fSel.idnEnable.getOrElse(false.B)) {
        io.out := io.ipu.innerProd
    } .elsewhen (io.fSel.addEnable.getOrElse(false.B)) {
        io.out := io.ipu.bpWeight.getOrElse(0.S) + io.ipu.bpActvtn.getOrElse(0.S)
    } .elsewhen (io.fSel.maxEnable.getOrElse(false.B)) {
        when (io.ipu.bpWeight.getOrElse(0.S) > io.ipu.bpActvtn.getOrElse(0.S)) {
            io.out := io.ipu.bpWeight.getOrElse(0.S)
        } .otherwise {
            io.out := io.ipu.bpActvtn.getOrElse(0.S)
        }
    } .elsewhen (io.fSel.accEnable.getOrElse(false.B)) {
        io.out := io.ipu.innerProd + io.rf.getOrElse(0.S)
    } .otherwise {
        io.out := 0.S
    }
}

class NLUConfig(val inBitWidth: Int, val outBitWidth: Int, val funcs: List[String]) {

    for(x <- funcs) {
        require(List("Identity", "ReLu").contains(x), "Unsupported Function")
    }

    val idSupp = funcs.contains("Identity")
    val reluSupp = funcs.contains("ReLu")
    val tanhSupp = false //funcs.contains("tanh")
    val sinhSupp = false //funcs.contains("sinh")
}

class NLUFSel(c: NLUConfig) extends Bundle {

    override def cloneType = (new NLUFSel(c)).asInstanceOf[this.type]

    val idEnable = if (c.idSupp) Some(Bool()) else None
    val reluEnable = if (c.reluSupp) Some(Bool()) else None
    val tanhEnable = if (c.tanhSupp) Some(Bool()) else None
    val sinhEnable = if (c.sinhSupp) Some(Bool()) else None
}

class NLU(c: NLUConfig) extends Module {

    val io = IO(new Bundle {
        val fSel = Input(new NLUFSel(c))
        val in = Input(SInt(c.inBitWidth.W))
        val out = Output(SInt(c.outBitWidth.W))
    })

    when (io.fSel.idEnable.getOrElse(false.B)) {
        io.out := io.in
    } .elsewhen (io.fSel.reluEnable.getOrElse(false.B)) {
        when (io.in.data > 0.S) {
            io.out := io.in.data
        } .otherwise {
            io.out := 0.S
        }
    } .elsewhen (io.fSel.tanhEnable.getOrElse(false.B)) {
        // TODO
        io.out := 0.S
    } .elsewhen (io.fSel.sinhEnable.getOrElse(false.B)) {
        // TODO
        io.out := 0.S
    } .otherwise {
        io.out := 0.S
    }
}

class StateMachineConfig(
        val numStates: Int,
        val numCtrlSigs: Int,
        val stateMap: (UInt, UInt, StateMachineConfig) => UInt) {

    val stateWidth = log2Up(numStates)
    val ctrlWidth = log2Up(numCtrlSigs)
}

class StateMachine(c: StateMachineConfig) extends Module {

    val io = IO(new Bundle {
        val control = Input(UInt(c.ctrlWidth.W))
        val out = Output(UInt(c.stateWidth.W))
    })

    val register = RegInit(0.U(c.stateWidth.W))
    register := c.stateMap(register, io.control, c)

    io.out := register
}

class PEConfig(
        val weightRFConfig: RFConfig,
        val actvtnRFConfig: RFConfig,
        val scratchRFConfig: RFConfig,
        val ipuConfig: IPUConfig,
        val aluConfig: ALUConfig,
        val nluConfig: NLUConfig,
        val smConfig: StateMachineConfig,
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
        val state = Input(UInt(c.smConfig.stateWidth.W))
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

class PE(c: PEConfig) extends Module {

    val cw = c.weightRFConfig
    val ca = c.actvtnRFConfig
    val cs = c.scratchRFConfig

    val io = IO(new Bundle {
        val stateCtrl = Input(UInt(c.smConfig.ctrlWidth.W))
        val toWeightRF = Input(Vec(cw.numExtInputs, SInt(cw.dataWidth.W)))
        val toActvtnRF = Input(Vec(ca.numExtInputs, SInt(ca.dataWidth.W)))
        val toScratchRF = Input(Vec(cs.numExtInputs, SInt(cs.dataWidth.W)))
        val fromWeightRF = Output(Vec(cw.numExtOutputs, SInt(cw.dataWidth.W)))
        val fromActvtnRF = Output(Vec(ca.numExtOutputs, SInt(ca.dataWidth.W)))
        val fromScratchRF = Output(Vec(cs.numExtOutputs, SInt(cs.dataWidth.W)))
        val totalOutput = Output(SInt(c.nluConfig.outBitWidth.W))
    })

    val stateMachine = Module(new StateMachine(c.smConfig))
    stateMachine.io.control := io.stateCtrl

    val decoder = Module(new Decoder(c))
    decoder.io.state := stateMachine.io.out

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
