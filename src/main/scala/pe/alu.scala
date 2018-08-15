package pe

import chisel3._

class ALUConfig(val dataWidth: Int, val funcs: List[String]) {

  require(funcs.nonEmpty, "Must support at least one function.")
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
