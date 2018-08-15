package pe

import chisel3._

// TODO: Replace strings with enums lol
class ALUConfig(val dataWidth: Int, val funcs: List[String], dataType: String, expWidth: Int = 0, sigWidth: Int = 0) {

  require(dataWidth == expWidth + sigWidth, "Datawidth must be the sum of Exponent Width and Signal Width. I'm keeping datawidth there just for backwards compat.\n")
  require(funcs.nonEmpty, "Must support at least one function.\n")
  for(x <- funcs) {
    require(List("Identity", "Add", "Max", "Accumulate").contains(x), "Unsupported function.\n")
  }

  val idnSupp: Boolean = funcs.contains("Identity")
  val addSupp: Boolean = funcs.contains("Add")
  val maxSupp: Boolean = funcs.contains("Max")
  val accSupp: Boolean = funcs.contains("Accumulate")
  val addBypassIn: Boolean = addSupp || maxSupp
}

class ALUFSel(c: ALUConfig) extends Bundle {

  override def cloneType = new ALUFSel(c).asInstanceOf[this.type]

  // Priority is given from top to bottom
  val idnEnable: Option[Bool] = if (c.idnSupp) Some(Bool()) else None
  val addEnable: Option[Bool] = if (c.addSupp) Some(Bool()) else None
  val maxEnable: Option[Bool] = if (c.maxSupp) Some(Bool()) else None
  val accEnable: Option[Bool] = if (c.accSupp) Some(Bool()) else None
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
