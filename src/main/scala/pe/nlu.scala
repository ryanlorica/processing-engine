package pe

import chisel3._
import chisel3.util._

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
