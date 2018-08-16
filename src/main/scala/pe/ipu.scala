package pe

import chisel3._
import chisel3.util.{HasBlackBoxResource, PriorityMux}
import chisel3.experimental._

class IPUConfig(val width: Int, val inBitWidth: Int, val outBitWidth: Int, val bpType: String, val dataType: String) {

  private val binPow = List(1, 2, 4, 8, 16, 32, 64)

  require(binPow.contains(inBitWidth) && binPow.contains(outBitWidth))
  require(width >= 1, "Width must be at least one.\n")
  require(List("None", "Firm").contains(bpType), "Bypass must be \"None\" or \"Firm\".\n")
  require(inBitWidth > 0 && outBitWidth > 0, "Data bitwidth must be greater than 0\n")

  val bpFirm: Boolean = bpType == "Firm"
}

class IPUOutput(outBitWidth: Int, bp: Boolean) extends Bundle {

  override def cloneType = new IPUOutput(outBitWidth, bp).asInstanceOf[this.type]

  val innerProd = SInt(outBitWidth.W)
  // Extending the bitwidths for consistency
  val bpWeight = if (bp) Some(SInt(outBitWidth.W)) else None
  val bpActvtn = if (bp) Some(SInt(outBitWidth.W)) else None
}


//noinspection TypeAnnotation
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
    if (c.dataType == "Int") {
      io.pairwiseProd := VecInit((io.weightVec zip io.actvtnVec).map { case (a, b) => a * b })
    } else {
      io.pairwiseProd := VecInit((io.weightVec zip io.actvtnVec).map {
        case (a, b) =>
          val fpMult = Module(new FPMult(c.dataType))
          fpMult.io.in1 := a
          fpMult.io.in2 := b
          val out = Wire(SInt(c.outBitWidth.W))
          out := fpMult.io.out
          out
      })
    }
  }

  private class SumTree extends Module {
    val io = IO(new Bundle {
      val inVec = Input(Vec(c.width, SInt(c.outBitWidth.W)))
      val sum = Output(SInt(c.outBitWidth.W))
    })

    // Recursively creates a balanced syntax tree
    private def adjReduce[A](xs: List[A], op: (A, A) => A): A = xs match {
      case List(single) => single
      case default =>
        val grouped = default.grouped(2).toList
        val result = for (g <- grouped) yield { g match {
          case List(a, b) => op(a, b)
          case List(x) => x
        }}
        adjReduce(result, op)
    }

    private val fpAdd = (a: SInt, b: SInt) => {
      val ret = Wire(SInt(c.outBitWidth.W))
      val fpAddMod = chisel3.core.Module(new FPAdd(c.dataType))
      fpAddMod.io.in1 := a
      fpAddMod.io.in2 := b
      ret := fpAddMod.io.out
      ret
    }

    if (c.dataType == "Int") {
      io.sum := adjReduce(io.inVec.toList, (x: SInt, y: SInt) => x + y)
    } else {
      io.sum := adjReduce(io.inVec.toList, fpAdd)
    }
  }

  private val pMult = chisel3.core.Module(new PMult)
  pMult.io.weightVec := io.weightIn
  pMult.io.actvtnVec := io.actvtnIn

  private val sumTree = chisel3.core.Module(new SumTree)
  sumTree.io.inVec := pMult.io.pairwiseProd

  io.out.innerProd := sumTree.io.sum

  if (c.bpFirm) {
    io.out.bpWeight.get := PriorityMux(io.bpSel.get, io.weightIn)
    io.out.bpActvtn.get := PriorityMux(io.bpSel.get, io.actvtnIn)
  }
}
