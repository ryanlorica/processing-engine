package pe

import chisel3._
import chisel3.util._

class FPMult(dataType: String) extends Module {

  private val bitWidth: Int = dataType match {
    case "FP16" => 16
    case "FP32" => 32
    case "FP64" => 64
    case _ => 15 // BF16
  }

  private class ValExec_MulAddRecF16_mul(dataType: String) extends BlackBox {
    val io = IO(new Bundle {
      val a = Input(SInt(bitWidth.W))
      val b = Input(SInt(bitWidth.W))
      val roundingMode = Input(SInt(3.W))
      val detectTininess = Input(Bool())
      val expected_out = Input(SInt(bitWidth.W))
      val expected_exceptionFlags = Input(SInt(5.W))
      val expected_recOut = Output(SInt((bitWidth + 1).W))
      val actual_out = Output(SInt((bitWidth + 1).W))
      val actual_exceptionFlags = Output(SInt(5.W))
      val check = Output(Bool())
      val pass = Output(Bool())
    })
  }

  val io = IO(new Bundle {
    val in1 = Input(SInt(bitWidth.W))
    val in2 = Input(SInt(bitWidth.W))
    val out = Output(SInt(bitWidth.W))
  })

  private val hfMult = Module(new ValExec_MulAddRecF16_mul(dataType))
  hfMult.io.a := io.in1
  hfMult.io.b := io.in2
  io.out := hfMult.io.actual_out
}

class FPAdd(dataType: String) extends Module {

  private val bitWidth: Int = dataType match {
    case "FP16" => 16
    case "FP32" => 32
    case "FP64" => 64
    case _ => 15
  }

  private class HardfloatAdd(dataType: String) extends BlackBox{
    val io = IO(new Bundle {
      val io_a = Input(SInt(bitWidth.W))
      val io_b = Input(SInt(bitWidth.W))
      val io_roundingMode = Input(SInt(3.W))
      val io_detectTininess = Input(Bool())
      val io_expected_out = Input(SInt(bitWidth.W))
      val io_expected_exceptionFlags = Input(SInt(5.W))
      val io_expected_recOut = Output(SInt((bitWidth + 1).W))
      val io_actual_out = Output(SInt((bitWidth + 1).W))
      val io_actual_exceptionFlags = Output(SInt(5.W))
      val io_check = Output(Bool())
      val io_pass = Output(Bool())
    })
  }

  val io = IO(new Bundle {
    val in1 = Input(SInt(bitWidth.W))
    val in2 = Input(SInt(bitWidth.W))
    val out = Output(SInt(bitWidth.W))
  })

  private val hfAdd = Module(new HardfloatAdd(dataType))
  hfAdd.io.io_a := io.in1
  hfAdd.io.io_b := io.in2
  io.out := hfAdd.io.io_actual_out
}