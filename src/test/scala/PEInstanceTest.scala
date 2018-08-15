import org.scalatest.FunSuite
import pe._
import chisel3._
import chisel3.iotesters.{Driver, PeekPokeTester}

// I think I don't need scalatest here. Oh well.
class PEInstanceTest extends FunSuite {
  test("Check instantiation.") {

    val rsDecodeWeightRF = (state: UInt, c: RFConfig) => {

      val ctrl = Wire(new RFControl(c))

      when (state === 0.U || state === 1.U || state === 2.U) {
        ctrl.external.get.wEnable(0) := true.B
        ctrl.external.get.wAddr.get(0) := state
      } .otherwise {
        ctrl.external.get.wEnable(0) := false.B
        ctrl.external.get.wAddr.get(0) := DontCare
      }

      when (state >= 1.U && state <= 9.U) {
        ctrl.internal.get.rEnable(0) := true.B
        ctrl.internal.get.rAddr.get(0) := (state - 1.U) % 3.U
      } .otherwise {
        ctrl.internal.get.rEnable(0) := false.B
        ctrl.internal.get.rAddr.get(0) := DontCare
      }

      ctrl
    }

    val rsDecodeActvtnRF = (state: UInt, c: RFConfig) => {

      val ctrl = Wire(new RFControl(c))

      when (state <= 8.U) {
        ctrl.external.get.wEnable(0) := true.B
        ctrl.external.get.wAddr.get(0) := state % 3.U
      } .otherwise {
        ctrl.external.get.wEnable(0) := false.B
        ctrl.external.get.wAddr.get(0) := DontCare
      }

      when (state >= 1.U && state <= 9.U) {
        ctrl.internal.get.rEnable(0) := true.B
        ctrl.internal.get.rAddr.get(0) := (state - 1.U) % 3.U
      } .otherwise {
        ctrl.internal.get.rEnable(0) := false.B
        ctrl.internal.get.rAddr.get(0) := DontCare
      }

      ctrl
    }

    val rsDecodeScratchRF = (state: UInt, c: RFConfig) => {

      val ctrl = Wire(new RFControl(c))

      when (state >= 1.U && state <= 9.U) {
        ctrl.internal.get.wEnable(0) := true.B
      } .otherwise {
        ctrl.internal.get.wEnable(0) := false.B
      }

      when (state >= 1.U && state <= 10.U && state =/= 4.U && state =/= 7.U) {
        ctrl.internal.get.rEnable(0) := true.B
      } .otherwise {
        ctrl.internal.get.rEnable(0) := false.B
      }

      when (state === 1.U) {
        ctrl.internal.get.rAddr.get(0) := DontCare
        ctrl.internal.get.wAddr.get(0) := 0.U
      } .elsewhen (state === 2.U) {
        ctrl.internal.get.rAddr.get(0) := 0.U
        ctrl.internal.get.wAddr.get(0) := 1.U
      } .elsewhen (state === 3.U) {
        ctrl.internal.get.rAddr.get(0) := 1.U
        ctrl.internal.get.wAddr.get(0) := 0.U
      } .elsewhen (state === 4.U) {
        ctrl.internal.get.rAddr.get(0) := DontCare
        ctrl.internal.get.wAddr.get(0) := 1.U
      } .elsewhen (state === 5.U) {
        ctrl.internal.get.rAddr.get(0) := 1.U
        ctrl.internal.get.wAddr.get(0) := 2.U
      } .elsewhen (state === 6.U) {
        ctrl.internal.get.rAddr.get(0) := 2.U
        ctrl.internal.get.wAddr.get(0) := 1.U
      } .elsewhen (state === 7.U) {
        ctrl.internal.get.rAddr.get(0) := DontCare
        ctrl.internal.get.wAddr.get(0) := 2.U
      } .elsewhen (state === 8.U) {
        ctrl.internal.get.rAddr.get(0) := 2.U
        ctrl.internal.get.wAddr.get(0) := 3.U
      } .elsewhen (state === 9.U) {
        ctrl.internal.get.rAddr.get(0) := 3.U
        ctrl.internal.get.wAddr.get(0) := 2.U
      } .elsewhen (state === 10.U) {
        ctrl.internal.get.rAddr.get(0) := 2.U
        ctrl.internal.get.wAddr.get(0) := DontCare
      } .otherwise {
        ctrl.internal.get.rAddr.get(0) := DontCare
        ctrl.internal.get.wAddr.get(0) := DontCare
      }

      ctrl
    }

    val rsDecodeIPU = (state: UInt, c: IPUConfig) => {
      val bpSel = Wire(Vec(c.width, Bool()))
      bpSel
    }

    val rsDecodeALU = (state: UInt, c: ALUConfig) => {
      val fSel = Wire(new ALUFSel(c))
      when (state === 1.U || state === 4.U || state === 7.U) {
        fSel.idnEnable.get := true.B
        fSel.accEnable.get := false.B
      } .elsewhen (state > 1.U && state < 10.U) {
        fSel.idnEnable.get := false.B
        fSel.accEnable.get := true.B
      } .otherwise {
        fSel.idnEnable.get := false.B
        fSel.accEnable.get := false.B
      }
      fSel
    }

    val rsDecodeNLU = (state: UInt, c: NLUConfig) => {
      val fSel = Wire(new NLUFSel(c))
      fSel.idEnable.get := true.B
      fSel
    }

    val rsPEConfig = new PEConfig(
      new RFConfig(0, 1, 1, 0, 4, 16, "None"),
      new RFConfig(0, 1, 1, 0, 4, 16, "None"),
      new RFConfig(1, 0, 1, 0, 4, 16, "None"),
      new IPUConfig(1, 16, 16, "None", dataType  = "Int", expWidth = 8, sigWidth = 8),
      new ALUConfig(8, List("Identity", "Accumulate"), dataType = "Int", expWidth = 4, sigWidth = 4),
      new NLUConfig(8, 8, List("Identity")),
      smWidth = 4,
      rsDecodeWeightRF,
      rsDecodeActvtnRF,
      rsDecodeScratchRF,
      rsDecodeIPU,
      rsDecodeALU,
      rsDecodeNLU
    )

    val ret = Driver(() => new PE(rsPEConfig)) {
      uut => new PeekPokeTester(uut) {

      }
    }

    assert(ret)
  }
}
