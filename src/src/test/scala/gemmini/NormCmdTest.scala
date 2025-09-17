package gemmini

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers


// 用于测试 NormCmd 对象的方法, 通过一个简单的 Module 包装
class NormCmdTop extends Module {
    val io = IO(new Bundle {
        val cmd_in = Input(NormCmd.Type())
        val writes_to_main_memory = Output(Bool())
        val non_reset_version = Output(UInt(3.W))
    })

    io.writes_to_main_memory := NormCmd.writes_to_main_memory(io.cmd_in)
    io.non_reset_version := NormCmd.non_reset_version(io.cmd_in).asUInt
}

class NormCmdTest extends AnyFreeSpec with Matchers {
    "NormCmdTop" in {
        // -------------- 生成 Verilog --------------
        // val verilog = ChiselStage.emitVerilog(new NormCmdTop)
        println(getVerilogString(new NormCmdTop))

        simulate(new NormCmdTop) { dut =>
            def test_case(cmd: NormCmd.Type, expected_writes: Boolean, expected_non_reset: NormCmd.Type): Unit = {
                dut.io.cmd_in.poke(cmd)
                dut.clock.step(1)
                dut.io.writes_to_main_memory.expect(expected_writes.B)
                dut.io.non_reset_version.expect(expected_non_reset.litValue)
            }

            // Test cases
            test_case(NormCmd.RESET, true, NormCmd.RESET)
            test_case(NormCmd.SUM, false, NormCmd.SUM)
            test_case(NormCmd.MEAN, false, NormCmd.SUM)
            test_case(NormCmd.VARIANCE, false, NormCmd.VARIANCE)
            test_case(NormCmd.INV_STDDEV, false, NormCmd.VARIANCE)
            test_case(NormCmd.MAX, false, NormCmd.MAX)
            test_case(NormCmd.SUM_EXP, false, NormCmd.SUM_EXP)
            test_case(NormCmd.INV_SUM_EXP, false, NormCmd.SUM_EXP)
        }
    }
}
