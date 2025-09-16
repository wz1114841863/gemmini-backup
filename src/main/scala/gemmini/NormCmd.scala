package gemmini

import chisel3._
import chisel3.util._

object NormCmd extends ChiselEnum {
    val RESET, SUM, MEAN, VARIANCE, INV_STDDEV, MAX, SUM_EXP, INV_SUM_EXP = Value

    // 是否会写主存
    def writes_to_main_memory(cmd: Type): Bool = {
        cmd === RESET
    }

    // 把衍生命令映射回原生命令
    def non_reset_version(cmd: Type): Type = {
        MuxCase(
            cmd,
            Seq(
                (cmd === MEAN) -> SUM,
                (cmd === MAX) -> MAX,
                (cmd === INV_STDDEV) -> VARIANCE,
                (cmd === INV_SUM_EXP) -> SUM_EXP
            )
        )
    }
}
