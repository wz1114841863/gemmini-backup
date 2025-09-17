package gemmini

import chisel3._
import chisel3.util._

// extends ChiselEnum, 把枚举值变成"可综合成硬件比较树"的类型.
object NormCmd extends ChiselEnum {
    val RESET, SUM, MEAN, VARIANCE, INV_STDDEV, MAX, SUM_EXP, INV_SUM_EXP = Value

    // 是否会写主存, Type 保证运行时对应一个最小位宽的UInt
    def writes_to_main_memory(cmd: Type): Bool = {
        cmd === RESET
    }

    // 把衍生命令映射回原生命令
    def non_reset_version(cmd: Type): Type = {
        MuxCase(  // 综合为优先级多路选择树
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
