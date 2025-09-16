package gemmini

import chisel3._
import chisel3.util._

// 带反压控制的/可配置延迟的通用流水线寄存器
class Pipeline[T <: Data](gen: T, latency: Int)(comb: Seq[T => T] = Seq.fill(latency + 1)((x: T) => x)) extends Module {
    val io = IO(new Bundle {
        val in = Flipped(Decoupled(gen)) // 输入, 带反压
        val out = Decoupled(gen) // 输出, 带反压
        val busy = Output(Bool()) // 当流水线中有数据时为真
    })

    // comb是一个长度为latency + 1的函数序列, 每个函数的类型为T => T
    // comb的第i个函数表示在流水线的第i个阶段对数据进行的组合逻辑处理
    require(comb.size == latency + 1, "length of combinational is incorrect")

    if (latency == 0) {
        // 延迟为0时, 流水线退化为组合逻辑
        io.in.ready := io.out.ready
        io.out.valid := io.in.valid
        io.out.bits := comb.head(io.in.bits)
        io.busy := io.in.valid
    } else {
        val stages = Reg(Vec(latency, gen)) // 每级寄存器
        val valids = RegInit(VecInit(Seq.fill(latency)(false.B))) // 每级有效位
        val stalling = VecInit(Seq.fill(latency)(false.B)) // 每级是否停顿
        io.busy := io.in.valid || valids.reduce(_ || _) // 只要输入有效,或流水线中任意一级有数据,就认为流水线忙碌

        // Stall signals
        // 如果某一级 valid 为真且下游阻塞,则本级必须阻塞.
        // 阻塞信号从后向前传播,形成 流水线气泡(bubble).
        io.in.ready := !stalling.head
        stalling.last := valids.last && !io.out.ready
        (stalling.init, stalling.tail, valids.init).zipped.foreach { case (s1, s2, v1) =>
            s1 := v1 && s2
        }

        // Valid signals
        // When the pipeline stage ahead of you isn't stalling, then make yourself invalid
        // 每一级的 valid 位表示该级是否有数据.
        io.out.valid := valids.last
        when(io.out.ready) {
            valids.last := false.B
        }
        // 当某级数据被下游接收, 则该级 valid 置位
        (valids.init, stalling.tail).zipped.foreach { case (v1, s2) =>
            when(!s2) {
                v1 := false.B
            }
        }

        // When the pipeline stage behind you is valid then become true
        // 当某级数据被上游送入, 则该级 valid 置位
        when(io.in.fire) {
            valids.head := true.B
        }

        (valids.tail, valids.init).zipped.foreach { case (v2, v1) =>
            when(v1) {
                v2 := true.B
            }
        }

        // Stages
        // 数据从输入经过comb.head处理后送入第一级寄存器
        // 每一级寄存器的数据经过对应的comb处理后送入下一级
        // 如果下游未阻塞(!s2),则数据向前传递.
        when(io.in.fire) {
            stages.head := comb.head(io.in.bits)
        }
        io.out.bits := comb.last(stages.last)

        ((stages.tail zip stages.init) zip (stalling.tail zip comb.tail.init)).foreach { case ((st2, st1), (s2, c1)) =>
            when(!s2) {
                st2 := c1(st1)
            }
        }
    }
}

object Pipeline {
    // 使用指定的comb函数序列
    def apply[T <: Data](in: ReadyValidIO[T], latency: Int, comb: Seq[T => T]): DecoupledIO[T] = {
        val p = Module(new Pipeline(in.bits.cloneType, latency)(comb))
        p.io.in <> in
        p.io.out
    }

    // 使用恒等函数作为comb
    def apply[T <: Data](in: ReadyValidIO[T], latency: Int): DecoupledIO[T] = {
        val p = Module(new Pipeline(in.bits.cloneType, latency)())
        p.io.in <> in
        p.io.out
    }
}
