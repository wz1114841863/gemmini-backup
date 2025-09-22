package gemmini

import chisel3._
import chisel3.util._
import Util._

/*
    带权重的仲裁器, 用于在两个输入流之间进行选择, 以决定哪个输入流的数据被传输到输出.
    通过权重参数, 可以控制每个输入流被选择的频率.
    1) 权重仲裁: inA 可以设置一个权重值 weightA,inA 会连续被选中 weightA 次,然后切换一次到 inB.
    2) 强制优先级: 通过 forceA 或 forceB 强制选择某一通道.
    3) 静态权重模式: 当 weightA === 0 且 staticWeightAEnabled === true 时,启用静态策略,基于 k, i, j 等信号进行智能调度.
    4) 防止饥饿: 通过计数器 count 保证 inB 至少能每 weightA + 1 个周期获得一次机会.
 */
class WeightedArbiter[T <: Data](t: T, maxWeightA: Int, staticWeightAEnabled: Boolean) extends Module {
    val io = IO(new Bundle {
        val inA = Flipped(Decoupled(t))
        val inB = Flipped(Decoupled(t))
        val weightA = Input(UInt(log2Up(maxWeightA + 1).W)) // 设置A的权重
        val forceA = Input(Bool()) // 强制选择 A 或 B
        val forceB = Input(Bool())
        val out = Decoupled(t)

        val inA_idle = Input(Bool()) // 表示 A/B 是否空闲
        val inB_idle = Input(Bool())
        val inA_k = Input(UInt(16.W)) // TODO magic number
        val inB_k = Input(UInt(16.W)) // TODO magic number
        val inA_i = Input(UInt(16.W)) // TODO magic number
        val inB_j = Input(UInt(16.W)) // TODO magic number
    })

    val count = Reg(UInt(log2Up(maxWeightA + 1).W))
    val A_chosen = WireInit(false.B)
    val B_chosen = WireInit(false.B)

    val staticWeightA = io.weightA === 0.U && staticWeightAEnabled.B
    val weightA = if (staticWeightAEnabled) { io.weightA }
    else { Mux(io.weightA === 0.U, 3.U, io.weightA) }

    io.inA.ready := false.B
    io.inB.ready := false.B

    when(io.forceA) {
        io.out <> io.inA
    }.elsewhen(io.forceB) {
        io.out <> io.inB
    }.elsewhen(!staticWeightA) {
        when(io.inA.valid && io.inB.valid) {
            when(count < weightA) {
                io.out <> io.inA
                A_chosen := true.B
            }.otherwise {
                io.out <> io.inB
                B_chosen := true.B
            }
        }.elsewhen(io.inA.valid) {
            io.out <> io.inA
        }.otherwise {
            io.out <> io.inB
        }
    }.otherwise {
        when(io.inA_idle) {
            io.out <> io.inB
        }.elsewhen(io.inB_idle) {
            io.out <> io.inA
        }.elsewhen(io.inA_k > io.inB_k || (io.inB_k === 0.U && io.inB_j === 0.U)) {
            io.out <> io.inB
        }.otherwise {
            io.out <> io.inA
        }
    }

    when(io.out.fire) {
        when(A_chosen) {
            count := satAdd(count, 1.U, weightA + 1.U)
        }.elsewhen(B_chosen) {
            count := 0.U
        }
    }

    assert(!(io.forceA && io.forceB))
    assert(!(A_chosen && B_chosen))
    assert((!io.inA.valid && !io.inB.valid) || (weightA > 0.U || staticWeightAEnabled.B))
}
