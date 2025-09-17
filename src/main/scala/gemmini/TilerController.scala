//===========================================================================
// TilerController Implementation
//===========================================================================
package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._

/*
    流程:
        cmd_in → FSM(拆块) → Scheduler(排期) → 三条 issue 流
        三条 completed 流 → Arbiter → Scheduler(释放 ROB 槽位)
*/

/*
    矩阵乘描述:
        m x n x k, 4个地址, 移位, 激活函数, 重复偏置
        m/n/k:矩阵规模
        addr_a/b/c/d:A/B/C/D 四个阵列的 DRAM 地址
        三组移位:in_rshift / acc_rshift / relu6_lshift
        activation:00=none, 01=ReLU, 10=ReLU6, 11=预留
        repeating_bias:C 矩阵是否做广播 bias
        status:Rocket core 的 MStatus,用于异常溯源
*/
class TilerCmd(OTYPE_BITS_IDX: Int)(implicit p: Parameters) extends CoreBundle {
    val m = UInt(32.W)
    val n = UInt(32.W)
    val k = UInt(32.W)
    val addr_a = UInt(xLen.W)
    val addr_b = UInt(xLen.W)
    val addr_c = UInt(xLen.W)
    val addr_d = UInt(xLen.W)
    val in_rshift = UInt(OTYPE_BITS_IDX.W)
    val acc_rshift = UInt(OTYPE_BITS_IDX.W)
    val relu6_lshift = UInt(OTYPE_BITS_IDX.W)
    val activation = UInt(2.W)
    val repeating_bias = Bool()
    val status = new MStatus
}

class TilerController[T <: Data: Arithmetic, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V])(implicit
    val p: Parameters
) extends Module with HasCoreParameters {
    import config._

    // =========================================================================
    // Interface
    // =========================================================================
    val io = IO(new Bundle {
        val cmd_in = Flipped(Decoupled(new TilerCmd(LOG2_OTYPE_BITS)))
        val issue = new Bundle {
            val exec = Decoupled(new GemminiCmd(ROB_ENTRIES))
            val load = Decoupled(new GemminiCmd(ROB_ENTRIES))
            val store = Decoupled(new GemminiCmd(ROB_ENTRIES))
        }
        val completed = new Bundle {
            val exec = Flipped(Valid(UInt(LOG2_ROB_ENTRIES.W)))
            val load = Flipped(Decoupled(UInt(LOG2_ROB_ENTRIES.W)))
            val store = Flipped(Decoupled(UInt(LOG2_ROB_ENTRIES.W)))
        }
        val busy = Output(Bool())
    })

    // =========================================================================
    // dispatch incoming commands
    // =========================================================================
    // 把一次矩阵乘拆成多次小矩阵乘
    val fsm = TilerFSM(config)
    fsm.io.cmd_in <> io.cmd_in

    // 拿到tile描述后, 生成真正的load, exec等指令
    val sched = TilerScheduler(config)
    sched.io.cmd_in <> fsm.io.sched_out
    io.issue.exec <> sched.io.issue.exec
    io.issue.load <> sched.io.issue.load
    io.issue.store <> sched.io.issue.store

    // =========================================================================
    // arbitrate incoming completions
    // =========================================================================
    // 三路完成信号, 可能同时 valid,Arbiter 按轮询方式把 ROB-id 合并成一路,
    // 还给 Scheduler,让它释放对应槽位.
    val arb = Module(new Arbiter(UInt(LOG2_ROB_ENTRIES.W), 3))
    arb.io.in(0).valid := io.completed.exec.valid
    arb.io.in(0).bits := io.completed.exec.bits
    arb.io.in(1) <> io.completed.load
    arb.io.in(2) <> io.completed.store
    sched.io.completed <> arb.io.out

    // =========================================================================
    // busy signal
    // =========================================================================
    io.busy := fsm.io.busy || sched.io.busy
}

object TilerController {
    def apply[T <: Data: Arithmetic, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V])(implicit
        p: Parameters
    ) = Module(new TilerController(config))
}
