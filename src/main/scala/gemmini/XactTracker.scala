package gemmini

import chisel3._
import chisel3.util._
import gemmini.Util.UDValid
import midas.targetutils.SynthesizePrintf

/*
    事务追踪器, 用于跟踪未完成的DMA请求.
*/
class XactTrackerEntry[U <: Data](
    maxShift: Int,
    spadWidth: Int,
    accWidth: Int,
    spadRows: Int,
    accRows: Int,
    maxReqBytes: Int,
    mvin_scale_t_bits: Int,
    nCmds: Int
) extends Bundle {
    // DMA请求的元数据包, 描述了从哪里搬/搬多少/怎么搬/搬到哪里
    val shift = UInt(log2Up(maxShift).W)  // 请求的字节偏移量
    val addr = UInt(log2Up(spadRows max accRows).W)  // 目标地址(行号)
    val is_acc = Bool()  // 目标是acc还是spad
    val accumulate = Bool()  // 是否进行累加
    val has_acc_bitwidth = Bool()  // 累加时,是否使用acc的位宽
    val scale = UInt(mvin_scale_t_bits.W)  // 缩放因子
    val repeats = UInt(16.W) // 重复次数, TODO magic number
    val pixel_repeats = UInt(8.W) // 像素重复次数, TODO magic number
    val len = UInt(16.W) // 数据长度, TODO magic number
    val block_stride = UInt(16.W) // 快间stride, TODO magic number
    val spad_row_offset = UInt(log2Up(spadWidth max accWidth).W)  // spad行偏移量
    val lg_len_req = UInt(log2Up(log2Up(maxReqBytes + 1) + 1).W)  // 每个请求的长度(以2的幂次表示)
    val bytes_to_read = UInt(log2Up(maxReqBytes + 1).W)  // 还需要读取的字节数
    val cmd_id = UInt(log2Up(nCmds).W)  // 命令ID, 用于区分不同的命令
}


class XactTrackerAllocIO[U <: Data](
    nXacts: Int,
    maxShift: Int,
    spadWidth: Int,
    accWidth: Int,
    spadRows: Int,
    accRows: Int,
    maxReqBytes: Int,
    mvin_scale_t_bits: Int,
    nCmds: Int
) extends Bundle {
    // 外部模块请求分配一个事务entry, 返回一个空闲的xactid和entry内容.
    val valid = Output(Bool())  // 是否有alloc请求
    val ready = Input(Bool())  // 是否准备好接受请求

    val xactid = Input(UInt(log2Up(nXacts).W))  // 请求方选择的事务 ID
    val entry = Output(
        new XactTrackerEntry(maxShift, spadWidth, accWidth, spadRows, accRows, maxReqBytes, mvin_scale_t_bits, nCmds)
    )

    def fire(dummy: Int = 0) = valid && ready

}


class XactTrackerPeekIO[U <: Data](
    val nXacts: Int,
    val maxShift: Int,
    val spadWidth: Int,
    val accWidth: Int,
    val spadRows: Int,
    val accRows: Int,
    val maxReqBytes: Int,
    mvin_scale_t_bits: Int,
    nCmds: Int
) extends Bundle {
    // 根据xactid查询entry内容, 或释放该entry(pop = true)
    val xactid = Input(UInt(log2Up(nXacts).W))  // 要查询的事务 ID
    val pop = Input(Bool())  // 是否释放该entry
    val entry = Output(
        new XactTrackerEntry(maxShift, spadWidth, accWidth, spadRows, accRows, maxReqBytes, mvin_scale_t_bits, nCmds)
    )
}


/*
    事务追踪表, 记录当前正在进行的DMA请求的状态.
    每个请求对应一个entry, 通过xactid索引.
    maxShift: the maximum number of bytes in the beginning of a TileLink response which may be discarded
    spadWidth: the width of an spad row in bytes
    spadRows: the total number of rows in the spad
    maxReqBytes:
    Removed:
        maxMatrices: the maximum number of rows from different matrices which can be packed into one request
 */
class XactTracker[U <: Data](
    nXacts: Int,
    maxShift: Int,
    spadWidth: Int,
    accWidth: Int,
    spadRows: Int,
    accRows: Int,
    maxReqBytes: Int,
    mvin_scale_t_bits: Int,
    nCmds: Int,
    use_firesim_simulation_counters: Boolean
) extends Module {
    val io = IO(new Bundle {
        val alloc = Flipped(
            new XactTrackerAllocIO(
                nXacts,
                maxShift,
                spadWidth,
                accWidth,
                spadRows,
                accRows,
                maxReqBytes,
                mvin_scale_t_bits,
                nCmds
            )
        )
        val peek = new XactTrackerPeekIO(
            nXacts,
            maxShift,
            spadWidth,
            accWidth,
            spadRows,
            accRows,
            maxReqBytes,
            mvin_scale_t_bits,
            nCmds
        )
        val busy = Output(Bool())

        val counter = new CounterEventIO()
    })
    // 追踪表存储nXacts个entry
    val entries = Reg(
        Vec(
            nXacts,
            UDValid(
                new XactTrackerEntry(
                    maxShift,
                    spadWidth,
                    accWidth,
                    spadRows,
                    accRows,
                    maxReqBytes,
                    mvin_scale_t_bits,
                    nCmds
                )
            )
        )
    )
    // 找到第一个空闲的entry, 分配给请求方
    // 如果没有空闲entry, 则分配(nXacts - 1), ready为false
    val free_entry = MuxCase((nXacts - 1).U, entries.zipWithIndex.map { case (e, i) => !e.valid -> i.U })
    io.alloc.ready := !entries.map(_.valid).reduce(_ && _)
    io.alloc.xactid := free_entry

    io.peek.entry := entries(io.peek.xactid).bits

    io.busy := entries.map(_.valid).reduce(_ || _)

    // 写入与释放
    when(io.alloc.fire()) {
        entries(free_entry).valid := true.B
        entries(free_entry).bits := io.alloc.entry
    }

    when(io.peek.pop) {
        entries(io.peek.xactid).valid := false.B
        assert(entries(io.peek.xactid).valid)
    }

    // 复位逻辑
    when(reset.asBool) {
        entries.foreach(_.valid := false.B)
    }

    // Performance counters
    CounterEventIO.init(io.counter)

    val total_latency = RegInit(0.U(CounterExternal.EXTERNAL_WIDTH.W))
    when(io.counter.external_reset) {
        total_latency := 0.U
    }.otherwise {
        total_latency := total_latency + PopCount(entries.map(_.valid))
    }

    io.counter.connectExternalCounter(CounterExternal.RDMA_TOTAL_LATENCY, total_latency)

    // FireSim 调试输出
    if (use_firesim_simulation_counters) {
        val cntr = Counter(500000)
        when(cntr.inc()) {
            printf(SynthesizePrintf("RDMA total latency: %d\n", total_latency))
        }
    }
}
