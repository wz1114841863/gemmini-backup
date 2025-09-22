package gemmini

import chisel3._
import chisel3.util._

import Util._

class ZeroWriterReq[Tag <: Data](laddr_t: LocalAddr, max_cols: Int, tag_t: Tag) extends Bundle {
    val laddr = laddr_t // 起始地址
    val cols = UInt(log2Up(max_cols + 1).W) // 要清零的列数
    val block_stride = UInt(16.W) // 块之间的步长, TODO magic number
    val tag = tag_t // 请求的标签, 用于区分不同的请求
}

class ZeroWriterResp[Tag <: Data](laddr_t: LocalAddr, block_cols: Int, tag_t: Tag) extends Bundle {
    val laddr = laddr_t.cloneType // 当前块的起始地址
    val mask = Vec(block_cols, Bool()) // 哪些列需要被清零
    val last = Bool() // 是否是最后一个块
    val tag = tag_t // 请求的标签,回传
}

/*
    ZeroWriter的作用是 将一块内存区域清零, 通常用于初始化输出缓冲区或中间结果缓冲区.
        接收一个请求清零(一片连续地址)的信号,这个模块会计算并并转发(但是不实际进行操作)对应bank的清零指令
 */
class ZeroWriter[T <: Data, U <: Data, V <: Data, Tag <: Data](config: GemminiArrayConfig[T, U, V], tag_t: Tag)
    extends Module {
    import config._

    val block_cols = meshColumns * tileColumns
    val max_cols = (dma_maxbytes / (inputType.getWidth / 8)) max block_cols

    val io = IO(new Bundle {
        val req = Flipped(Decoupled(new ZeroWriterReq(local_addr_t, max_cols, tag_t)))
        val resp = Decoupled(new ZeroWriterResp(local_addr_t, block_cols, tag_t))
    })

    // 当前正在处理的请求
    val req = Reg(UDValid(new ZeroWriterReq(local_addr_t, max_cols, tag_t)))
    // 当前处理到的列索引, 用于跟踪进度
    val col_counter = Reg(UInt(log2Up(max_cols).W))

    io.req.ready := !req.valid

    io.resp.valid := req.valid
    // 计算当前块的地址偏移量, 使用col_counter / block_cols来确定当前是第几个块
    io.resp.bits.laddr := req.bits.laddr + req.bits.block_stride * {
        // This code block was originally just "col_counter / block_cols.U". We
        // changed it to satisfy Verilator's linter
        if (col_counter.getWidth >= log2Ceil(block_cols + 1))
            (col_counter / block_cols.U(col_counter.getWidth.W))
        else
            0.U
    }
    // 对当前块的每一列, 判断是否需要清零(mask为true表示需要清零)
    io.resp.bits.mask.zipWithIndex.foreach { case (m, i) => m := col_counter + i.U < req.bits.cols }
    // 判断是否是最后一个块
    io.resp.bits.last := col_counter +& block_cols.U >= req.bits.cols
    io.resp.bits.tag := req.bits.tag

    when(io.resp.fire) {
        // floorAdd是一个自定义函数,实现"循环加法",即不超过 cols
        val next_col_counter = floorAdd(col_counter, block_cols.U, req.bits.cols)

        col_counter := next_col_counter

        when(next_col_counter === 0.U) {
            req.pop()
            io.req.ready := true.B
        }
    }

    // 当有新请求到来时,保存请求并重置列计数器
    when(io.req.fire) {
        req.push(io.req.bits)

        col_counter := 0.U
    }

    when(reset.asBool) {
        req.pop()
    }
}
