package gemmini

import chisel3._
import chisel3.util._

import Util._

/*
    向量-标量乘法器, 用于对向量的每个元素与一个标量值进行乘法运算
    作用: 向量数据与标量数据相乘, 结果输出为新的向量数据
 */
class VectorScalarMultiplierReq[T <: Data, U <: Data, Tag <: Data](block_cols: Int, t: T, u: U, tag_t: Tag)
    extends Bundle {
    val in: Vec[T] = Vec(block_cols, t.cloneType) // 向量数据
    val scale: U = u.cloneType // 标量系数
    val repeats: UInt = UInt(16.W) // 同一向量要重复乘多少次, TODO magic number
    val pixel_repeats: UInt = UInt(8.W) // TODO magic number
    val last: Bool = Bool() // 当前请求是最后一个
    val tag: Tag = tag_t.cloneType
}

class VectorScalarMultiplierResp[T <: Data, Tag <: Data](block_cols: Int, t: T, tag_t: Tag) extends Bundle {
    val out: Vec[T] = Vec(block_cols, t.cloneType) // 乘完后的向量
    val row: UInt = UInt(16.W) // TODO magic number
    val last: Bool = Bool()
    val tag: Tag = tag_t.cloneType
}

class DataWithIndex[T <: Data, U <: Data](t: T, u: U) extends Bundle {
    val data = t.cloneType
    val scale = u.cloneType
    val id = UInt(2.W) // 属于哪一条nEntries, TODO hardcoded
    val index = UInt() // 属于向量中第几个元素
}

class ScalePipe[T <: Data, U <: Data](t: T, mvin_scale_args: ScaleArguments[T, U]) extends Module {
    // 乘法 + 固定延迟移位链, latency 由 mvin_scale_args.latency 给出.
    val u = mvin_scale_args.multiplicand_t
    val io = IO(new Bundle {
        val in = Input(Valid(new DataWithIndex(t, u)))
        val out = Output(Valid(new DataWithIndex(t, u)))
    })
    val latency = mvin_scale_args.latency
    val out = WireInit(io.in)
    out.bits.data := mvin_scale_args.scale_func(io.in.bits.data, io.in.bits.scale.asTypeOf(u))
    io.out := Pipe(out, latency)
}

class VectorScalarMultiplier[T <: Data, U <: Data, Tag <: Data](
    mvin_scale_args: Option[ScaleArguments[T, U]],
    block_cols: Int,
    t: T,
    tag_t: Tag
) extends Module {
    // 根据mvin_scale_args在编译期就分叉成两种硬件结构
    val (u, num_scale_units, always_identity) = mvin_scale_args match {
        case Some(ScaleArguments(_, _, multiplicand_t, num_scale_units, _, _)) =>
            (multiplicand_t, num_scale_units, false)
        case None => (Bool(), -1, true) // TODO make this a 0-width UInt
    }

    val io = IO(new Bundle {
        val req = Flipped(Decoupled(new VectorScalarMultiplierReq(block_cols, t, u, tag_t)))
        val resp = Decoupled(new VectorScalarMultiplierResp(block_cols, t, tag_t))
    })

    val width = block_cols
    val latency = mvin_scale_args match {
        case Some(ScaleArguments(_, latency, _, _, _, _)) => latency
        case None                                         => 0
    }

    val in = Reg(Valid(new VectorScalarMultiplierReq(block_cols, t, u, tag_t)))
    val in_fire = WireInit(false.B)
    io.req.ready := !in.valid || (in.bits.repeats === 0.U && in_fire)

    when(io.req.fire) {
        in.valid := io.req.valid
        in.bits := io.req.bits
    }.elsewhen(in_fire) {
        when(in.bits.repeats === 0.U) {
            in.valid := false.B
        }
        in.bits.repeats := in.bits.repeats - 1.U
    }
    when(reset.asBool) {
        in.valid := false.B
    }

    if (num_scale_units == -1) {
        // 旁路路径: 没有scale单元, 直接输出输入
        // 直接使用mvin_scale_args的scale_func对输入进行缩放
        val pipe = Module(
            new Pipeline[VectorScalarMultiplierResp[T, Tag]](
                new VectorScalarMultiplierResp(block_cols, t, tag_t),
                latency
            )()
        )
        io.resp <> pipe.io.out
        in_fire := pipe.io.in.fire

        pipe.io.in.valid := in.valid
        pipe.io.in.bits.tag := in.bits.tag
        pipe.io.in.bits.last := in.bits.repeats === 0.U && in.bits.last
        pipe.io.in.bits.row := in.bits.repeats
        pipe.io.in.bits.out := (mvin_scale_args match {
            case Some(ScaleArguments(mvin_scale_func, _, multiplicand_t, _, _, _)) =>
                in.bits.in.map(x => mvin_scale_func(x, in.bits.scale.asTypeOf(multiplicand_t)))
            case None => in.bits.in
        })
    } else {
        val nEntries = 3  // 乒乓缓冲, 最多同时处理3个请求
        val regs = Reg(Vec(nEntries, Valid(new VectorScalarMultiplierReq(block_cols, t, u, tag_t))))  // 保存输入向量
        val out_regs = Reg(Vec(nEntries, new VectorScalarMultiplierResp(block_cols, t, tag_t)))  // 保存输出向量

        // 记录每个元素是否已经送入scale单元
        val fired_masks = Reg(Vec(nEntries, Vec(width, Bool())))
        val completed_masks = Reg(Vec(nEntries, Vec(width, Bool())))
        val head_oh = RegInit(1.U(nEntries.W)) // 指向下一个要毕业的slot
        val tail_oh = RegInit(1.U(nEntries.W)) // 指向下一个要填充的slot

        // 如果head指向的条目已完成, 就把io.resp.valid 拉高
        io.resp.valid := Mux1H(
            head_oh.asBools,
            (regs zip completed_masks).map({ case (r, c) => r.valid && c.reduce(_ && _) })
        )
        io.resp.bits := Mux1H(head_oh.asBools, out_regs)
        when(io.resp.fire) {
            for (i <- 0 until nEntries) {
                when(head_oh(i)) {
                    regs(i).valid := false.B  // 只对 head 指向的那一条目清零
                }
            }
            head_oh := (head_oh << 1) | head_oh(nEntries - 1)  // 循环左移
        }

        // tail_oh指针指向 下一个可写的空条目
        // 把一条新请求(in)写进nEntries乒乓缓冲
        in_fire := (in.valid &&
            (!Mux1H(tail_oh.asBools, regs.map(_.valid))))
        when(in_fire) {
            for (i <- 0 until nEntries) {
                when(tail_oh(i)) {
                    regs(i).valid := true.B
                    regs(i).bits := in.bits
                    out_regs(i).tag := in.bits.tag
                    out_regs(i).last := in.bits.repeats === 0.U && in.bits.last
                    out_regs(i).row := in.bits.repeats
                    out_regs(i).out := in.bits.in
                    val identity = (u match {
                        case u: UInt  => Arithmetic.UIntArithmetic.cast(u).identity
                        case s: SInt  => Arithmetic.SIntArithmetic.cast(s).identity
                        case f: Float => Arithmetic.FloatArithmetic.cast(f).identity
                        case b: Bool  => 1.U(1.W)
                    })
                    // 如果送来的scale正好等于1,就把该条目的所有 fired/completed 掩码一次性全置 1.
                    fired_masks(i).foreach(_ := in.bits.scale.asUInt === identity.asUInt || always_identity.B)
                    completed_masks(i).foreach(_ := in.bits.scale.asUInt === identity.asUInt || always_identity.B)
                }
            }
            // 写完后tail_oh循环左移 1 位,把最高位卷回最低位,形成 FIFO 环形指针.
            tail_oh := (tail_oh << 1) | tail_oh(nEntries - 1)
        }

        // 双重循环把 二维矩阵 (条目, 元素) 拉平成一维
        val inputs = Seq.fill(width * nEntries) { Wire(Decoupled(new DataWithIndex(t, u))) }
        for (i <- 0 until nEntries) {
            for (w <- 0 until width) {
                val input = inputs(i * width + w)
                input.valid := regs(i).valid && !fired_masks(i)(w)
                input.bits.data := regs(i).bits.in(w)
                input.bits.scale := regs(i).bits.scale.asTypeOf(u)
                input.bits.id := i.U
                input.bits.index := w.U
                when(input.fire) {
                    fired_masks(i)(w) := true.B
                }
            }
        }
        for (i <- 0 until num_scale_units) {
            // // 挑出"序号 % num_scale_units == i"的所有元素通道
            val arbIn = inputs.zipWithIndex.filter({ case (_, w) => w % num_scale_units == i }).map(_._1)
            // 接一个轮询仲裁器
            val arb = Module(new RRArbiter(new DataWithIndex(t, u), arbIn.length))
            arb.io.in <> arbIn
            arb.io.out.ready := true.B
            // 仲裁器输出先寄存一级,防止组合路径过长
            val arbOut = Reg(Valid(new DataWithIndex(t, u)))
            arbOut.valid := arb.io.out.valid
            arbOut.bits := arb.io.out.bits
            when(reset.asBool) {
                arbOut.valid := false.B
            }

            val pipe = Module(new ScalePipe(t, mvin_scale_args.get))
            pipe.io.in := arbOut
            val pipe_out = pipe.io.out
            // 把结果写回对应的 out_regs和completed_masks
            for (j <- 0 until nEntries) {
                for (w <- 0 until width) {
                    if ((j * width + w) % num_scale_units == i) {
                        when(pipe_out.fire && pipe_out.bits.id === j.U && pipe_out.bits.index === w.U) {
                            out_regs(j).out(w) := pipe_out.bits.data
                            completed_masks(j)(w) := true.B
                        }
                    }
                }
            }
        }

        when(reset.asBool) {
            regs.foreach(_.valid := false.B)
        }
    }
}

object VectorScalarMultiplier {
    // Returns the input and output IO of the module (together with the pipeline)
    def apply[T <: Data, U <: Data, Tag <: Data](
        scale_args: Option[ScaleArguments[T, U]],
        t: T,
        cols: Int,
        tag_t: Tag,
        is_acc: Boolean,
        is_mvin: Boolean = true
    ) = {
        assert(!is_acc || is_mvin)
        val vsm = Module(new VectorScalarMultiplier(scale_args, cols, t, tag_t))
        val vsm_in_q = Module(new Queue(chiselTypeOf(vsm.io.req.bits), 2))
        vsm.io.req <> vsm_in_q.io.deq
        (vsm_in_q.io.enq, vsm.io.resp)
    }
}
