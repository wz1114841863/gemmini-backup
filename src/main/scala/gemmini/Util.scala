package gemmini

import chisel3._
import chisel3.util._

// 各种工具函数
object Util {
    // 回绕加法
    def wrappingAdd(u: UInt, n: UInt, max_plus_one: Int): UInt = {
        val max = max_plus_one - 1
        if (max == 0) {
            0.U
        } else {
            assert(n <= max.U, "cannot wrapAdd when n is larger than max")
            Mux(u >= max.U - n + 1.U && n =/= 0.U, n - (max.U - u) - 1.U, u + n)
        }
    }

    // 回绕加法,带使能信号
    def wrappingAdd(u: UInt, n: UInt, max_plus_one: UInt, en: Bool = true.B): UInt = {
        val max = max_plus_one - 1.U
        assert(n <= max || max === 0.U, "cannot wrapAdd when n is larger than max, unless max is 0")

        /*
        Mux(!en, u,
            Mux (max === 0.U, 0.U,
            Mux(u >= max - n + 1.U && n =/= 0.U, n - (max - u) - 1.U, u + n)))
         */

        // MuxCase(default, Array(c1 -> a, c2 -> b, ...))
        MuxCase(
            u + n,
            Seq(
                (!en) -> u,
                (max === 0.U) -> 0.U,
                (u >= max - n + 1.U && n =/= 0.U) -> (n - (max - u) - 1.U)
            )
        )
    }

    // 饱和加法, 防止溢出, +&(加宽加法, 防止溢出截断)
    def satAdd(u: UInt, v: UInt, max: UInt): UInt = {
        Mux(u +& v > max, max, u + v)
    }

    // 回零加法, 加法后如果超过max, 则回零
    def floorAdd(u: UInt, n: UInt, max_plus_one: UInt, en: Bool = true.B): UInt = {
        val max = max_plus_one - 1.U

        MuxCase(
            u + n,
            Seq(
                (!en) -> u,
                ((u +& n) > max) -> 0.U
            )
        )
    }

    // 回零加法, 带符号数
    def sFloorAdd(s: SInt, n: UInt, max_plus_one: SInt, min: SInt, en: Bool = true.B): SInt = {
        val max = max_plus_one - 1.S

        MuxCase(
            s + n.zext,
            Seq(
                (!en) -> s,
                ((s +& n.zext) > max) -> min
            )
        )
    }

    // 回绕减法
    def wrappingSub(u: UInt, n: UInt, max_plus_one: Int): UInt = {
        val max = max_plus_one - 1
        assert(n <= max.U, "cannot wrapSub when n is larger than max")
        Mux(u < n, max.U - (n - u) + 1.U, u - n)
    }

    // 向上取整除法, ceilingDivide(10, 3) = 4
    def ceilingDivide(numer: Int, denom: Int): Int = {
        if (numer % denom == 0) { numer / denom }
        else { numer / denom + 1 }
    }

    // 找到小于等于 u 的最大 2 的幂
    def closestLowerPowerOf2(u: UInt): UInt = {
        // TODO figure out a more efficient way of doing this. Is this many muxes really necessary?
        // 找到最高有效位索引
        val exp = u.asBools.zipWithIndex
            .map { case (b, i) =>
                Mux(b, i.U, 0.U)
            }
            .reduce((acc, u) => Mux(acc > u, acc, u))

        (1.U << exp).asUInt
    }

    // 在满足地址对齐和 stride 对齐的前提下,找到最大 2 的幂
    def closestAlignedLowerPowerOf2(u: UInt, addr: UInt, stride: UInt, rowBytes: Int): UInt = {
        // 计算对齐位宽, rowBytes是2的幂, 所以低lgRowBytes位为0即表示"对齐"
        val lgRowBytes = log2Ceil(rowBytes)

        // TODO figure out a more efficient way of doing this. Is this many muxes really necessary?
        val exp = u.asBools.zipWithIndex
            .map { case (b, i) =>
                Mux(b && addr(i + lgRowBytes - 1, 0) === 0.U && stride(i + lgRowBytes - 1, 0) === 0.U, i.U, 0.U)
            }
            .reduce((acc, u) => Mux(acc > u, acc, u))

        (1.U << exp).asUInt
    }

    // This function will return "next" with a 0-cycle delay when the "enable" signal is high. It's like a queue with
    // the "pipe" and "flow" parameters set to "true"
    def RegEnableThru[T <: Data](next: T, enable: Bool): T = {
        val buf = RegEnable(next, enable)
        Mux(enable, next, buf)
    }

    def RegEnableThru[T <: Data](next: T, init: T, enable: Bool): T = {
        val buf = RegEnable(next, init, enable)
        Mux(enable, next, buf)
    }

    def maxOf(u1: UInt, u2: UInt): UInt = {
        Mux(u1 > u2, u1, u2)
    }

    // 泛型, 支持任何实现了Arithmetic类型类的类型
    def maxOf[T <: Data](x: T, y: T)(implicit ev: Arithmetic[T]): T = {
        import ev._
        Mux(x > y, x, y)
    }

    def minOf(u1: UInt, u2: UInt): UInt = {
        Mux(u1 < u2, u1, u2)
    }

    // 将一组数用二叉树方式累加,减少加法器延迟,
    // 调用 accumulateTree 的地方会生成一个树形加法器
    def accumulateTree[T <: Data](xs: Seq[T])(implicit ev: Arithmetic[T]): T = {
        import ev._

        assert(xs.nonEmpty, "can't accumulate 0 elements")

        if (xs.length == 1) {
            xs.head
        } else {
            val upperRowLen = 1 << log2Ceil(xs.length)
            val upperRow = xs.padTo(upperRowLen, xs.head.zero)
            val pairs = upperRow.grouped(2)
            val lowerRow = pairs.map { case Seq(a, b) => a + b }
            accumulateTree(lowerRow.toSeq)
        }
    }

    // An undirectioned Valid bundle
    class UDValid[T <: Data](t: T) extends Bundle {
        val valid = Bool()
        val bits = t.cloneType

        def push(b: T): Unit = {
            valid := true.B
            bits := b
        }

        def pop(dummy: Int = 0): T = {
            valid := false.B
            bits
        }

    }

    object UDValid {
        def apply[T <: Data](t: T): UDValid[T] = new UDValid(t)
    }

    // creates a Reg and the next-state Wire, and returns both
    def regwire(bits: Int) = {
        val wire = Wire(UInt(bits.W))
        val reg = RegNext(wire)
        wire := reg // default wire to read from reg
        (reg, wire)
    }
}
