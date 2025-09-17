package gemmini

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

// Arithmetic[UInt] 实例
trait Arithmetic[T <: Data] {

}

// MacUnit 黄金模型
case class MacUnitGolden(a: UInt, b: UInt, c: UInt) {
    def compute: Int = a * b + acc
}
