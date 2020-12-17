package riscv

import chisel3._
import chisel3.util._
import scala.collection._

object BarrelShifter {
    val SLL = "b00".U
    val SRL = "b01".U
    val SRA = "b11".U
    val ROR = "b10".U
}

import BarrelShifter._

class BarrelShifter extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(32.W))
        val shamt = Input(UInt(5.W))
        val op = Input(UInt(2.W))
        val out = Output(UInt(32.W))
    })

    private val shift = Wire(Vec(6, UInt(32.W)))
    shift(0) := io.in
    io.out := shift(5)

    for (i <- 1 to 5) {
        val d = 1 << (i-1)
        shift(i) := Mux(io.shamt(i-1) === false.B, shift(i - 1),
            MuxLookup (io.op, io.in, Seq (
                SLL -> Cat(shift(i-1)(31-d, 0), 0.U(d.W)),
                SRL -> Cat(0.U(d.W), shift(i-1)(31, d)),
                SRA -> Cat(Fill(d, io.in(31)), shift(i-1)(31, d)),
                ROR -> Cat(shift(i-1)(d-1, 0), shift(i-1)(31, d))
            ))
        )
    }
}
