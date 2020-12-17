package riscv

import chisel3._
import chisel3.util._
import scala.collection._

object ALU {
    val ADD  = "b0000".U
    val SUB  = "b1000".U
    val SLL  = "b0001".U
    val SLT  = "b0010".U
    val SLTU = "b0011".U
    val XOR  = "b0100".U
    val SRL  = "b0101".U
    val SRA  = "b1101".U
    val OR   = "b0110".U
    val AND  = "b0111".U

    val COPY_B = "b1111".U
}

import ALU._

class ALU extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(32.W))
        val b = Input(UInt(32.W))
        val op = Input(UInt(4.W))
        val out = Output(UInt(32.W))
    })

    var shifter = Module(new BarrelShifter)
    shifter.io.in := io.a
    shifter.io.shamt := io.b(4, 0)
    shifter.io.op := io.op(3, 2)

    io.out := MuxLookup (io.op, io.a, Seq (
        ADD -> (io.a + io.b),
        SUB -> (io.a - io.b),
        OR -> (io.a | io.b),
        AND -> (io.a & io.b),
        XOR -> (io.a ^ io.b),

        SLT -> Cat(0.U(31.W), (io.a.asSInt < io.b.asSInt)),
        SLTU -> Cat(0.U(31.W), (io.a < io.b)),

        SLL -> shifter.io.out,
        SRL -> shifter.io.out,
        SRA -> shifter.io.out,

        COPY_B -> io.b
    ))
}
