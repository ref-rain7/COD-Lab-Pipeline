package riscv

import chisel3._
import chisel3.util._

import Control._

class Immediate extends Module {
    val io = IO(new Bundle {
        val sel = Input(UInt(3.W))
        val inst = Input(UInt(32.W))
        val out = Output(UInt(32.W))
    })

    io.out := MuxLookup(io.sel, Cat(Fill(20, io.inst(31)), io.inst(31, 20)),
        Seq(
            IMM_I -> Cat(Fill(20, io.inst(31)), io.inst(31, 20)),
            IMM_S -> Cat(Fill(20, io.inst(31)), io.inst(31, 25), io.inst(11, 7)),
            IMM_B -> Cat(Fill(20, io.inst(31)), io.inst(7), io.inst(30, 25), io.inst(11, 8), 0.B),
            IMM_U -> Cat(io.inst(31, 12), Fill(12, 0.B)),
            IMM_J -> Cat(Fill(12, io.inst(31)), io.inst(19, 12), io.inst(20), io.inst(30, 21), 0.B)
        )
    )
}