package riscv

import chisel3._

class Comparator extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(32.W))
        val b = Input(UInt(32.W))
        val funct3 = Input(UInt(3.W))
        val branchCond = Output(Bool())
    })

    /* Branch Instructions:
     * funct3[2]: '0': use EQUAL signal
     *            '1': use LESS_THAN signal
     * funct3[1]: '0': comparison is signed,
     *            '1': comparison is unsigned
     * funct3[0]: '0': take branch if signal selected by funct[2] is '1'
     *            '1': take branch if signal selected by funct[2] is '0'
     */
    val equal = Wire(Bool())
    val lessThan = Wire(Bool())
    equal := ~((io.a ^ io.b).orR())
    lessThan := Mux(io.funct3(1), io.a < io.b, io.a.asSInt < io.b.asSInt)
    io.branchCond := io.funct3(0) ^ Mux(io.funct3(2), lessThan, equal)
}