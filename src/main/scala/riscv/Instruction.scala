package riscv

import chisel3._
import chisel3.util._

object Instruction {
    val NOP = "b0010011".asUInt(32.W) // addi x0, x0, 0

    /* instr[30], instr[14:12], instr[6:2] */
    val LUI   = BitPat("b?_???_01101")
    val AUIPC = BitPat("b?_???_00101")

    val JAL   = BitPat("b?_???_11011")
    val JALR  = BitPat("b?_???_11001") // ignore func3 (it should be "b000"), instead, use "???"

    val BRANCH = BitPat("b?_???_11000")
    val BEQ   = BitPat("b?_000_11000")
    val BNE   = BitPat("b?_001_11000")
    val BLT   = BitPat("b?_100_11000")
    val BGE   = BitPat("b?_101_11000")
    val BLTU  = BitPat("b?_110_11000")
    val BGEU  = BitPat("b?_111_11000")

    val ADDI  = BitPat("b?_000_00100")
    val SLTI  = BitPat("b?_010_00100")
    val SLTIU = BitPat("b?_011_00100")
    val XORI  = BitPat("b?_100_00100")
    val ORI   = BitPat("b?_110_00100")
    val ANDI  = BitPat("b?_111_00100")
    val SLLI  = BitPat("b?_001_00100") // ignore instr[30](i.e. funct7[5]) (it should be "b0"), instead, use "?"
    val SRLI  = BitPat("b0_101_00100")
    val SRAI  = BitPat("b1_101_00100")

    val ADD   = BitPat("b0_000_01100")
    val SUB   = BitPat("b1_000_01100")
    val SLL   = BitPat("b?_001_01100") // ignore instr[30](i.e. funct7[5])
    val SLT   = BitPat("b?_010_01100") // ignore instr[30](i.e. funct7[5])
    val SLTU  = BitPat("b?_011_01100") // ignore instr[30](i.e. funct7[5])
    val XOR   = BitPat("b?_100_01100") // ignore instr[30](i.e. funct7[5])
    val SRL   = BitPat("b0_101_01100")
    val SRA   = BitPat("b1_101_01100")
    val OR    = BitPat("b?_110_01100") // ignore instr[30](i.e. funct7[5])
    val AND   = BitPat("b?_111_01100") // ignore instr[30](i.e. funct7[5])

    val LW    = BitPat("b?_010_00000")
    val SW    = BitPat("b?_010_01000")

    /* not implemented */
    val LB    = BitPat("b?_000_00000")
    val LH    = BitPat("b?_001_00000")
    val LBU   = BitPat("b?_100_00000")
    val LHU   = BitPat("b?_101_00000")
    val SB    = BitPat("b?_000_01000")
    val SH    = BitPat("b?_001_01000")
}

