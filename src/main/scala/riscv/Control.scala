package riscv

import chisel3._
import chisel3.util._
import scala.collection._


object Control {
    val IMM_I = 0.U(3.W)
    val IMM_S = 2.U(3.W)
    val IMM_B = 3.U(3.W)
    val IMM_U = 4.U(3.W)
    val IMM_J = 5.U(3.W)

    val ALU_A_SEL_REG = 0.U(1.W)
    val ALU_A_SEL_PC = 1.U(1.W)

    val ALU_B_SEL_REG = 0.U(1.W)
    val ALU_B_SEL_IMM = 1.U(1.W)

    val WB_SEL_ALU = 0.U(2.W)
    val WB_SEL_PC_PLUS4 = 1.U(2.W)
    val WB_SEL_MEM = 2.U(2.W)
}

import Instruction._
import  Control._

class Control extends Module {
    val io = IO(new Bundle {
        val instType = Input(UInt(9.W)) /* instr[30], instr[14:12], instr[6:2] */

        val branch = Output(Bool())
        val jump = Output(Bool())
        val load = Output(Bool())
        val store = Output(Bool())

        val immSel = Output(UInt(3.W))
        val aluASel = Output(UInt(1.W))
        val aluBSel = Output(UInt(1.W))
        val aluCtrl = Output(UInt(4.W))
        val wbEn = Output(Bool())
        val wbSel = Output(UInt(2.W))
    })

    /* defaults */
    io.branch := false.B
    io.jump := false.B
    io.load := false.B
    io.store := false.B

    io.immSel := IMM_I
    io.aluASel := ALU_A_SEL_REG
    io.aluBSel := ALU_B_SEL_REG
    io.aluCtrl := "hf".U // meaningless
    io.wbEn := false.B
    io.wbSel := WB_SEL_ALU

    when (io.instType === LUI) {
        io.immSel := IMM_U
        io.aluBSel := ALU_B_SEL_IMM; io.aluCtrl := ALU.COPY_B
        io.wbEn := true.B
    }
    .elsewhen (io.instType === AUIPC) {
        io.immSel := IMM_U
        io.aluASel := ALU_A_SEL_PC; io.aluBSel := ALU_B_SEL_IMM; io.aluCtrl := ALU.ADD
        io.wbEn := true.B
    }
    .elsewhen (io.instType === JAL) {
        io.jump := true.B
        io.immSel := IMM_J
        io.aluASel := ALU_A_SEL_PC; io.aluBSel := ALU_B_SEL_IMM; io.aluCtrl := ALU.ADD
        io.wbEn := true.B; io.wbSel := WB_SEL_PC_PLUS4
    }
    .elsewhen (io.instType === JALR) {
        io.jump := true.B
        io.aluASel := ALU_A_SEL_REG; io.aluBSel := ALU_B_SEL_IMM; io.aluCtrl := ALU.ADD
        io.wbEn := true.B; io.wbSel := WB_SEL_PC_PLUS4
    }
    .elsewhen (io.instType === BRANCH) {
        io.branch := true.B
        io.immSel := IMM_B
        io.aluASel := ALU_A_SEL_PC; io.aluBSel := ALU_B_SEL_IMM; io.aluCtrl := ALU.ADD
    }
    .elsewhen (io.instType === LW) {
        io.load := true.B
        io.aluASel := ALU_A_SEL_REG; io.aluBSel := ALU_B_SEL_IMM; io.aluCtrl := ALU.ADD
        io.wbEn := true.B; io.wbSel := WB_SEL_MEM
    }
    .elsewhen(io.instType === SW) {
        io.store := true.B
        io.immSel := IMM_S
        io.aluASel := ALU_A_SEL_REG; io.aluBSel := ALU_B_SEL_IMM; io.aluCtrl := ALU.ADD
    }



    private val regImmInstrArgs = Array(
        (ADDI, ALU.ADD), (SLTI, ALU.SLT), (SLTIU, ALU.SLTU),
        (XORI, ALU.XOR), (ORI, ALU.OR), (ANDI, ALU.AND),
        (SLLI, ALU.SLL), (SRLI, ALU.SRL), (SRAI, ALU.SRA)
    )
    for (t <- regImmInstrArgs) {
        when (io.instType === t._1) {
            io.wbEn := true.B
            io.aluBSel := ALU_B_SEL_IMM; io.aluCtrl := t._2
        }
    }

    private val regRegInstrArgs = Array(
        (ADD, ALU.ADD), (SUB, ALU.SUB), (SLL, ALU.SLL), (SLT, ALU.SLT), (SLTU, ALU.SLTU),
        (XOR, ALU.XOR), (SRL, ALU.SRL), (SRA, ALU.SRA), (OR, ALU.OR), (AND, ALU.AND)
    )
    for (t <- regRegInstrArgs) {
        when (io.instType === t._1) {
            io.wbEn := true.B
            io.aluCtrl := t._2
        }
    }

}
