package riscv

import chisel3._
import chisel3.util._
import scala.collection._

import Control._
import Instruction.NOP


class PipelinedCPU extends Module {
    val io = IO(new Bundle {
        val instAddr = Output(UInt(32.W))
        val instData = Input(UInt(32.W))
        val memAddr = Output(UInt(32.W))
        val memRead = Output(Bool())
        val memRdData = Input(UInt(32.W))
        val memWrite = Output(Bool())
        val memWrData = Output(UInt(32.W))
    })

    val pc = RegInit(0.U(32.W))
    val ctrl = Module(new Control)
    val reg = Module(new RegisterFile)
    val imm = Module(new Immediate)
    val cmp = Module(new Comparator)
    val alu = Module(new ALU)

    val stall = Wire(Bool())  // load-use
    val flush = Wire(Bool()) // jump or take branch
    val rs_data1 = Wire(UInt(32.W))
    val rs_data2 = Wire(UInt(32.W))
    val wb_data = Wire(UInt(32.W))

    reg.clock := (!clock.asBool()).asClock()


    // Pipeline Registers
    // RegNext(next: ..., init: ...)
    // RegEnable(next: ..., init: ..., enable: ...)
    object IF_ID {
        val pc = RegInit(0.U(32.W))
        val inst = RegEnable(Mux(flush, NOP, io.instData), NOP, !stall)
        // for convenience
        val rs1 = inst(19, 15)
        val rs2 = inst(24, 20)
        val rd = inst(11, 7)
    }

    object ID_EX {
        val pc = RegNext(IF_ID.pc)
        val inst = RegNext(IF_ID.inst, NOP)
        val reg_data1 = RegNext(reg.io.rdata1)
        val reg_data2 = RegNext(reg.io.rdata2)
        val imm_sel = RegNext(ctrl.io.immSel)
        val alu_a_sel = RegNext(ctrl.io.aluASel)
        val alu_b_sel = RegNext(ctrl.io.aluBSel)
        val alu_ctrl = RegNext(ctrl.io.aluCtrl)
        val jump     = RegNext(Mux(stall | flush, false.B, ctrl.io.jump), false.B)
        val branch   = RegNext(Mux(stall | flush, false.B, ctrl.io.branch), false.B)
        val load     = RegNext(Mux(stall | flush, false.B, ctrl.io.load), false.B)
        val store  = RegNext(Mux(stall | flush, false.B, ctrl.io.store), false.B)
        val wb_en    = RegNext(Mux(stall | flush, false.B, ctrl.io.wbEn), false.B)
        val wb_sel = RegNext(ctrl.io.wbSel)
        // for convenience
        val rs1 = inst(19, 15)
        val rs2 = inst(24, 20)
        val rd = inst(11, 7)
        val mem_read = load
        val mem_write = store
    }

    object EX_MEM {
        val pc_plus4 = RegNext(ID_EX.pc + 4.U)
        val rd = RegNext(ID_EX.rd)
        val transfer = RegNext(
            Mux(flush, false.B, ID_EX.jump | (ID_EX.branch & cmp.io.branchCond)),
            false.B
        )
        val alu_out = RegNext(alu.io.out)
        val mem_read = RegNext(Mux(flush, false.B, ID_EX.mem_read), false.B)
        val mem_write = RegNext(Mux(flush, false.B, ID_EX.mem_write), false.B)
        val mem_wr_data = RegNext(rs_data2)
        val wb_en = RegNext(Mux(flush, false.B, ID_EX.wb_en), false.B)
        val wb_sel = RegNext(ID_EX.wb_sel)
    }

    object MEM_WB {
        val pc_plus4 = RegNext(EX_MEM.pc_plus4)
        val rd = RegNext(EX_MEM.rd)
        val alu_out = RegNext(EX_MEM.alu_out)
        val mem_rddata = RegNext(io.memRdData)
        val wb_en = RegNext(EX_MEM.wb_en, false.B)
        val wb_sel = RegNext(EX_MEM.wb_sel)
    }

    flush := EX_MEM.transfer
    stall := ID_EX.load && (IF_ID.rs1 === ID_EX.rd || IF_ID.rs2 === ID_EX.rd)

    io.instAddr := pc
    when (EX_MEM.transfer) {
        pc := Cat(EX_MEM.alu_out(31, 1), 0.B)
    } .elsewhen(!stall) {
        pc := pc + 4.U
    }
    when (!stall) {
        IF_ID.pc := pc
    }

    // forwarding
    val forward_c1a = ID_EX.rs1.orR && (ID_EX.rs1 === EX_MEM.rd) && EX_MEM.wb_en && (EX_MEM.wb_sel === WB_SEL_ALU)
    val forward_c1b = ID_EX.rs2.orR && (ID_EX.rs2 === EX_MEM.rd) && EX_MEM.wb_en && (EX_MEM.wb_sel === WB_SEL_ALU)
    val forward_c2a = ID_EX.rs1.orR && (ID_EX.rs1 === MEM_WB.rd) && MEM_WB.wb_en
    val forward_c2b = ID_EX.rs2.orR && (ID_EX.rs2 === MEM_WB.rd) && MEM_WB.wb_en

    when(forward_c1a) {
        rs_data1 := EX_MEM.alu_out
    } .elsewhen(forward_c2a) {
        rs_data1 := wb_data
    } .otherwise {
        rs_data1 := ID_EX.reg_data1
    }
    when(forward_c1b) {
        rs_data2 := EX_MEM.alu_out
    } .elsewhen(forward_c2b) {
        rs_data2 := wb_data
    } .otherwise {
        rs_data2 := ID_EX.reg_data2
    }

    // ------- ID -------
    ctrl.io.instType := Cat(IF_ID.inst(30), IF_ID.inst(14, 12), IF_ID.inst(6, 2))
    reg.io.raddr1 := IF_ID.rs1
    reg.io.raddr2 := IF_ID.rs2

    // ------- EX -------
    imm.io.inst := ID_EX.inst
    imm.io.sel := ID_EX.imm_sel
    cmp.io.a := rs_data1
    cmp.io.b := rs_data2
    cmp.io.funct3 := ID_EX.inst(14, 12)
    alu.io.a := MuxLookup(ID_EX.alu_a_sel, rs_data1, Seq(
        ALU_A_SEL_REG -> rs_data1,
        ALU_A_SEL_PC -> ID_EX.pc
    ))
    alu.io.b := MuxLookup(ID_EX.alu_b_sel, rs_data2, Seq(
        ALU_B_SEL_REG -> rs_data2,
        ALU_B_SEL_IMM -> imm.io.out
    ))
    alu.io.op := ID_EX.alu_ctrl

    // ------- MEM -------
    io.memAddr := EX_MEM.alu_out
    io.memRead := EX_MEM.mem_read
    io.memWrData := EX_MEM.mem_wr_data
    io.memWrite := EX_MEM.mem_write

    // ------- WB -------
    reg.io.wen := MEM_WB.wb_en
    reg.io.waddr := MEM_WB.rd
    reg.io.wdata := wb_data
    wb_data := MuxLookup(MEM_WB.wb_sel, MEM_WB.alu_out, Seq(
        WB_SEL_ALU -> MEM_WB.alu_out,
        WB_SEL_MEM -> MEM_WB.mem_rddata,
        WB_SEL_PC_PLUS4 -> MEM_WB.pc_plus4
    ))
}
