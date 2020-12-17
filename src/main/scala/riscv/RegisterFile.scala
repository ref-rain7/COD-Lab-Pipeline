package riscv

import chisel3._
import chisel3.util._
import scala.collection._

class RegisterFile(w:Int = 32) extends Module {
    val io = IO(new Bundle {
        val wen = Input(Bool())
        val waddr = Input(UInt(5.W))
        val wdata = Input(UInt(w.W))
        val raddr1 = Input(UInt(5.W))
        val rdata1 = Output(UInt(w.W))
        val raddr2 = Input(UInt(5.W))
        val rdata2 = Output(UInt(w.W))
    })

    val reg = RegInit(VecInit(Seq.fill(32)(0.U(w.W))))

    when (io.wen) {
        reg(io.waddr) := io.wdata
    }

    io.rdata1 := Mux(io.raddr1 === 0.U(5.W), 0.U(w.W), reg(io.raddr1))
    io.rdata2 := Mux(io.raddr2 === 0.U(5.W), 0.U(w.W), reg(io.raddr2))
}
