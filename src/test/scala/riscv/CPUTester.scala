package riscv

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import scala.util._
import chisel3.util._
import riscv.CPUTests.{getMem, passInstAddr}

import scala.io.Source

class CPUTester extends ChiselFlatSpec {
    val args = Array(
        "--backend-name", "verilator",
        "--generate-vcd-output", "off"
    )
    Driver.execute(args, () => new PipelinedCPU) {
        c => new CPUTests(c)
    }
}

object CPUTests {
    val failInstAddr = 0x3500
    val passInstAddr = 0x3504

    def getMem() : Array[Long] = {
        val src = Source.fromFile("src/test/scala/riscv/ram.txt")
        val mem = new Array[Long](0x2a00)
        for (line <- src.getLines()) {
            val s = line.trim.stripPrefix("@").split(' ')
            if (!s(0).isEmpty) {
                val addr = BigInt(s(0), 16).toInt
                mem(addr) = BigInt(s(1), 16).toLong
            }
        }
        src.close()
        return mem
    }
}

class CPUTests(c : PipelinedCPU) extends PeekPokeTester(c) {
    val mem = getMem()

    def runCycle(): Long = {
        val instAddr = peek(c.io.instAddr).toLong
        val memAddr = peek(c.io.memAddr).toLong
        val memRead = peek(c.io.memRead).toLong
        val memWrData = peek(c.io.memWrData).toLong
        val memWrite = peek(c.io.memWrite).toLong
        poke(c.io.instData, mem(instAddr / 4))
        if (memRead != 0) {
            poke(c.io.memRdData, mem(memAddr / 4))
        }
        if (memWrite != 0) {
            mem(memAddr / 4) = memWrData
        }
        step(1)
        return instAddr
    }

    def test(): Unit = {
        var hitPassCnt = 0
        for (i <- 0 until 0x4000) {
            val instAddr = runCycle()
            if (instAddr == passInstAddr) {
                hitPassCnt += 1
            }
            if (hitPassCnt == 3) {
                for (t <- 0 until 10) {
                    val instAddr = runCycle()
                    println(instAddr.toHexString)
                }
                return
            }
        }
    }

    test()
}