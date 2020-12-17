package riscv

import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

object main {
    def main(args: Array[String]) {
        // println((new ChiselStage).emitVerilog(new ))
        (new ChiselStage).execute(
            Array("-X", "verilog"),
            Seq(ChiselGeneratorAnnotation(() => new PipelinedCPU))
        )
    }
}