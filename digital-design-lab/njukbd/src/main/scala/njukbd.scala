package njukbd

import chisel3._, chisel3.util._
import KeyboardState._

class njukbd extends Module {
  val io = IO(new Bundle {
    val ps2Clk = Input(Bool())
    val ps2Data = Input(Bool())
    val hex8 = Output(UInt((6 * 8).W))
  })

  val buffer = RegInit(0.U(10.W))
  val count = RegInit(0.U(4.W))

  val ps2ClkReg = RegInit(0.U(3.W))
  ps2ClkReg := ps2ClkReg(1, 0) ## io.ps2Clk

  val state = RegInit(kbdWait)

  val keyCount = RegInit(0.U(8.W))
  val currentKey = RegInit(0.U(8.W))

  val looseKey = RegInit(false.B)
  val closeHex = RegInit(true.B)

  switch(state) {
    is(kbdWait) {
      when(ps2ClkReg(2) & ~ps2ClkReg(1)) {
        buffer := io.ps2Data ## buffer(9, 1)
        count := count + 1.U
        state := kbdSample
        closeHex := false.B
        printf("back to kbdSample\n")
      }

    }

    is(kbdSample) {
      when(ps2ClkReg(2) & ~ps2ClkReg(1)) {
        when(count === 10.U) {
          when(~buffer(0) & io.ps2Data & buffer(9, 1).xorR) {
            state := kbdReady
          }.otherwise {
            state := kbdWait
          }
          count := 0.U
          printf("buffer: %b, count: %d\n", buffer, count)
        }.otherwise {
          buffer := io.ps2Data ## buffer(9, 1)
          count := count + 1.U
          printf("buffer: %b, count: %d\n", buffer, count)
        }
      }
    }

    is(kbdReady) {

      when(buffer(8, 1) === "hF0".U) {
        keyCount := keyCount + 1.U
        looseKey := true.B
      }

      when(looseKey) {
        currentKey := 0.U
        looseKey := false.B
        closeHex := true.B
      }.otherwise {
        currentKey := buffer(8, 1)
      }

      when(ps2ClkReg(2) & ~ps2ClkReg(1)) {
        buffer := io.ps2Data ## buffer(9, 1)
        count := count + 1.U
        state := kbdSample
      }

      buffer := 0.U
      state := kbdWait
    }
  }

  val keyCountHex = Module(new Hex8Out2())
  keyCountHex.io.num := keyCount
  val currentKeyHex = Module(new Hex8Out2())
  currentKeyHex.io.num := currentKey

  io.hex8 := keyCountHex.io.out ## "b1111_1111_1111_1111".U ## Mux(
    closeHex,
    "b1111_1111_1111_1111".U,
    currentKeyHex.io.out
  )

}

object Main extends App {
  println("Hello World, I will now generate the Verilog file!")
  emitVerilog(new njukbd, Array("--target-dir", "out/verilog"))
}
