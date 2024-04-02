import chisel3._, chisel3.util._

class njualu extends Module {
  val io = IO(new Bundle {
    val controlSignal = Input(UInt(3.W))
    val enable = Input(Bool())
    val numX = Input(UInt(4.W))
    val numY = Input(UInt(4.W))
    val resultLED = Output(UInt(16.W))
    val resultHex8 = Output(UInt(24.W))
  })

  val result = Wire(UInt())
  result := 0.U

  switch(io.controlSignal) {
    is("b000".U) { result := io.numX + io.numY }
    is("b001".U) { result := io.numX - io.numY }
    is("b010".U) { result := ~io.numX }
    is("b011".U) { result := io.numX & io.numY }
    is("b100".U) { result := io.numX | io.numY }
    is("b101".U) { result := io.numX ^ io.numY }
    is("b110".U) { result := io.numX < io.numY }
    is("b111".U) { result := io.numX === io.numY }
  }

  var hex8Mod = Module(new Hex8())
  hex8Mod.io.num := result

  io.resultLED := Mux(io.enable, result, 0.U)
  io.resultHex8 := Mux(io.enable, hex8Mod.io.out, 0.U)
}

object Main extends App {
  println("Hello World, I will now generate the Verilog file!")
  emitVerilog(new njualu, Array("--target-dir", "out/verilog"))
}
