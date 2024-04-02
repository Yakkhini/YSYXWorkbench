import chisel3._, chisel3.util._

class njuencoder extends Module {
  val io = IO(new Bundle {
    val oneHot8 = Input(UInt(8.W))
    val enable = Input(Bool())
    val ledNum = Output(UInt(3.W))
    val ledEN = Output(Bool())
    val hex8Num = Output(UInt(8.W))
  })

  val Hex8 = Module(new Hex8())
  val resultNum = Wire(UInt())

  resultNum := 0.U
  io.ledEN := false.B

  when(io.enable) {
    io.ledEN := true.B
    switch(io.oneHot8) {
      is("b00000000".U) { io.ledEN := false.B }
      is("b00000001".U) { resultNum := 0.U }
      is("b00000010".U) { resultNum := 1.U }
      is("b00000100".U) { resultNum := 2.U }
      is("b00001000".U) { resultNum := 3.U }
      is("b00010000".U) { resultNum := 4.U }
      is("b00100000".U) { resultNum := 5.U }
      is("b01000000".U) { resultNum := 6.U }
      is("b10000000".U) { resultNum := 7.U }
    }
  }

  Hex8.io.num := resultNum
  io.ledNum := Mux(io.ledEN, resultNum, 0.U)
  io.hex8Num := Mux(io.ledEN, Hex8.io.out, 0.U)

}

object Main extends App {
  println("Hello World, I will now generate the Verilog file!")
  emitVerilog(new njuencoder, Array("--target-dir", "out/verilog"))
}
