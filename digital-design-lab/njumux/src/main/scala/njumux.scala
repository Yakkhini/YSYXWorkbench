import chisel3._, chisel3.util._

class njumux extends Module {
  val io = IO(new Bundle {
    val x0 = Input(UInt(2.W))
    val x1 = Input(UInt(2.W))
    val x2 = Input(UInt(2.W))
    val x3 = Input(UInt(2.W))
    val y = Input(UInt(2.W))
    val f = Output(UInt(2.W))

  })

  val result = Wire(UInt(2.W))
  result := 0.U

  switch(io.y) {
    is(0.U) { result := io.x0 }
    is(1.U) { result := io.x1 }
    is(2.U) { result := io.x2 }
    is(3.U) { result := io.x3 }
  }

  io.f := result
}

object Main extends App {
  println("Hello World, I will now generate the Verilog file!")
  emitVerilog(new njumux, Array("--target-dir", "out/verilog"))
}
