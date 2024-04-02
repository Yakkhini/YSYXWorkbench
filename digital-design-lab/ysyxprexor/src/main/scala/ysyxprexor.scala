import chisel3._

class ysyxprexor extends Module {
  val io = IO(new Bundle {
    val a = Input(Bits(1.W))
    val b = Input(Bits(1.W))
    val f = Output(Bits(1.W))
  })

  io.f := io.a ^ io.b
}

object Main extends App {
  println("Hello World, I will now generate the Verilog file!")
  emitVerilog(new ysyxprexor, Array("--target-dir", "out/verilog"))
}
