import chisel3._

class ysyxprelight extends Module {
  val io = IO(new Bundle {
    val led = Output(Bits(16.W))
  })

  val cntReg = RegInit(0.U(32.W))
  val ledReg = RegInit(0.U(16.W))

  cntReg := Mux(cntReg >= 5000000.U, 0.U, cntReg + 1.U)

  when(cntReg === 0.U) {
    ledReg := 1.U
  }.otherwise {
    ledReg := ledReg(14, 0) ## ledReg(15)
  }

  io.led := ledReg
}

object Main extends App {
  println("Hello World, I will now generate the Verilog file!")
  emitVerilog(new ysyxprelight, Array("--target-dir", "out/verilog"))
}
