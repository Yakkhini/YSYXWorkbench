import chisel3._, chisel3.util._

class njushift extends Module {
  val io = IO(new Bundle {
    val trigger = Input(Bool())
    val seed = Input(UInt(16.W))
    val outData = Output(UInt(16.W))
    val dataHex8 = Output(UInt(32.W))
  })

  def risingedge(x: Bool) = x && !RegNext(x)

  val shiftReg = RegInit(0.U(16.W))
  val hex8Mod0 = Module(new Hex8())
  val hex8Mod1 = Module(new Hex8())
  val hex8Mod2 = Module(new Hex8())
  val hex8Mod3 = Module(new Hex8())

  val newBit = Wire(UInt())

  newBit := shiftReg(3) ^ shiftReg(2) ^ shiftReg(1) ^ shiftReg(0)

  when(risingedge(io.trigger)) {
    when(shiftReg === 0.U) {
      shiftReg := io.seed
    }.otherwise {
      shiftReg := newBit ## shiftReg(15, 1)
    }
  }

  hex8Mod0.io.num := shiftReg(3, 0)
  hex8Mod1.io.num := shiftReg(7, 4)
  hex8Mod2.io.num := shiftReg(11, 8)
  hex8Mod3.io.num := shiftReg(15, 12)

  io.dataHex8 := hex8Mod3.io.out ## hex8Mod2.io.out ## hex8Mod1.io.out ## hex8Mod0.io.out

  io.outData := shiftReg
}

object Main extends App {
  println("Hello World, I will now generate the Verilog file!")
  emitVerilog(new njushift, Array("--target-dir", "out/verilog"))
}
