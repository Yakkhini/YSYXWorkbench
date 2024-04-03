package njukbd

import chisel3._, chisel3.util._

class Hex8Out2 extends Module {
  val io = IO(new Bundle {
    val num = Input(UInt())
    val out = Output(UInt())
  })

  val hex8Mod0 = Module(new Hex8())
  val hex8Mod1 = Module(new Hex8())

  hex8Mod0.io.num := io.num(3, 0)
  hex8Mod1.io.num := io.num(7, 4)

  io.out := hex8Mod1.io.out ## hex8Mod0.io.out

}
