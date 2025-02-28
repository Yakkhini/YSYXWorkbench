package taohe

import chisel3._

import taohe.util.RegisterFileBundle

class RegisterFile extends Module {
  val io = IO(new RegisterFileBundle)

  val registers = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

  when(
    io.fromEXU.bits.writeEnable && io.fromEXU.bits.writeAddr =/= 0.U && io.fromEXU.valid
  ) {
    registers(io.fromEXU.bits.writeAddr) := io.fromEXU.bits.writeData
  }

  io.toIDU.bits.readData1 := registers(io.fromIDU.bits.readAddr1)
  io.toIDU.bits.readData2 := registers(io.fromIDU.bits.readAddr2)

  io.toIDU.valid := true.B
  io.fromIDU.ready := true.B
  io.fromEXU.ready := true.B
  io.fromEXU.ready := true.B
}
