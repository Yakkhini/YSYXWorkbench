import chisel3._, chisel3.util._

class Hex8 extends Module {
  val io = IO(new Bundle {
    val num = Input(UInt())
    val out = Output(UInt())
  })

  val result = Wire(UInt())
  result := 0.U

  // wire [7:0] segs [7:0];
  // assign segs[0] = 8'b11111101;
  // assign segs[1] = 8'b01100000;
  // assign segs[2] = 8'b11011010;
  // assign segs[3] = 8'b11110010;
  // assign segs[4] = 8'b01100110;
  // assign segs[5] = 8'b10110110;
  // assign segs[6] = 8'b10111110;
  // assign segs[7] = 8'b11100000;
  // reverse

  switch(io.num) {
    is(0.U) { result := "b00000010".U }
    is(1.U) { result := "b10011111".U }
    is(2.U) { result := "b00100100".U }
    is(3.U) { result := "b00001100".U }
    is(4.U) { result := "b10011001".U }
    is(5.U) { result := "b01001001".U }
    is(6.U) { result := "b01000001".U }
    is(7.U) { result := "b00011111".U }
    is(8.U) { result := "b00000001".U }
    is(9.U) { result := "b00001001".U }
    is(10.U) { result := "b00010001".U }
    is(11.U) { result := "b11000001".U }
    is(12.U) { result := "b01100011".U }
    is(13.U) { result := "b10000101".U }
    is(14.U) { result := "b01100001".U }
    is(15.U) { result := "b01110001".U }
  }

  io.out := result

}
