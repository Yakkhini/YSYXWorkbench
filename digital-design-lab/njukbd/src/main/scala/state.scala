package njukbd

import chisel3._, chisel3.util._

object KeyboardState extends ChiselEnum {
  val kbdWait, kbdSample, kbdReady = Value
}
