package taohe.idu

import chisel3._
import chisel3.util.BitPat
import chisel3.util.experimental.decode.DecodePattern
import chisel3.util.experimental.decode.DecodeField
import chisel3.util.experimental.decode.DecodeTable
import chisel3.util.experimental.decode.BoolDecodeField

import taohe.util.enum._

case class InstructionPattern(
    val instType: InstType.Type,
    val func7: BitPat = BitPat.dontCare(7),
    val func3: BitPat = BitPat.dontCare(3),
    val opcode: BitPat,
    val manual: Boolean = false,
    val manualPrefix: BitPat = BitPat.dontCare(25)
) extends DecodePattern {
  def bitPat: BitPat = pattern

  val genPattern =
    func7 ## BitPat.dontCare(10) ## func3 ## BitPat.dontCare(5) ## opcode
  val manualPattern = manualPrefix ## opcode

  val pattern = if (manual) manualPattern else genPattern

}

object InstTypeField extends DecodeField[InstructionPattern, UInt] {
  def name: String = "instType"
  def chiselType = UInt(InstType.getWidth.W)

  def genTable(op: InstructionPattern): BitPat = BitPat(
    op.instType.litValue.U(InstType.getWidth.W)
  )
}

object ImmField extends DecodeField[InstructionPattern, UInt] {
  def name: String = "immType"
  def chiselType = UInt(ImmType.getWidth.W)

  def genTable(op: InstructionPattern): BitPat = {
    op.instType match {
      // R type doesn't have imm, meaningless
      case InstType.R => BitPat(ImmType.I.litValue.U(ImmType.getWidth.W))
      case InstType.I => BitPat(ImmType.I.litValue.U(ImmType.getWidth.W))
      case InstType.S => BitPat(ImmType.S.litValue.U(ImmType.getWidth.W))
      case InstType.B => BitPat(ImmType.B.litValue.U(ImmType.getWidth.W))
      case InstType.U => BitPat(ImmType.U.litValue.U(ImmType.getWidth.W))
      case InstType.J => BitPat(ImmType.J.litValue.U(ImmType.getWidth.W))
    }
  }

}

object ALUOpField extends DecodeField[InstructionPattern, UInt] {
  def name: String = "aluOpType"
  def chiselType = UInt(ALUOpType.getWidth.W)

  def genTable(op: InstructionPattern): BitPat = {
    op.instType match {
      case InstType.R => {
        op.func3.rawString match {
          case "000" => {
            if (op.func7.rawString == "0000000")
              BitPat(ALUOpType.ADD.litValue.U(ALUOpType.getWidth.W))
            else BitPat(ALUOpType.SUB.litValue.U(ALUOpType.getWidth.W))
          }
          case "001" => BitPat(ALUOpType.SLL.litValue.U(ALUOpType.getWidth.W))
          case "010" => BitPat(ALUOpType.SLT.litValue.U(ALUOpType.getWidth.W))
          case "011" => BitPat(ALUOpType.SLTU.litValue.U(ALUOpType.getWidth.W))
          case "100" => BitPat(ALUOpType.XOR.litValue.U(ALUOpType.getWidth.W))
          case "101" => {
            if (op.func7.rawString == "0000000")
              BitPat(ALUOpType.SRL.litValue.U(ALUOpType.getWidth.W))
            else BitPat(ALUOpType.SRA.litValue.U(ALUOpType.getWidth.W))
          }
          case "110" => BitPat(ALUOpType.OR.litValue.U(ALUOpType.getWidth.W))
          case "111" => BitPat(ALUOpType.AND.litValue.U(ALUOpType.getWidth.W))
          case _: String =>
            BitPat(ALUOpType.ADD.litValue.U(ALUOpType.getWidth.W))
        }
      }
      case InstType.I => {
        op.func3.rawString match {
          case "000" => BitPat(ALUOpType.ADD.litValue.U(ALUOpType.getWidth.W))
          case "010" => BitPat(ALUOpType.SLT.litValue.U(ALUOpType.getWidth.W))
          case "011" => BitPat(ALUOpType.SLTU.litValue.U(ALUOpType.getWidth.W))
          case "100" => BitPat(ALUOpType.XOR.litValue.U(ALUOpType.getWidth.W))
          case "110" => BitPat(ALUOpType.OR.litValue.U(ALUOpType.getWidth.W))
          case "111" => BitPat(ALUOpType.AND.litValue.U(ALUOpType.getWidth.W))
          case "001" => BitPat(ALUOpType.SLL.litValue.U(ALUOpType.getWidth.W))
          case "101" => {
            if (op.func7.rawString == "0000000")
              BitPat(ALUOpType.SRL.litValue.U(ALUOpType.getWidth.W))
            else BitPat(ALUOpType.SRA.litValue.U(ALUOpType.getWidth.W))
          }

          case _: String =>
            BitPat(ALUOpType.ADD.litValue.U(ALUOpType.getWidth.W))
        }
      }

      case _ => BitPat(ALUOpType.ADD.litValue.U(ALUOpType.getWidth.W))

    }
  }
}

object CompareOpField extends DecodeField[InstructionPattern, UInt] {
  def name: String = "compareOpType"
  def chiselType = UInt(CompareOpType.getWidth.W)

  def genTable(op: InstructionPattern): BitPat = {
    op.func3.rawString match {
      case "000" =>
        BitPat(CompareOpType.EQ.litValue.U(CompareOpType.getWidth.W))
      case "001" =>
        BitPat(CompareOpType.NE.litValue.U(CompareOpType.getWidth.W))
      case "100" =>
        BitPat(CompareOpType.LT.litValue.U(CompareOpType.getWidth.W))
      case "101" =>
        BitPat(CompareOpType.GE.litValue.U(CompareOpType.getWidth.W))
      case "110" =>
        BitPat(CompareOpType.LTU.litValue.U(CompareOpType.getWidth.W))
      case "111" =>
        BitPat(CompareOpType.GEU.litValue.U(CompareOpType.getWidth.W))
      case _: String =>
        BitPat(CompareOpType.EQ.litValue.U(CompareOpType.getWidth.W))
    }
  }
}

object Data1Field extends DecodeField[InstructionPattern, UInt] {
  def name: String = "data1Type"
  def chiselType = UInt(Data1Type.getWidth.W)

  def genTable(op: InstructionPattern): BitPat = {
    op.instType match {
      case InstType.R => BitPat(Data1Type.RS1.litValue.U(Data1Type.getWidth.W))
      case InstType.I => BitPat(Data1Type.RS1.litValue.U(Data1Type.getWidth.W))
      case InstType.S => BitPat(Data1Type.RS1.litValue.U(Data1Type.getWidth.W))
      case InstType.B => BitPat(Data1Type.RS1.litValue.U(Data1Type.getWidth.W))
      case InstType.U => {
        if (op.opcode == BitPat("b0010111"))
          BitPat(Data1Type.PC.litValue.U(Data1Type.getWidth.W))
        else BitPat(Data1Type.RS1.litValue.U(Data1Type.getWidth.W))
      }
      case InstType.J => BitPat(Data1Type.PC.litValue.U(Data1Type.getWidth.W))
    }
  }

}

object Data2Field extends DecodeField[InstructionPattern, UInt] {
  def name: String = "data2Type"
  def chiselType = UInt(Data2Type.getWidth.W)
  def genTable(op: InstructionPattern): BitPat = {
    op.instType match {
      case InstType.R => BitPat(Data2Type.RS2.litValue.U(Data2Type.getWidth.W))
      case InstType.I => BitPat(Data2Type.IMM.litValue.U(Data2Type.getWidth.W))
      case InstType.S => BitPat(Data2Type.IMM.litValue.U(Data2Type.getWidth.W))
      case InstType.B => BitPat(Data2Type.RS2.litValue.U(Data2Type.getWidth.W))
      case InstType.U => BitPat(Data2Type.IMM.litValue.U(Data2Type.getWidth.W))
      case InstType.J => BitPat(Data2Type.IMM.litValue.U(Data2Type.getWidth.W))
    }
  }

}

object RegWriteDataTypeField extends DecodeField[InstructionPattern, UInt] {
  def name: String = "regWriteDataType"
  def chiselType = UInt(RegWriteDataType.getWidth.W)
  def genTable(op: InstructionPattern): BitPat = {
    op.instType match {
      case InstType.R =>
        BitPat(RegWriteDataType.RESULT.litValue.U(RegWriteDataType.getWidth.W))
      case InstType.I => {
        op.opcode.rawString match {
          case "1100111" =>
            BitPat(
              RegWriteDataType.NEXTPC.litValue.U(RegWriteDataType.getWidth.W)
            )
          case "1110011" =>
            op.func3.rawString match {
              case "000" =>
                BitPat(
                  RegWriteDataType.RESULT.litValue.U(
                    RegWriteDataType.getWidth.W
                  )
                )
              case _ =>
                BitPat(
                  RegWriteDataType.CSRDATA.litValue.U(
                    RegWriteDataType.getWidth.W
                  )
                )
            }
          case "0000011" =>
            BitPat(
              RegWriteDataType.MEMREAD.litValue.U(RegWriteDataType.getWidth.W)
            )
          case _ =>
            BitPat(
              RegWriteDataType.RESULT.litValue.U(RegWriteDataType.getWidth.W)
            )
        }
      }

      case InstType.J =>
        BitPat(RegWriteDataType.NEXTPC.litValue.U(RegWriteDataType.getWidth.W))

      case _ =>
        BitPat(RegWriteDataType.RESULT.litValue.U(RegWriteDataType.getWidth.W))
    }
  }
}

object NextPCDataTypeField extends DecodeField[InstructionPattern, UInt] {
  def name: String = "nextPCDataType"
  def chiselType = UInt(NextPCDataType.getWidth.W)
  def genTable(op: InstructionPattern): BitPat = {
    op.instType match {
      case InstType.J =>
        BitPat(NextPCDataType.RESULT.litValue.U(NextPCDataType.getWidth.W))
      case InstType.I => {
        op.opcode.rawString match {
          case "1100111" =>
            BitPat(NextPCDataType.RESULT.litValue.U(NextPCDataType.getWidth.W))
          case "1110011" =>
            op.func3.rawString match {
              case "000" => {
                if (op.pattern(31, 20).rawString == "000000000001")
                  BitPat(
                    NextPCDataType.NORMAL.litValue.U(NextPCDataType.getWidth.W)
                  )
                else
                  BitPat(
                    NextPCDataType.CSRDATA.litValue.U(NextPCDataType.getWidth.W)
                  )
              }
              case _ =>
                BitPat(
                  NextPCDataType.NORMAL.litValue.U(NextPCDataType.getWidth.W)
                )
            }
          case _ =>
            BitPat(NextPCDataType.NORMAL.litValue.U(NextPCDataType.getWidth.W))
        }
      }

      case InstType.B =>
        BitPat(NextPCDataType.BRANCH.litValue.U(NextPCDataType.getWidth.W))

      case _ =>
        BitPat(NextPCDataType.NORMAL.litValue.U(NextPCDataType.getWidth.W))
    }
  }
}

object MemLenField extends DecodeField[InstructionPattern, UInt] {
  def name: String = "memoryLenth"
  def chiselType = UInt(MemLen.getWidth.W)
  def genTable(op: InstructionPattern): BitPat = {
    op.func3.rawString match {
      // Maybe we should just use 2 bits for this field? Whatever, espresso will optimize it.
      case "000" => BitPat(MemLen.B.litValue.U(MemLen.getWidth.W))
      case "001" => BitPat(MemLen.H.litValue.U(MemLen.getWidth.W))
      case "010" => BitPat(MemLen.W.litValue.U(MemLen.getWidth.W))
      case "100" => BitPat(MemLen.B.litValue.U(MemLen.getWidth.W)) // LBU
      case "101" => BitPat(MemLen.H.litValue.U(MemLen.getWidth.W)) // LHU
      case _     => BitPat(MemLen.B.litValue.U(MemLen.getWidth.W))
    }
  }
}

object UnsignField extends BoolDecodeField[InstructionPattern] {
  def name: String = "unsign"
  def genTable(op: InstructionPattern): BitPat = {
    // Currently we only care about memory load unsigned
    // Maybe we should support some unsigned ALU operation in the future
    if (op.opcode == BitPat("b0000011") && op.func3(2) == BitPat("b1"))
      BitPat(true.B)
    else BitPat(false.B)
  }
}

object BreakField extends BoolDecodeField[InstructionPattern] {
  def name: String = "break"

  // Only EBREAK has a break signal
  def genTable(op: InstructionPattern): BitPat = {
    if (
      op.pattern == BitPat.N(11) ## BitPat
        .Y(1) ## BitPat.N(13) ## BitPat("b1110011")
    ) BitPat(true.B)
    else BitPat(false.B)
  }
}

object DecodeSupportField extends DecodeField[InstructionPattern, Bool] {
  def name: String = "decodeSupport"
  def chiselType = Bool()
  def genTable(op: InstructionPattern): BitPat = BitPat.Y(1)
  override def default: BitPat = BitPat.N(1)
}

object CSROPTypeField extends DecodeField[InstructionPattern, UInt] {
  def name: String = "csrOperation"
  def chiselType = UInt(CSROPType.getWidth.W)
  def genTable(op: InstructionPattern): BitPat = {
    op.opcode.rawString match {
      case "1110011" =>
        op.func3.rawString match {
          case "000" =>
            op.pattern(31, 20).rawString match {
              case "000000000000" =>
                BitPat(CSROPType.CALL.litValue.U(CSROPType.getWidth.W))
              case "001100000010" =>
                BitPat(CSROPType.RET.litValue.U(CSROPType.getWidth.W))
              case _ => BitPat(CSROPType.NONE.litValue.U(CSROPType.getWidth.W))
            }
          case "001" => BitPat(CSROPType.RW.litValue.U(CSROPType.getWidth.W))
          case "010" => BitPat(CSROPType.RS.litValue.U(CSROPType.getWidth.W))
          case _     => BitPat(CSROPType.NONE.litValue.U(CSROPType.getWidth.W))
        }
      case _ => BitPat(CSROPType.NONE.litValue.U(CSROPType.getWidth.W))
    }
  }
}

object IDUTable {

  val possiblePatterns = Seq(
    InstructionPattern(
      InstType.U,
      opcode = BitPat("b0110111")
    ), // LUI

    InstructionPattern(
      InstType.U,
      opcode = BitPat("b0010111")
    ), // AUIPC

    InstructionPattern(
      InstType.J,
      opcode = BitPat("b1101111")
    ), // JAL

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b000"),
      opcode = BitPat("b1100111")
    ), // JALR

    InstructionPattern(
      InstType.B,
      func3 = BitPat("b000"),
      opcode = BitPat("b1100011")
    ), // BEQ

    InstructionPattern(
      InstType.B,
      func3 = BitPat("b001"),
      opcode = BitPat("b1100011")
    ), // BNE

    InstructionPattern(
      InstType.B,
      func3 = BitPat("b100"),
      opcode = BitPat("b1100011")
    ), // BLT

    InstructionPattern(
      InstType.B,
      func3 = BitPat("b101"),
      opcode = BitPat("b1100011")
    ), // BGE

    InstructionPattern(
      InstType.B,
      func3 = BitPat("b110"),
      opcode = BitPat("b1100011")
    ), // BLTU

    InstructionPattern(
      InstType.B,
      func3 = BitPat("b111"),
      opcode = BitPat("b1100011")
    ), // BGEU

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b000"),
      opcode = BitPat("b0000011")
    ), // LB

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b001"),
      opcode = BitPat("b0000011")
    ), // LH

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b010"),
      opcode = BitPat("b0000011")
    ), // LW

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b100"),
      opcode = BitPat("b0000011")
    ), // LBU

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b101"),
      opcode = BitPat("b0000011")
    ), // LHU

    InstructionPattern(
      InstType.S,
      func3 = BitPat("b000"),
      opcode = BitPat("b0100011")
    ), // SB

    InstructionPattern(
      InstType.S,
      func3 = BitPat("b001"),
      opcode = BitPat("b0100011")
    ), // SH

    InstructionPattern(
      InstType.S,
      func3 = BitPat("b010"),
      opcode = BitPat("b0100011")
    ), // SW

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b000"),
      opcode = BitPat("b0010011")
    ), // ADDI

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b010"),
      opcode = BitPat("b0010011")
    ), // SLTI

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b011"),
      opcode = BitPat("b0010011")
    ), // SLTIU

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b100"),
      opcode = BitPat("b0010011")
    ), // XORI

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b110"),
      opcode = BitPat("b0010011")
    ), // ORI

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b111"),
      opcode = BitPat("b0010011")
    ), // ANDI

    InstructionPattern(
      InstType.I,
      func7 = BitPat("b0000000"),
      func3 = BitPat("b001"),
      opcode = BitPat("b0010011")
    ), // SLLI

    InstructionPattern(
      InstType.I,
      func7 = BitPat("b0000000"),
      func3 = BitPat("b101"),
      opcode = BitPat("b0010011")
    ), // SRLI

    InstructionPattern(
      InstType.I,
      func7 = BitPat("b0100000"),
      func3 = BitPat("b101"),
      opcode = BitPat("b0010011")
    ), // SRAI

    InstructionPattern(
      InstType.R,
      func3 = BitPat("b000"),
      func7 = BitPat("b0000000"),
      opcode = BitPat("b0110011")
    ), // ADD

    InstructionPattern(
      InstType.R,
      func3 = BitPat("b000"),
      func7 = BitPat("b0100000"),
      opcode = BitPat("b0110011")
    ), // SUB

    InstructionPattern(
      InstType.R,
      func3 = BitPat("b001"),
      func7 = BitPat("b0000000"),
      opcode = BitPat("b0110011")
    ), // SLL

    InstructionPattern(
      InstType.R,
      func3 = BitPat("b010"),
      func7 = BitPat("b0000000"),
      opcode = BitPat("b0110011")
    ), // SLT

    InstructionPattern(
      InstType.R,
      func3 = BitPat("b011"),
      func7 = BitPat("b0000000"),
      opcode = BitPat("b0110011")
    ), // SLTU

    InstructionPattern(
      InstType.R,
      func3 = BitPat("b100"),
      func7 = BitPat("b0000000"),
      opcode = BitPat("b0110011")
    ), // XOR

    InstructionPattern(
      InstType.R,
      func3 = BitPat("b101"),
      func7 = BitPat("b0000000"),
      opcode = BitPat("b0110011")
    ), // SRL

    InstructionPattern(
      InstType.R,
      func3 = BitPat("b101"),
      func7 = BitPat("b0100000"),
      opcode = BitPat("b0110011")
    ), // SRA

    InstructionPattern(
      InstType.R,
      func3 = BitPat("b110"),
      func7 = BitPat("b0000000"),
      opcode = BitPat("b0110011")
    ), // OR

    InstructionPattern(
      InstType.R,
      func3 = BitPat("b111"),
      func7 = BitPat("b0000000"),
      opcode = BitPat("b0110011")
    ), // AND

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b000"),
      opcode = BitPat("b1110011"),
      manual = true,
      manualPrefix = BitPat.N(11) ## BitPat.N(1) ## BitPat.N(13)
    ), // ECALL

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b001"),
      opcode = BitPat("b1110011")
    ), // CSRRW

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b010"),
      opcode = BitPat("b1110011")
    ), // CSRRS

    InstructionPattern(
      InstType.I,
      // EBREAK func3 will not use in pattern but useful in fields
      func3 = BitPat("b000"),
      opcode = BitPat("b1110011"),
      manual = true,
      manualPrefix = BitPat.N(11) ## BitPat.Y(1) ## BitPat.N(13)
    ), // EBREAK

    InstructionPattern(
      InstType.I,
      func3 = BitPat("b000"),
      opcode = BitPat("b1110011"),
      manual = true,
      manualPrefix = BitPat("b00110000001") ## BitPat.N(1) ## BitPat.N(13)
    ) // MRET
  )

  val allFields = Seq(
    DecodeSupportField,
    InstTypeField,
    ImmField,
    ALUOpField,
    CompareOpField,
    Data1Field,
    Data2Field,
    RegWriteDataTypeField,
    NextPCDataTypeField,
    MemLenField,
    UnsignField,
    BreakField,
    CSROPTypeField
  )

  val decodeTable = new DecodeTable(possiblePatterns, allFields)
}
