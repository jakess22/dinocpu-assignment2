// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu

import chisel3._
import chisel3.util._
import dinocpu.components._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.4 of Patterson and Hennessy
 * This follows figure 4.21
 */
class SingleCycleCPU(implicit val conf: CPUConfig) extends BaseCPU {
  // All of the structures required
  val pc              = dontTouch(RegInit(0.U(64.W)))
  val control         = Module(new Control())
  val registers       = Module(new RegisterFile())
  val aluControl      = Module(new ALUControl())
  val alu             = Module(new ALU())
  val immGen          = Module(new ImmediateGenerator())
  val controlTransfer = Module(new ControlTransferUnit())
  val (cycleCount, _) = Counter(true.B, 1 << 30)

  control.io := DontCare
  registers.io := DontCare
  aluControl.io := DontCare
  alu.io := DontCare
  immGen.io := DontCare
  controlTransfer.io := DontCare
  io.dmem <> DontCare

  //FETCH
  io.imem.address := pc
  io.imem.valid := true.B

  val instruction = Wire(UInt(32.W))
  when ((pc % 8.U) === 4.U) {
    instruction := io.imem.instruction(63, 32)
  } .otherwise {
    instruction := io.imem.instruction(31, 0)
  }
  
  // Your code goes here
  // Data Memory
  io.dmem.address := alu.io.result
  when (control.io.memop === 1.U || control.io.memop === 2.U) {
    io.dmem.valid := 1.U
  } .otherwise {
    io.dmem.valid := 0.U
  }
  io.dmem.writedata := registers.io.readdata2

  when (instruction(6,0) === "b0000011".U) { // ld
    io.dmem.memread := 1.U
    io.dmem.maskmode := 3.U
    io.dmem.sext := 1.U
    io.dmem.memwrite := 0.U
  } .elsewhen (instruction(6,0) === "b0100011".U) { // sd
    io.dmem.memread := 0.U
    io.dmem.maskmode := 3.U
    io.dmem.sext := 0.U
    io.dmem.memwrite := 1.U
  } .otherwise {
    io.dmem.memread := 0.U
    io.dmem.maskmode := 0.U
    io.dmem.sext := 0.U
    io.dmem.memwrite := 0.U
  }
  
  // Control Transfer Unit
  // fix me: wire controltransferop
  controlTransfer.io.pc := pc 
  controlTransfer.io.funct3 := instruction(6, 0)
  controlTransfer.io.operand1 := registers.io.readdata1
  controlTransfer.io.operand2 := registers.io.readdata2 
  controlTransfer.io.imm := immGen.io.sextImm

  // PC
  pc := controlTransfer.io.nextpc

  // immGen
  immGen.io.instruction := instruction

  // Control Unit
  control.io.opcode := instruction(6, 0)

  // RF
  registers.io.readreg1 := instruction(19, 15)
  registers.io.readreg2 := instruction(24, 20)
  registers.io.writereg := instruction(11, 7)
  // prevent writing to reg0
  when (registers.io.writereg === 0.U) {
    registers.io.wen := false.B
  } .otherwise {
    registers.io.wen := control.io.writeback_valid
  }
  when (control.io.writeback_src === 0.U) {
    registers.io.writedata := alu.io.result
  } .elsewhen (control.io.writeback_src === 1.U) {
    registers.io.writedata := immGen.io.sextImm
  } .elsewhen (control.io.writeback_src === 2.U) {
    registers.io.writedata := io.dmem.readdata
  }
  

  // ALU Control
  aluControl.io.aluop := control.io.aluop
  aluControl.io.funct7 := instruction(31, 25)
  aluControl.io.funct3 := instruction(14, 12)

  // ALU
  when (instruction(6,0) === "b0000011".U) { // ld 
    alu.io.operation := "b01100".U // ADDI
  } .elsewhen (instruction(6,0) === "b0100011".U) { // sd 
    alu.io.operation := "b01100".U // ADDI
  } .elsewhen (instruction(6,0) === "b0010111".U) { // auipc
    alu.io.operation := "b01100".U // ADDI
  } .otherwise {
    alu.io.operation := aluControl.io.operation
  }

  when (control.io.op1_src === 0.U) {
    alu.io.operand1 := registers.io.readdata1
  } .elsewhen (control.io.op1_src === 1.U) {
    alu.io.operand1 := pc
  }

  when (control.io.op2_src === 0.U) { // RF readdata2
    alu.io.operand2 := registers.io.readdata2
  } .elsewhen (control.io.op2_src === 2.U || control.io.op2_src === 4.U) { // immediate
    alu.io.operand2 := immGen.io.sextImm 
  }
}

/*
 * Object to make it easier to print information about the CPU
 */
object SingleCycleCPUInfo {
  def getModules(): List[String] = {
    List(
      "dmem",
      "imem",
      "control",
      "registers",
      "csr",
      "aluControl",
      "alu",
      "immGen",
      "controlTransfer"
    )
  }
}
