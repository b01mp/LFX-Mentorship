package stack

import chisel3.stage.ChiselStage
import java.nio.file.Paths

// Your code starts here

// Importing these Libraries for all the Chisel Definitions and utilities
import chisel3._
import chisel3.util._

// the main StackModule Class which does the push, pop, peek ops
// Accepts paramterized  data width, *dataWidth* and stack depth, *len*
class StackModule(val dataWidth: Int, val len: Int) extends Module {
  
  // I/O declaration as per the specification
  val io = IO(new Bundle {
    val in        = Input(UInt(32.W))             
    val out       = Output(UInt(dataWidth.W))
    val underflow = Output(Bool())
    val overflow  = Output(Bool())
    val isEmpty   = Output(Bool())
    val isFull    = Output(Bool())
    val popped    = Output(Bool())
    val peeked    = Output(Bool())
  })
  
  // Stack Storage
  val stack = Reg(Vec(len, UInt(dataWidth.W)))      // the actual memory of the stack
  val depth = RegInit(0.U(log2Ceil(len + 1).W))     // Holds the number of elements in the stack
  
  // Instruction decoding
  val cmd = io.in(6, 0)       // opcode for push, pop, peek
  val data = io.in(31, 7)     // any immediate value to push gets stored in this
  
  // Lookup values for push, pop, peek opcodes
  val (isPush, isPop, isPeek) = (
    cmd === "b0100111".U,
    cmd === "b1000011".U, 
    cmd === "b1000000".U
  )
  
  // Data handling
  val pushData = Wire(UInt(dataWidth.W))
  if (dataWidth >= 25) {
    pushData := Cat(0.U((dataWidth-25).W), data)
  } else {
    pushData := data(dataWidth-1, 0)
  }
  
  // Stack Operations 
  val stackFull = depth === len.U
  val stackEmpty = depth === 0.U
  
  // Output Data Logic
  // Calculates the top index and data before any depth modifications
  val topIdx = depth - 1.U
  val topData = Mux(stackEmpty, 0.U, stack(topIdx))
  
  // Output uses the current state before any modifications
  val outputReg = RegInit(0.U(dataWidth.W))
  when(isPop || isPeek) {
      outputReg := topData
    }.otherwise {
      outputReg := 0.U
    }
    io.out := outputReg

  
  // Stack operations happen after output calculation
  when(isPush && !stackFull) {
    stack(depth) := pushData
    depth := depth + 1.U
  }.elsewhen(isPop && !stackEmpty) {
    depth := depth - 1.U
  }
  // Peek does not modify any state
  io.isEmpty := stackEmpty
  io.isFull := stackFull
  
  // Only these are registered for clean pulse generation
  io.underflow := RegNext((isPop || isPeek) && stackEmpty, false.B)
  io.overflow := RegNext(isPush && stackFull, false.B)
  io.popped := RegNext(isPop && !stackEmpty, false.B)
  io.peeked := RegNext(isPeek && !stackEmpty, false.B)
}

// Your code ends here

object SVGen extends App {
  val out = Paths.get(
    "out",
    this.getClass
      .getName
      .stripSuffix("$")
  ).toString
  new ChiselStage().emitSystemVerilog(
    new StackModule(args(0).toInt, args(1).toInt),
    Array("--target-dir", out),
  )
}