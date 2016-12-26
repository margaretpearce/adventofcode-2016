import scala.collection.mutable.ArrayBuffer

class Day25(fileName: String) {
  private var instructions = ArrayBuffer.empty[String]
  private var registers: Array[Long] = Array(0,0,0,0)
  private var clockSignal = new StringBuilder()

  def setInput: Unit = {
    val input = scala.io.Source.fromFile(fileName)
    this.instructions = this.instructions ++ input.getLines().toArray
  }

  def resetSafe(): Unit = {
    this.registers = Array(0,0,0,0)
    this.instructions.clear()
    this.clockSignal.clear()
  }

  def setEggs(numEggs: Int): Unit = {
    this.registers(0) = numEggs
  }

  def processInstructions = {
    var i = 0

    while (i < this.instructions.length && this.clockSignal.length < 10) {
      val instruction = this.instructions(i).split(" ")

      // Optimization: speed up multiplication on lines 4-9 (necessary for part b)
      if (instruction(0).equals("tgl")) {
        val registerIndex = this.getIndex(instruction(1))

        if (registerIndex != -1) {
          val instructionIndex = i + this.registers(registerIndex).toInt

          // Check if the instruction to toggle is valid
          if (instructionIndex >= 0 && instructionIndex < this.instructions.length) {
            val instructionToToggle = this.instructions(instructionIndex).split(" ")

            // Switch out the instructions at this index (arguments unaffected)
            if (instructionToToggle(0).equals("inc")) {
              instructionToToggle(0) = "dec"
            } else if (instructionToToggle(0).equals("dec") || instructionToToggle(0).equals("tgl")) {
              instructionToToggle(0) = "inc"
            } else if (instructionToToggle(0).equals("jnz")) {
              instructionToToggle(0) = "cpy"
            } else if (instructionToToggle(0).equals("cpy")) {
              instructionToToggle(0) = "jnz"
            }

            this.instructions(instructionIndex) = instructionToToggle.mkString(" ")
          }
        }
        i = i + 1
      }
      else if (instruction(0).equals("cpy")) {
        val copyIntoIndex = this.getIndex(instruction(2))
        val copyFrom = this.getIndex(instruction(1))

        // If an integer is provided, copy the integer into the register
        if (copyFrom == -1) {
          this.registers(copyIntoIndex) = Integer.parseInt(instruction(1), 10)
        }
        // Otherwise, copy the value from the register
        else {
          this.registers(copyIntoIndex) = this.registers(copyFrom)
        }

        i = i + 1
      }
      else if (instruction(0).equals("inc")) {
        // Get the register and add one to it
        val registerIndex = this.getIndex(instruction(1))
        this.registers(registerIndex) = this.registers(registerIndex) + 1
        i = i + 1
      }
      else if (instruction(0).equals("dec")) {
        // Get the register and substract one from it
        val registerIndex = this.getIndex(instruction(1))
        this.registers(registerIndex) = this.registers(registerIndex) - 1
        i = i + 1
      }
      else if (instruction(0).equals("jnz")) {
        // Check if the integer OR register value is not 0
        val xIndex = this.getIndex(instruction(1))
        val xIsZero = (xIndex == -1 && instruction(1).equals("0")) || (xIndex != -1 && this.registers(xIndex) == 0)

        // If the second parameter is not zero, jump forward or backward the specified number of steps
        if (!xIsZero) {
          val yIndex = this.getIndex(instruction(2))

          if (yIndex != -1) {
            i = i + this.registers(yIndex).toInt
          } else {
            i = i + Integer.parseInt(instruction(2), 10)
          }
        } else {
          i = i + 1
        }
      }
      else if (instruction(0).equals("out")) {
        val registerIndex = this.getIndex(instruction(1))

        if (registerIndex != -1) {
          this.clockSignal ++= this.registers(registerIndex).toString
        } else {
          this.clockSignal ++= instruction(1)
        }

        i = i + 1
      }
    }
  }

  def runExperiment(): Int = {
    var i = 1
    val stringToMatch = "0101010101"

    do {
      this.resetSafe()
      this.setInput
      this.setEggs(i)
      this.processInstructions
      i += 1
    }
    while(!stringToMatch.equals(this.clockSignal.toString()))

    i-1
  }

  def getIndex(registerName: String): Int = {
    val characters = "abcd"
    characters.indexOf(registerName)
  }

  def printRegisterValues = {
    for (register <- this.registers.indices) {
      val registerName = "abcd".charAt(register)
      println(registerName + ": " + this.registers(register))
    }
  }
}

object Day25Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day25(args(0))

    // Part A
    println("Part A:")
    val smallestI = puzzle.runExperiment()
    println("i = " + smallestI)
  }
}
