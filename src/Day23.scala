import scala.collection.mutable.ArrayBuffer

class Day23 {
  var instructions = ArrayBuffer.empty[String]
  var registers: Array[Long] = Array(0,0,0,0)

  def getInput(fileName: String): Unit = {
    val input = scala.io.Source.fromFile(fileName)
    this.instructions = this.instructions ++ input.getLines().toArray
  }

  def processInstructions = {
    var i = 0
    while (i < this.instructions.length) {
      val instruction = this.instructions(i).split(" ")

      if (instruction(0).equals("cpy")) {
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
          i = i + Integer.parseInt(instruction(2), 10)
        } else {
          i = i + 1
        }
      }
    }
  }

  def getIndex(registerName: String): Int = {
    val characters = "abcd"
    characters.indexOf(registerName)
  }
}

object Day23Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day23()
    puzzle.getInput(args(0))
    puzzle.processInstructions
  }
}
