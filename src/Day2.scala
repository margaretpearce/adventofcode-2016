class Day2(partB: Boolean = false) {
  val keypad: Array[Array[Int]] = Array(
    Array(1, 2, 3),
    Array(4, 5, 6),
    Array(7, 8, 9)
  )

  val keypad_partb: Array[Array[Char]] = Array(
    Array(' ', ' ', '1', ' ', ' '),
    Array(' ', '2', '3', '4', ' '),
    Array('5', '6', '7', '8', '9'),
    Array(' ', 'A', 'B', 'C', ' '),
    Array(' ', ' ', 'D', ' ', ' ')
  )

  var code: String = ""

  // Start at 5: (1,1) is the index of element with value 5 in keypad
  var x: Int = 1
  var y: Int = 1

  if (partB) {
    // (2,0) is the index of the element with value 5 in part B's keypad
    x = 0
    y = 2
  }

  def processInput(fileName: String): String = {
    val input = scala.io.Source.fromFile(fileName)
    var button: String = ""

    // Process each line one at a time
    for (line <- input.getLines()) {
      // Each line represents a button to hit on the keypad
      if (partB) {
        button = this.getButtonPartB(line.trim())
      } else {
        button = this.getButton(line.trim())
      }

      this.code = this.code.concat(button)
    }

    // After processing all of the instructions, the code variable is populated
    this.code
  }

  def getButton(instruction: String): String = {
    // Read the instruction one character at a time
    val moves = instruction.toCharArray

    // If a move doesn't lead to a button, ignore it
    for (move <- moves) {
      if (move == 'U') {
        this.y = Math.max(0, this.y - 1)
      } else if (move == 'D') {
        this.y = Math.min(2, this.y + 1)
      } else if (move == 'L') {
        this.x = Math.max(0, this.x - 1)
      } else if (move == 'R') {
        this.x = Math.min(2, this.x + 1)
      }
    }

    // Press whatever button you're on at the end of each line (note y=row, x=col in the array lookup)
    this.keypad(this.y)(this.x).toString
  }

  def getButtonPartB(instruction: String): String = {
    // Read the instruction one character at a time
    val moves = instruction.toCharArray

    // If a move doesn't lead to a button, ignore it
    for (move <- moves) {
      if (move == 'U') {
        val new_y = Math.max(0, this.y - 1)

        // Check if the move is valid
        if (this.keypad_partb(new_y)(this.x) != ' ') {
          this.y = new_y
        }
      } else if (move == 'D') {
        val new_y = Math.min(4, this.y + 1)

        if (this.keypad_partb(new_y)(this.x) != ' ') {
          this.y = new_y
        }
      } else if (move == 'L') {
        val new_x = Math.max(0, this.x - 1)

        if (this.keypad_partb(this.y)(new_x) != ' ') {
          this.x = new_x
        }
      } else if (move == 'R') {
        val new_x = Math.min(4, this.x + 1)

        if (this.keypad_partb(this.y)(new_x) != ' ') {
          this.x = new_x
        }
      }
    }

    // Press whatever button you're on at the end of each line (note y=row, x=col in the array lookup)
    this.keypad_partb(this.y)(this.x).toString
  }
}

object Day2Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day2(args(1).toBoolean)
    val code = puzzle.processInput(args(0))
    println(code)
  }
}