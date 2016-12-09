class Day8 {
  private val width: Int = 50
  private val height: Int = 6
  private val screen = Array.fill[Array[Boolean]](this.height)(Array.fill[Boolean](this.width)(false))

  def processInput(fileName: String): Unit = {
    val input = scala.io.Source.fromFile(fileName)

    for (line <- input.getLines()) {
      // For visualizing the pixel movement: print the instruction and the screen
      // this.printScreen
      // println(line)

      this.parseInstruction(line)
    }
  }

  def parseInstruction(instruction: String): Unit = {
    val pieces = instruction.split(" ")

    if (pieces(0).equalsIgnoreCase("rotate")) {
      // Get the starting point and number of rotations
      val location: Int = Integer.parseInt(pieces(2).split("=")(1), 10)
      val numRotations: Int = Integer.parseInt(pieces(4), 10)

      // Column rotation
      if (pieces(1).equalsIgnoreCase("column")) {
        this.rotateCol(location, numRotations)
      }
      // Row rotation
      else if (pieces(1).equalsIgnoreCase("row")) {
        this.rotateRow(location, numRotations)
      }
    }
    else if (pieces(0).equalsIgnoreCase("rect")) {
      // Get the dimensions of the rectangle
      val dimension = pieces(1).split("x")
      val numRows = Integer.parseInt(dimension(1), 10)
      val numCols = Integer.parseInt(dimension(0), 10)

      // Turn on all pixels in this rectangle (starting at the top right corner)
      this.rectangle(numRows, numCols)
    }
  }

  def rotateRow(rowNumber: Int, numRotations: Int): Unit = {
    // Rotate the row array
    this.screen(rowNumber) = this.rotateRight(this.screen(rowNumber), this.width, numRotations)
  }

  def rotateCol(colNumber: Int, numRotations: Int): Unit = {
    // Get the column from the 2d array
    val column: Array[Boolean] = this.screen.map{_(colNumber)}

    // Rotate the column array
    val returnedColumn: Array[Boolean] = this.rotateRight(column, this.height, numRotations)

    // Put the column back in place
    for (rowIndex <- 0 to this.height-1) {
      this.screen(rowIndex)(colNumber) = returnedColumn(rowIndex)
    }
  }

  def rotateRight(pixels: Array[Boolean], size: Int, rotations: Int): Array[Boolean] = {
    pixels.drop(size - (rotations % size)) ++ pixels.take(size - (rotations % size))
  }

  def rectangle(numRows: Int, numCols: Int): Unit = {
    for (y <- 0 to numRows-1) {
      for (x <- 0 to numCols-1) {
        this.screen(y)(x) = true
      }
    }
  }

  def numberPixelsOn: Int = {
    var count: Int = 0
    for (y <- 0 to this.height-1) {
      for (x <- 0 to this.width-1) {
        if (this.screen(y)(x)) {
          count = count + 1
        }
      }
    }
    count
  }

  def printScreen(emptyChar: String = ".") = {
    for (y <- 0 to this.height-1) {
      for (x <- 0 to this.width-1) {
        if (this.screen(y)(x)) {
          print("#")
        } else {
          print(emptyChar)
        }
      }
      print("\n")
    }
    print("\n\n")
  }

  def printScreenLetters = {
    this.printScreen(" ")
  }
}

object Day8Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day8()
    puzzle.processInput(args(0))
    println(puzzle.numberPixelsOn)
    puzzle.printScreenLetters
  }
}
