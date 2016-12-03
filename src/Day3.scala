class Day3 {
  def processInput(fileName: String): Integer = {
    val input = scala.io.Source.fromFile(fileName)
    var numPossible: Int = 0

    for (line <- input.getLines()) {
      // Get the sides from this line
      val sides = line.trim().split(" +")
      val side1 = Integer.parseInt(sides(0).trim, 10)
      val side2 = Integer.parseInt(sides(1).trim, 10)
      val side3 = Integer.parseInt(sides(2).trim, 10)

      // Check if this is a valid triangle
      if (checkPossible(side1, side2, side3)) {
        numPossible = numPossible + 1
      }
    }

    numPossible
  }

  def processInputPartB(fileName: String): Integer = {
    val input = scala.io.Source.fromFile(fileName)
    var numPossible: Int = 0

    // Save column-wise results as arrays
    val column1 : Array[Int] = Array(0,0,0)
    val column2 : Array[Int] = Array(0,0,0)
    val column3 : Array[Int] = Array(0,0,0)

    // Track hour many columns have been read in so far
    var count = 0

    for (line <- input.getLines()) {
      // Get the sides from this line
      val sides = line.trim().split(" +")
      val side1 = Integer.parseInt(sides(0).trim, 10)
      val side2 = Integer.parseInt(sides(1).trim, 10)
      val side3 = Integer.parseInt(sides(2).trim, 10)

      // Save the side information for each column
      column1(count) = side1
      column2(count) = side2
      column3(count) = side3

      count = count + 1

      // Check if we've read three columns and are ready to test the triangles
      if (count == 3) {
        // Check triangle formed by column 1
        if (checkPossible(column1(0), column1(1), column1(2))) {
          numPossible = numPossible + 1
        }

        // Check triangle formed by column 2
        if (checkPossible(column2(0), column2(1), column2(2))) {
          numPossible = numPossible + 1
        }

        // Check triangle formed by column 3
        if (checkPossible(column3(0), column3(1), column3(2))) {
          numPossible = numPossible + 1
        }

        // Reset the counter
        count = 0
      }
    }

    numPossible
  }

  def checkPossible(side1: Int, side2: Int, side3: Int): Boolean = {
    if (side1 + side2 <= side3)
      false
    else if (side1 + side3 <= side2)
      false
    else if (side2 + side3 <= side1)
      false
    else
      true
  }
}

object Day3Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day3()

    // Part 1
    val numPossible = puzzle.processInput(args(0))
    println(numPossible)

    // Part 2
    val partBNumPossible = puzzle.processInputPartB(args(0))
    println(partBNumPossible)
  }
}