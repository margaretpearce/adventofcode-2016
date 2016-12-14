class Day13 (favoriteNumber: Int) {
  private var destinationX = -1
  private var destinationY = -1
  private var currentX = 1
  private var currentY = 1

  def isCoordinateWall(x: Int, y: Int): Boolean = {
    // Find x*x + 3*x + 2*x*y + y + y*y
    var formula = (x*x) + (3*x) + (2*x*y) + y + (y*y)

    // Add your puzzle input
    formula = formula + favoriteNumber

    // Find the binary representation
    val binaryRep = Integer.toBinaryString(formula)

    // Count the number of bits that are one
    val numberBitsOne = binaryRep.count(b => b == '1')

    // If the # of bits that are 1 is odd, it's a wall, else it's open space
    numberBitsOne % 2 == 1
  }

  def printGrid(xLimit: Int, yLimit: Int): Unit = {
    println("  0123456789")
    for (y <- 0 to yLimit) {
      print(y + " ")
      for (x <- 0 to xLimit) {
        if (this.isCoordinateWall(x,y)) {
          print("#")
        }  else {
          print(".")
        }
      }
      print("\n")
    }
  }

  def getShortestPathToLocation: Int = {
    // TODO
    0
  }
}

object Day13Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day13(Integer.parseInt(args(0), 10))
    println(puzzle.isCoordinateWall(0,0))
    puzzle.printGrid(9, 6)
  }
}