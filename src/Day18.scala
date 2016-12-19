import scala.collection.mutable.ArrayBuffer

class Day18 {
  private val tiles = ArrayBuffer[ArrayBuffer[Boolean]]()

  def computeMap(input: String, n: Int): Unit = {
    this.readFirstRow(input)
    this.computeNRows(n)
  }

  def readFirstRow(input: String): Unit = {
    val firstRow = ArrayBuffer[Boolean]()
    for (i <- 0 to input.length-1) {
      firstRow.append(input.charAt(i).equals('.'))
    }
    this.tiles.append(firstRow)
  }

  def computeNRows(n: Int): Unit = {
    for (row <- 1 to n-1) { // Only create n-1 rows: we were given the first row
      val newRow = ArrayBuffer[Boolean]()

      // Compute this row by looking at the row above
      for (i <- this.tiles(row-1).indices) {
        val leftTile = if (i > 0) this.tiles(row-1)(i-1) else true
        val rightTile = if (i < this.tiles(row-1).length-1) this.tiles(row-1)(i+1) else true
        val centerTile = this.tiles(row-1)(i)

        var newTile = true

        // Trap: Its left and center tiles are traps, but its right tile is not
        if (!leftTile && !centerTile && rightTile) { newTile = false }

        // Trap: Its center and right tiles are traps, but its left tile is not.
        if (!centerTile && !rightTile && leftTile) { newTile = false }

        // Only its left tile is a trap.
        if (!leftTile && centerTile && rightTile) { newTile = false }

        // Only its right tile is a trap.
        if (!rightTile && leftTile && centerTile) { newTile = false }

        newRow.append(newTile)
      }

      this.tiles.append(newRow)
    }
  }

  def countSafe: Int = {
    this.tiles.map(_.count(_ == true)).sum
  }

  def printMap: Unit = {
    for (i <- this.tiles.indices) {
      for (j <- this.tiles(i).indices) {
        if (this.tiles(i)(j)) print(".") else print("^")
      }
      print("\n")
    }
  }

  def clearMap: Unit = {
    this.tiles.clear()
  }
}

object Day18Puzzle {
  def main(args: Array[String]): Unit = {
    // Part A
    val puzzle = new Day18()
    puzzle.computeMap(args(0), Integer.parseInt(args(1), 10))
    puzzle.printMap
    println("Part a # safe: " + puzzle.countSafe)

    // Part B
    puzzle.clearMap
    puzzle.computeMap(args(0), Integer.parseInt(args(2), 10))
    println("Part b # safe: " + puzzle.countSafe)
  }
}