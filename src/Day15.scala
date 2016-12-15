import scala.collection.mutable

class Day15 {
  private var discs = mutable.ListBuffer[(Int, Int)]()

  def readInput(file: String) = {
    val input = scala.io.Source.fromFile(file)
    for (line <- input.getLines()) {
      val words = line.split(" ")
      val positions = Integer.parseInt(words(3), 10)
      val startingSpot = Integer.parseInt(words(11).replace(".", ""), 10)
      this.discs.append((positions, startingSpot))
    }
  }

  def findTimeForCapsule(): Int = {
    var time = 0
    var foundMatch = false

    while (!foundMatch) {
      // Count the number of discs that pass
      var numMatches = 0
      for (i <- this.discs.indices) {
        val startingSpot: Int = this.discs(i)._2
        val positions: Int = this.discs(i)._1
        // The time to get to disc i is equal to the number of this disk and the time that has passed
        if ((time + i + 1 + startingSpot) % positions == 0) {
          numMatches += 1
        }
      }

      // If all discs pass, return true, otherwise keep searching for a time to hit the button
      if (numMatches == this.discs.length) {
        foundMatch = true
      }
      else {
        time = time + 1
      }
    }

    time
  }

  def addNewDisc(): Unit = {
    // Add a new disc with 11 positions and starting at position 0
    this.discs.append((11, 0))
  }
}

object Day15Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day15()
    puzzle.readInput(args(0))

    // Part A
    println("Part A: " + puzzle.findTimeForCapsule())

    // Part B
    puzzle.addNewDisc()
    println("Part B: " + puzzle.findTimeForCapsule())
  }
}