class Day16 {

  def fillDisc(input: String, targetLength: Int): String = {
    // Start with initial state (puzzle input)
    var disc: StringBuilder = new StringBuilder
    disc ++= input

    while (disc.length < targetLength) {
      var b = disc.reverse.toString().replace('1', 't').replace('0', '1').replace('t', '0')
      disc += '0'
      disc ++= b
    }

    // Once the data has been generated, create a checksum ONLY for data that fits
    val checksumData = disc.substring(0, targetLength)
    this.createChecksum(checksumData)
  }

  def createChecksum(data: String): String = {
    var checksum: StringBuilder = new StringBuilder

    // Consider every pair (every set of 2 characters in a row)
    for (i <- 0 to data.length-2 by 2) {
      val pair = data.substring(i, i + 2)
      if (pair.charAt(0) == pair.charAt(1)) {
        checksum += '1'
      } else {
        checksum += '0'
      }
    }

    while (checksum.length % 2 == 0) {
      val newChecksum = this.createChecksum(checksum.toString)
      checksum.clear()
      checksum ++= newChecksum
    }

    checksum.toString()
  }
}

object Day16Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day16()

    // Part A
    val checksum = puzzle.fillDisc(args(0), Integer.parseInt(args(1), 10))
    println("Checksum (part A): " + checksum)

    // Part B
    val checksumPartB = puzzle.fillDisc(args(0), Integer.parseInt(args(2), 10))
    println("Checksum (part B): " + checksumPartB)
  }
}