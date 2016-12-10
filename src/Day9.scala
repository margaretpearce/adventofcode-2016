class Day9 {
  private var decompressedLength: Int = 0
  private var decompressedLengthPartB: Long = 0

  def processInput(fileName: String): Unit = {
    val input = scala.io.Source.fromFile(fileName)

    for (line <- input.getLines()) {
      val transformedLine = line.trim.toLowerCase
      this.decompressedLength = this.decompressedLength + this.getLengthOfDecompressedLine(transformedLine)
      this.decompressedLengthPartB = this.decompressedLengthPartB +
        this.getLengthOfDecompressedLinePartB(transformedLine)
    }
  }

  def getLengthOfDecompressedLine(compressed: String): Int = {
    var decompressedLineLength: Int = 0
    var i: Int = 0

    // Read each line character by character
    while (i < compressed.length) {
      if (compressed.charAt(i).equals('(')) {
        val marker = compressed.substring(i + 1, compressed.substring(i + 1).indexOf(')') + i + 1)
        val numChars = Integer.parseInt(marker.split('x')(0), 10)
        val numRepeats = Integer.parseInt(marker.split('x')(1), 10)
        decompressedLineLength = decompressedLineLength + (numChars * numRepeats)

        // Move the index ahead (past the markers and parentheses) and past the characters to repeat
        i = i + numChars + 2 + marker.length
      } else {
        // No decompression needed, add 1 to length and move to next character
        decompressedLineLength = decompressedLineLength + 1
        i = i + 1
      }
    }

    decompressedLineLength
  }

  def getLengthOfDecompressedLinePartB(compressed: String): Long = {
    var decompressedLineLength: Long = 0
    var i: Int = 0

    // Read each line character by character
    while (i < compressed.length) {
      if (compressed.charAt(i).equals('(')) {
        val endMarker = compressed.substring(i + 1).indexOf(')') + i + 1
        val marker = compressed.substring(i + 1, endMarker)
        val numChars = Integer.parseInt(marker.split('x')(0), 10)
        val numRepeats = Integer.parseInt(marker.split('x')(1), 10)
        val repeatedString = compressed.substring(endMarker + 1, endMarker + 1 + numChars)

        // Recursively call this function to handle nested markers
        decompressedLineLength = decompressedLineLength +
          (numRepeats * this.getLengthOfDecompressedLinePartB(repeatedString))

        // Move the index ahead (past the markers and parentheses) and past the characters to repeat
        i = i + numChars + 2 + marker.length
      } else {
        // No decompression needed, add 1 to length and move to next character
        decompressedLineLength = decompressedLineLength + 1
        i = i + 1
      }
    }

    decompressedLineLength
  }

  def getDecompressedLength: Int = {
    this.decompressedLength
  }

  def getDecompressedLengthPartB: Long = {
    this.decompressedLengthPartB
  }
}

object Day9Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day9()
    puzzle.processInput(args(0))
    println(puzzle.getDecompressedLength)
    println(puzzle.getDecompressedLengthPartB)
  }
}
