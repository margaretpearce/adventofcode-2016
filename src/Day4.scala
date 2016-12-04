class Day4 {
  private var sectorIdSum: Integer = 0
  private var sectorIdNorthPole: Integer = 0

  def processInput(fileName: String): Unit = {
    val input = scala.io.Source.fromFile(fileName)

    for (line <- input.getLines()) {
      this.processLine(line)
    }
  }

  def processLine(line: String): Unit = {
    // Parse the line
    val lineParts = this.parseLine(line)

    // Get the five most common letters
    val fiveMostCommonLetters = this.getFiveMostCommonLetters(lineParts._1.mkString(""))

    // Determine if the checksum is valid, and increment the counter if so
    this.compareChecksum(lineParts._3, fiveMostCommonLetters, lineParts._2)

    // Shift letters in each name
    this.shiftCipher(lineParts._1, lineParts._2)
  }

  def parseLine(line: String): (Array[String], Integer, String) = {
    val linePieces = line.split(Array('-', '[', ']'))
    val numPieces: Integer = linePieces.length

    val checksum: String = linePieces(numPieces - 1)
    val sectorIdString: String = linePieces(numPieces - 2)
    val sectorId: Integer = Integer.parseInt(sectorIdString, 10)
    val encryptedNames: Array[String] = linePieces.take(numPieces - 2)

    // Return the tuple
    (encryptedNames, sectorId, checksum)
  }

  def getFiveMostCommonLetters(names: String): String = {
    // Count frequency by character (char, freq)
    val letterFrequency = names.groupBy(f => f).map(l => (l._1, l._2.length)).toList

    // Sort by frequency count, then by character
    val sortedLetterFrequency = letterFrequency.sortBy(l => (-l._2, l._1))

    // Get the top five characters and counts
    val firstFive = sortedLetterFrequency.take(5)

    // Get just the characters and join them as a string
    val firstFiveLetters = firstFive.map(l => l._1).mkString("")

    // Return as the computed checksum
    firstFiveLetters
  }

  def compareChecksum(checksumGiven: String, checksumComputed: String, sectorId: Integer): Unit = {
    if (checksumGiven.equalsIgnoreCase(checksumComputed)) {
      this.sectorIdSum = this.sectorIdSum + sectorId
    }
  }

  def getSectorIdSum: Integer = {
    this.sectorIdSum
  }

  def shiftCipher(names: Array[String], sectorId: Int) = {
    val letters = "abcdefghijklmnopqrstuvwxyz"
    var shiftedName = ""

    val shiftBy = sectorId % letters.length

    // Shift each name
    for (nameString <- names) {
      val nameChars = nameString.toCharArray
      for (c <- nameChars) {
        // Find index of c in letters
        val currentIndex = letters.indexOf(c)

        // Find the new index
        val shiftedIndex = (currentIndex + shiftBy) % letters.length

        // Find the new character
        val shiftedChar = letters.charAt(shiftedIndex)

        // Append to the string
        shiftedName = shiftedName.concat(shiftedChar.toString)
      }

      // Join names together with spaces
      shiftedName = shiftedName.concat(" ")
    }

    // Check if this location is north pole object storage
    if (shiftedName.trim().equalsIgnoreCase("northpole object storage")) {
      this.sectorIdNorthPole = sectorId
    }

    // Print the new name (they are very silly)
    println(shiftedName.concat(": ").concat(sectorId.toString))
  }

  def getNorthPoleObjectStorageSectorId = {
    this.sectorIdNorthPole
  }
}

object Day4Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day4()
    puzzle.processInput(args(0))

    // Part 1
    println("Part A: " + puzzle.getSectorIdSum)

    // Part 2
    println("Part B: " + puzzle.getNorthPoleObjectStorageSectorId)
  }
}
