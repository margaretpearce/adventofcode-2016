import scala.collection.mutable.ArrayBuffer

class Day7 {
  private var numSupportingTLS: Int = 0
  private var numSupportingSSL: Int = 0

  def processInput(fileName: String): Unit = {
    val input = scala.io.Source.fromFile(fileName)
    var countTLS = 0
    var countSSL = 0

    for (line <- input.getLines()) {
      val parsedSequences = this.parseLine(line)

      // Part A
      if (!this.abbaInSequences(parsedSequences._1)) {
        if (this.abbaInSequences(parsedSequences._2)) {
          countTLS = countTLS + 1
        }
      }

      // Part B
      val abaSequences = this.abaInSequences(parsedSequences._1)
      if (abaSequences != null && abaSequences.length > 0) {
        if (this.babInSequences(abaSequences, parsedSequences._2)) {
          countSSL = countSSL + 1
        }
      }
    }
    this.numSupportingTLS = countTLS
    this.numSupportingSSL = countSSL
  }

  def parseLine(line: String): (Array[String], Array[String]) = {
    val inSquareBrackets = ArrayBuffer.empty[String]
    val outsideSquareBrackets = ArrayBuffer.empty[String]

    // Split the line by '['
    val sequences = line.split(Array('['))

    for (sequence <- sequences) {
      // If the sequence contains ']', we have: "pieces0]pieces1"
      if (sequence.contains("]")) {
        val pieces = sequence.split(']')
        inSquareBrackets.append(pieces(0))
        outsideSquareBrackets.append(pieces(1))
      } else {
        outsideSquareBrackets.append(sequence)
      }
    }
    (inSquareBrackets.toArray, outsideSquareBrackets.toArray)
  }

  def abbaInSequences(sequences: Array[String]) : Boolean = {
    for (sequence <- sequences) {
      // Look at four characters at a time
      for (i <- 0 to sequence.length-4) {
        val subsequence = sequence.substring(i,i+4)
        // Skip if the first two characters are the same, e.g. "aa"
        if (!subsequence.charAt(0).equals(subsequence.charAt(1))) {
          // Otherwise, see if the first two characters == second two characters in reverse
          if (subsequence.substring(0,2).equals(subsequence.substring(2).reverse)) {
            return true
          }
        }
      }
    }
    false
  }

  def abaInSequences(sequences: Array[String]) : Array[String] = {
    val abaSequences = ArrayBuffer.empty[String]

    for (sequence <- sequences) {
      for (i <- 0 to sequence.length-3) {
        // Look for all instances of: "x*x" where x is any character and *!=x
        if (sequence.charAt(i).equals(sequence.charAt(i+2)) && !sequence.charAt(i).equals(sequence.charAt(i+1))) {
          abaSequences.append(sequence.substring(i, i+3))
        }
      }
    }
    abaSequences.toArray
  }

  def babInSequences(abaSequences: Array[String], sequences: Array[String]): Boolean = {
    for (aba <- abaSequences) {
      val bab = aba.charAt(1).toString.concat(aba.charAt(0).toString).concat(aba.charAt(1).toString)
      for (sequence <- sequences) {
        if (sequence.contains(bab)) {
          return true
        }
      }
    }
    false
  }

  def getNumSupportingTLS: Int = {
    this.numSupportingTLS
  }

  def getNumSupportingSSL: Int = {
    this.numSupportingSSL
  }
}

object Day7Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day7()
    puzzle.processInput(args(0))
    println(puzzle.getNumSupportingTLS)
    println(puzzle.getNumSupportingSSL)
  }
}
