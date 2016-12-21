class Day21(unscrambled: String, fileName: String) {
  private var buildScrambled: StringBuilder = new StringBuilder(unscrambled)

  def processInstructions: Unit = {
    val instructions = scala.io.Source.fromFile(fileName)

    for (instruction <- instructions.getLines()) {
      val i = instruction.split(" ")

      if (i(0).equalsIgnoreCase("move")) {
        val x = i(2).toInt
        val y = i(5).toInt
        this.move(x,y)
      }
      else if (i(0).equalsIgnoreCase("swap")) {
        if (i(1).equalsIgnoreCase("position")) {
          val x = i(2).toInt
          val y = i(5).toInt
          this.swapPosition(x,y)
        }
        else if (i(1).equalsIgnoreCase("letter")) {
          val x = i(2).charAt(0)
          val y = i(5).charAt(0)
          this.swapLetter(y,x)
        }
      }
      else if (i(0).equalsIgnoreCase("rotate")) {
        if (i(1).equalsIgnoreCase("based")) {
          val letter = i.last.charAt(0)
          this.rotateByLetterPosition(letter)
        }
        else if (i(1).equalsIgnoreCase("left") || i(1).equalsIgnoreCase("right")) {
          val steps = i(2).toInt
          val rotateLeft = i(1).equalsIgnoreCase("left")
          this.rotate(rotateLeft, steps)
        }
      }
      else if (i(0).equalsIgnoreCase("reverse")) {
        val x = i(2).toInt
        val y = i(4).toInt
        this.reverse(x,y)
      }

      println(this.buildScrambled)
    }
  }

  def move(from:Int, to:Int): Unit = {
    // Remove the character at position X
    val charToMove = this.buildScrambled.charAt(from)
    this.buildScrambled.deleteCharAt(from)

    // Insert it to end up at position Y
    this.buildScrambled.insert(to, charToMove)
  }

  def rotate(rotateLeft:Boolean, numSteps:Int) = {
    // ABCDEF
    // Rotate left 2: CDEFAB
    // = substring(2) + substring(0,2)
    // Rotate right 2: EFABCD
    // = substring(-2) + substring(0,-2)
    val numRotations = numSteps % this.buildScrambled.length

    if (rotateLeft) {
      this.buildScrambled
        .append(this.buildScrambled.subSequence(0,numRotations))
        .replace(0,numRotations,"")
    } else {
      this.buildScrambled.insert(0, this.buildScrambled.takeRight(numRotations))
      .setLength(this.buildScrambled.length-numRotations)
    }
  }

  def rotateByLetterPosition(letter: Char) = {
    // Find the index of this letter
    val index = this.buildScrambled.indexOf(letter)

    // Rotate one time plus a number equal to the index of the letter
    var numSteps = 1 + index

    // Rotate one additional time if the index was at least 4
    if (index >= 4) {
      numSteps += 1
    }

    this.rotate(false, numSteps)
  }

  def swapLetter(firstLetter:Char, secondLetter:Char) = {
    val swappedString = this.buildScrambled.toString()
      .replace(firstLetter, '-')
      .replace(secondLetter, firstLetter)
      .replace('-', secondLetter)

    this.buildScrambled.clear()
    this.buildScrambled ++= swappedString
  }

  def swapPosition(from:Int, to:Int) = {
    val charAtPositionFrom = this.buildScrambled.charAt(from)
    val charAtPositionTo = this.buildScrambled.charAt(to)

    // Replace char at "from" with "to" char
    this.buildScrambled.deleteCharAt(from)
    this.buildScrambled.insert(from, charAtPositionTo)

    // Replace char at "to" with "from" char
    this.buildScrambled.deleteCharAt(to)
    this.buildScrambled.insert(to, charAtPositionFrom)
  }

  def reverse(from:Int, to:Int) = {
    var reversedSubstring = this.buildScrambled.substring(from, to+1).reverse
    this.buildScrambled.delete(from, to+1)
    this.buildScrambled.insert(from, reversedSubstring)
  }

  def getScrambledPassword: String = {
    this.buildScrambled.toString()
  }
}

object Day21Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day21(args(0), args(1))
    puzzle.processInstructions

    // Part A
    val scrambled = puzzle.getScrambledPassword
    println("Part A: " + scrambled)
  }
}