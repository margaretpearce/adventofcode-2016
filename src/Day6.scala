import scala.collection.mutable.ArrayBuffer

class Day6 {
  val codeList = ArrayBuffer.empty[Array[Char]]

  def processInput(fileName: String): Unit = {
    val input = scala.io.Source.fromFile(fileName)

    for (line <- input.getLines()) {
      val charArray = line.toCharArray
      this.codeList.append(charArray)
    }
  }

  def getTranspose: Array[Array[Char]] = {
    val twoDimArray : Array[Array[Char]] = this.codeList.toArray
    twoDimArray.transpose
  }

  def getMessage: String = {
    val swappedArray = this.getTranspose
    var message : String = ""

    for (colArray <- swappedArray) {
      val col: Array[Char] = colArray
      val mostFreq = col.mkString.groupBy(f => f).map(l => (l._1, l._2.length)).toList.sortBy(l => -l._2).take(1)
      message = message.concat(mostFreq.head._1.toString)
    }

    message
  }
}

object Day6Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day6()
    puzzle.processInput(args(0))
    println(puzzle.getMessage)
  }
}
