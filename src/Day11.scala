import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer, scala.collection.mutable.Map

class Day11 {
  private val floorInitial = mutable.Map[Int, ArrayBuffer[Int]]()
  private val microchipNames = ArrayBuffer[String]()
  private val generatorNames = ArrayBuffer[String]()
  private val microchipNamesMapping = mutable.Map[String, Int]()
  private val generatorNamesMapping = mutable.Map[String, Int]()
  private var numMicrochipTypes = 0
  private var numGeneratorTypes = 0

  def processInput(fileName: String): Unit = {
    val input = scala.io.Source.fromFile(fileName)

    for (line <- input.getLines()) {
      val pieces = line.split(" ")

      // Get floor #
      var floor: Int = 0
      if (pieces(1).equals("first")) {
        floor = 1
      } else if (pieces(1).equals("second")) {
        floor = 2
      } else if (pieces(1).equals("third")) {
        floor = 3
      } else if (pieces(1).equals("fourth")) {
        floor = 4
      }

      val floorArray:ArrayBuffer[Int] = ArrayBuffer.empty[Int]

      // Get items on this floor
      for (i <- 5 to pieces.length-1) {
        if (pieces(i).contains("generator")) {
          // Get generators
          val generatorName = pieces(i-1)

          if (!this.generatorNames.contains(generatorName)) {
            this.numGeneratorTypes = this.numGeneratorTypes + 1
            this.generatorNamesMapping.put(generatorName, this.numGeneratorTypes)
          }

          val generatorNum:Int = this.generatorNamesMapping(generatorName)
          floorArray.append(generatorNum)
        }
        else if (pieces(i).contains("microchip")) {
          // Get microchips
          val microchip = pieces(i-1)
          val compatibleGenerator = microchip.split("-")(0)

          // Find corresponding generator and generator # (microchip # is -1*compatible generator number)
          if (!this.generatorNames.contains(compatibleGenerator)) {
            this.numGeneratorTypes = this.numGeneratorTypes + 1
            this.numMicrochipTypes = this.numMicrochipTypes + 1
            this.generatorNamesMapping.put(compatibleGenerator, this.numGeneratorTypes)
            this.microchipNamesMapping.put(microchip, -1*this.numGeneratorTypes)
            this.generatorNames.append(compatibleGenerator)
            this.microchipNames.append(microchip)
          }

          val microchipNum:Int = this.microchipNamesMapping(microchip)
          floorArray.append(microchipNum)
        }

        // Save this floor configuration
        this.floorInitial.put(floor, floorArray)
      }
    }
  }

  def printConfiguration: Unit = {
    for (key <- 4 to 1 by -1){
      val floorItems = this.floorInitial(key).toArray.mkString(" ")
      println("F" + key + " " + floorItems)
    }
  }

  def searchAStar: Int = {
    // A* search
    // Return minimal # of moves
    0
  }

  def generateNextMoves: Array[Array[Array[Int]]] = {
    val nextMoveCandidates = ArrayBuffer[Array[Array[Int]]]()

    // Try making moves on all floors
    for (floor <- 1 to 4) {

      // Try moving all items
      for (item <- 0 to 4) {
        val nextMoveCandidate = ArrayBuffer[Array[Int]]()

        // Only add to the list if it can't be pruned
        if (!this.pruneMove(nextMoveCandidate.toArray)) {
          nextMoveCandidates.append(nextMoveCandidate.toArray)
        }
      }
    }
    nextMoveCandidates.toArray
  }

  def pruneMove(move: Array[Array[Int]]): Boolean = {
    true
  }

  def stateMatchesGoal(state: Array[Array[Int]]) : Boolean = {
    true
  }
}

object Day11Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day11()
    puzzle.processInput(args(0))
    puzzle.printConfiguration
  }
}