import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer, scala.collection.mutable.Map

class FloorState {
  private var floor = Map[Int, ArrayBuffer[Int]]()
  private var currentScore = Int.MaxValue
  private var estimatedScore = Int.MaxValue
  private var elevator = 1

  def getNextStates(): List[FloorState] = {
    val nextStates = mutable.ListBuffer[FloorState]()
    
    // Move one up

    // Move two up

    // Move one down

    // Move two down

    nextStates.toList
  }

  def estimateHeuristic(): Int = {
    return 3*this.floor(1).length + 2*this.floor(2).length + this.floor(3).length
  }

  def stateIsValid(): Boolean = {
    for (key <- 4 to 1 by -1){
      val floorItems = this.floor(key).toArray
      val generators = floorItems.filter(f => f > 0)
      val chips = floorItems.filter(f => f < 0)

      // Chips must be connected to their generators if they are on the same floor as a generator
      for (chip <- chips) {
        if (!generators.contains(-1*chip) && generators.length != 0) {
          return false
        }
      }
    }

    true
  }

  def printState(): Unit = {
    for (key <- 4 to 1 by -1){
      val floorItems = this.floor(key).toArray.mkString(" ")
      println("F" + key + " " + floorItems)
    }
  }

  def getFloor(): Map[Int, ArrayBuffer[Int]] = {
    this.floor
  }

  def getCurrentScore(): Int = {
    this.currentScore
  }

  def getEstimatedScore(): Int = {
    this.estimatedScore
  }

  def getElevator(): Int = {
    this.elevator
  }

  def setFloor(layout: Map[Int, ArrayBuffer[Int]]): Unit = {
    this.floor = layout
  }

  def setCurrentScore(score: Int) = {
    this.currentScore = score
  }

  def setEstimatedScore(score: Int) = {
    this.estimatedScore = score
  }

  def setElevator(floorNum: Int) = {
    this.elevator = floorNum
  }

  def isGoal(): Boolean = {
    // All elements should be on the fourth floor
    for (element <- this.floor(4)) {
      if (!this.floor(4).contains(-1*element)) {
        return false
      }
    }

    // The other floors should be empty
    if (this.floor(3).nonEmpty || this.floor(2).nonEmpty || this.floor(1).nonEmpty) {
      return false
    }

    // If all conditions met, this is the goal state
    true
  }
}

class Day11 {
  // Puzzle input: the initial layout
  //private val floorInitial = mutable.Map[Int, ArrayBuffer[Int]]()
  private val startState = new FloorState()

  // String names of the microchips/ generators
  private val microchipNames = ArrayBuffer[String]()
  private val generatorNames = ArrayBuffer[String]()

  // Map names to the integer representation used in this program
  private val microchipNamesMapping = mutable.Map[String, Int]()
  private val generatorNamesMapping = mutable.Map[String, Int]()

  // Number of microchip types
  private var numMicrochipTypes = 0
  private var numGeneratorTypes = 0

  def processInput(fileName: String): Unit = {
    val input = scala.io.Source.fromFile(fileName)
    val floorInitial = mutable.Map[Int, ArrayBuffer[Int]]()

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

      val floorArray = ArrayBuffer.empty[Int]

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

        floorInitial.put(floor, floorArray)
      }
    }

    // Save this floor configuration
    this.startState.setFloor(floorInitial)
    this.startState.setCurrentScore(0)
    this.startState.setEstimatedScore(startState.estimateHeuristic())

    // Print the starting floor configuration
    this.startState.printState()
  }

  def searchAStar: Int = {
    // Set of nodes already evaluated
    var closedSet = mutable.ListBuffer[FloorState]()

    // Keep states in priority queue with the smallest estimated costs appearing first
    implicit def orderedState(f: Tuple2[Int, FloorState]): Ordered[(Int, FloorState)] = new Ordered[(Int, FloorState)] {
      def compare(other: Tuple2[Int, FloorState]) = -1*f._1.compare(other._1)
    }

    // Set of currently discovered states still to be evaluated
    var openSet = mutable.PriorityQueue.empty[Tuple2[Int, FloorState]]
    openSet.enqueue((this.startState.getEstimatedScore(), this.startState))

    // Previous steps to the current "best" state
    var cameFrom = mutable.Map[FloorState, FloorState]()

    while (openSet.nonEmpty) {
      // Get the state with the lowest estimated score
      val stateTuple = openSet.dequeue()
      val state = stateTuple._2

      if (state.isGoal()) {
        return stateTuple._1
      }

      closedSet.append(state)

      for (nextState <- state.getNextStates()) {
        if (!closedSet.contains(nextState)) {
          val newScoreEstimate = state.getCurrentScore() + 1

          if (newScoreEstimate < nextState.getCurrentScore()) {
            nextState.setCurrentScore(newScoreEstimate)
            cameFrom.put(nextState, state)
            nextState.setEstimatedScore(nextState.getCurrentScore() + nextState.estimateHeuristic())
          }

          if (!openSet.toList.contains(nextState)) {
            openSet.enqueue((nextState.getEstimatedScore(), nextState))
          }
        }
      }
    }

    // Failure case
    -1
  }
}

object Day11Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day11()
    puzzle.processInput(args(0))
  }
}