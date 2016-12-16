import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer, Map}

class FloorState {
  private var floor = Map[Int, ArrayBuffer[Int]]()
  private var currentScore = Int.MaxValue
  private var estimatedScore = Int.MaxValue
  private var elevator = 1

  def getNextStates(): List[FloorState] = {
    val nextStates = ListBuffer[FloorState]()

    // Can we move up?
    if (this.elevator < 4) {
      val statesMovingUp: List[FloorState] = moveUpStates()
      nextStates.appendAll(statesMovingUp)
    }

    // Can we move down?
    if (this.elevator > 1) {
      val statesMovingDown: List[FloorState] = moveDownStates()
      nextStates.appendAll(statesMovingDown)
    }

    nextStates.toList
  }

  def moveUpStates(): List[FloorState] = {
    val currentFloorItems = this.floor(elevator).toArray
    val nextStates = mutable.ListBuffer[FloorState]()

    // Move one up
    for (item <- currentFloorItems) {
      val moveOneUpState = this.copyState()
      val indexToRemove = moveOneUpState.floor(this.elevator).indexOf(item)

      moveOneUpState.floor(this.elevator).remove(indexToRemove)
      moveOneUpState.floor(this.elevator + 1).append(item)
      moveOneUpState.setElevator(this.elevator + 1)

      // Move two up
      for (secondItem <- currentFloorItems) {
        if (item != secondItem) {
          var moveTwoUpState = moveOneUpState.copyState()
          var indexToRemove = moveTwoUpState.floor(this.elevator).indexOf(secondItem)
          moveTwoUpState.floor(this.elevator).remove(indexToRemove)
          moveTwoUpState.floor(this.elevator + 1).append(secondItem)

          if (moveTwoUpState.stateIsValid()) {
            nextStates.append(moveTwoUpState)
          }
        }
      }

      if (moveOneUpState.stateIsValid()) {
        nextStates.append(moveOneUpState)
      }
    }
    nextStates.toList
  }

  def moveDownStates(): List[FloorState] = {
    val currentFloorItems = this.floor(elevator).toArray
    val nextStates = mutable.ListBuffer[FloorState]()

    // Move one down
    for (item <- currentFloorItems) {
      val moveOneDownState = this.copyState()
      val indexToRemove = moveOneDownState.floor(this.elevator).indexOf(item)
      moveOneDownState.floor(this.elevator).remove(indexToRemove)
      moveOneDownState.floor(this.elevator - 1).append(item)
      moveOneDownState.setElevator(this.elevator - 1)

      if (moveOneDownState.stateIsValid()) {
        nextStates.append(moveOneDownState)
      }

//      // Move two down
//      for (secondItem <- currentFloorItems) {
//        if (item != secondItem) {
//          val moveTwoDownState = moveOneDownState.copyState()
//          val indexToRemove = moveTwoDownState.floor(this.elevator).indexOf(secondItem)
//          moveTwoDownState.floor(this.elevator).remove(indexToRemove)
//          moveTwoDownState.floor(this.elevator - 1).append(secondItem)
//
//          if (moveTwoDownState.stateIsValid()) {
//            nextStates.append(moveTwoDownState)
//          }
//        }
//      }
    }

    nextStates.toList
  }

  def copyState(): FloorState = {
    var newState = new FloorState()
    newState.setElevator(this.elevator)

    val newFloor = mutable.Map[Int, ArrayBuffer[Int]]()
    newFloor.put(1, this.floor(1).map(identity))
    newFloor.put(2, this.floor(2).map(identity))
    newFloor.put(3, this.floor(3).map(identity))
    newFloor.put(4, this.floor(4).map(identity))

    newState.setFloor(newFloor)

    newState
  }

  def estimateHeuristic(): Int = {
    3 * this.floor(1).length + 2 * this.floor(2).length + this.floor(3).length
  }

  def stateIsValid(): Boolean = {
    for (key <- 4 to 1 by -1) {
      val floorItems = this.floor(key).toArray
      val generators = floorItems.filter(f => f > 0)
      val chips = floorItems.filter(f => f < 0)

      // Chips must be connected to their generators if they are on the same floor as a generator
      for (chip <- chips) {
        if (!generators.contains(-1 * chip) && generators.length != 0) {
          return false
        }
      }
    }

    true
  }

  def printState(): String = {
    var stateString = ""
    for (key <- 4 to 1 by -1) {
      val floorItems = this.floor(key).toArray.mkString(" ")
      stateString = stateString + "F" + key + " " + floorItems + "\n"
    }
    stateString
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
      if (!this.floor(4).contains(-1 * element)) {
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

  override def equals(that: Any): Boolean =
    that match {
      case that: FloorState =>
        this.currentScore == that.currentScore &&
          this.estimatedScore == that.estimatedScore &&
          this.elevator == that.elevator &&
          this.floor(1).sorted == that.floor(1).sorted &&
          this.floor(2).sorted == that.floor(2).sorted &&
          this.floor(3).sorted == that.floor(3).sorted &&
          this.floor(4).sorted == that.floor(4).sorted
      case _ => false
    }
}

class Day11 {
  // Puzzle input: the initial layout
  //private val floorInitial = mutable.Map[Int, ArrayBuffer[Int]]()
  private val startState = new FloorState()
  private var endState = new FloorState()

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
      for (i <- 5 to pieces.length - 1) {
        if (pieces(i).contains("generator")) {
          // Get generators
          val generatorName = pieces(i - 1)

          if (!this.generatorNames.contains(generatorName)) {
            this.numGeneratorTypes = this.numGeneratorTypes + 1
            this.generatorNamesMapping.put(generatorName, this.numGeneratorTypes)
            this.generatorNames.append(generatorName)

            this.numMicrochipTypes = this.numMicrochipTypes + 1
            this.microchipNamesMapping.put(generatorName.concat("-compatible"), -1 * this.numGeneratorTypes)
            this.microchipNames.append(generatorName.concat("-compatible"))
          }

          val generatorNum: Int = this.generatorNamesMapping(generatorName)
          floorArray.append(generatorNum)
        }
        else if (pieces(i).contains("microchip")) {
          // Get microchips
          val microchip = pieces(i - 1)
          val compatibleGenerator = microchip.split("-")(0)

          // Find corresponding generator and generator # (microchip # is -1*compatible generator number)
          if (!this.generatorNames.contains(compatibleGenerator)) {
            this.numGeneratorTypes = this.numGeneratorTypes + 1
            this.generatorNamesMapping.put(compatibleGenerator, this.numGeneratorTypes)
            this.generatorNames.append(compatibleGenerator)

            this.numMicrochipTypes = this.numMicrochipTypes + 1
            this.microchipNamesMapping.put(microchip, -1 * this.numGeneratorTypes)
            this.microchipNames.append(microchip)
          }

          val microchipNum: Int = this.microchipNamesMapping(microchip)
          floorArray.append(microchipNum)
        }
      }
      floorInitial.put(floor, floorArray)
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
    val closedSet = mutable.ListBuffer[FloorState]()

    // Keep states in priority queue with the smallest estimated costs appearing first
    implicit def orderedState(f: (Int, FloorState)): Ordered[(Int, FloorState)] = new Ordered[(Int, FloorState)] {
      def compare(other: (Int, FloorState)) = -1 * f._1.compare(other._1)
    }

    // Set of currently discovered states still to be evaluated
    val openSet = mutable.PriorityQueue.empty[(Int, FloorState)]
    openSet.enqueue((this.startState.getEstimatedScore(), this.startState))

    // Previous steps to the current "best" state
    val cameFrom = mutable.Map[FloorState, FloorState]()

    while (openSet.nonEmpty) {
      // Get the state with the lowest estimated score
      val stateTuple = openSet.dequeue()
      val state = stateTuple._2

      if (stateTuple._1 == 33) {
        println("hit 33")
      }

      if (state.isGoal()) {
        this.endState = state
        this.printPath(cameFrom)
        return stateTuple._1
      }

      closedSet.append(state)
      val nextStates = state.getNextStates()

      for (nextState <- nextStates) {
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

  def printPath(steps: Map[FloorState, FloorState]): Unit = {
    var sequence: String = this.endState.printState()
    var previousState = this.endState
    while (steps.contains(previousState)) {
      sequence = steps(previousState).printState() + "\n" + sequence
      previousState = steps(previousState)
    }
    println(sequence)
  }
}

object Day11Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day11()
    puzzle.processInput(args(0))

    // Part A
    val numMoves = puzzle.searchAStar
    println("Moves for part A: " + numMoves)

    // Part B
    val puzzlePartB = new Day11()
    puzzlePartB.processInput(args(1))
    val numMovesPartB = puzzlePartB.searchAStar
    println("Moves for part B: " + numMovesPartB)
  }
}