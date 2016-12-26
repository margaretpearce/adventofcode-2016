import scala.collection.mutable
import scala.collection.mutable.HashMap

class State(x:Int, y:Int, costVal:Int, cluster:Array[Array[Int]]) {
  private var cost = costVal

  def getHeuristicCost(): Int = {
    x + y
  }

  def getCost(): Int = {
    cost
  }

  def setCost(newCost:Int): Unit = {
    cost = newCost
  }
}

class Day22 {
  private var usedSpace = HashMap[(Int,Int), Int]()
  private var totalSpace = HashMap[(Int,Int), Int]()
  private var x_max = -1
  private var y_max = -1

  def setNodeInformation(fileName: String): Unit = {
    val nodes = scala.io.Source.fromFile(fileName)

    for (node <- nodes.getLines()) {
      if (node.startsWith("/dev/grid/node")) {
        // Get x, y, size, and used
        val nodeInfo = node.split(" +")

        val xyInfo = nodeInfo(0).split("-")
        val x = Integer.parseInt(xyInfo(1).substring(1), 10)
        val y = Integer.parseInt(xyInfo(2).substring(1), 10)

        // Track max x and y
        if (x > this.x_max) this.x_max = x
        if (y > this.y_max) this.y_max = y

        val size = Integer.parseInt(nodeInfo(1).trim().split("T")(0), 10)
        val used = Integer.parseInt(nodeInfo(2).trim().split("T")(0), 10)

        this.usedSpace.put((x,y), used)
        this.totalSpace.put((x,y), size)
      }
    }
  }

  def getNumberOfNodes(): Int = {
    this.totalSpace.size
  }

  def countViablePairs(): Int = {
    var numPairs: Int = 0

    for (a <- this.usedSpace.keySet) {
      for (b <- this.usedSpace.keySet) {
        if (this.usedSpace(a) != 0 && a != b && (this.usedSpace(a) <= (this.totalSpace(b) - this.usedSpace(b)))) {
          numPairs += 1
        }
      }
    }

    numPairs
  }

  def getArrayState(): Array[Array[Int]] = {
    val array = Array.ofDim[Int](this.x_max, this.y_max)
    for (x <- 0 to this.x_max) {
      for (y <- 0 to this.y_max) {
        array(x)(y) = this.usedSpace(x,y)
      }
    }
    array
  }

  def stateIsGoal(state: Array[Array[Int]]): Boolean = {
    state.map(i => i.sum).sum == state(0)(0)
  }

  def getStateHeuristic(state: Array[Array[Int]]): Int = {
    state.map(i => i.count(p => p > 0)).sum
  }

  def getNextStates(state: Array[Array[Int]]): Array[Array[Array[Int]]] = {
    null
  }

  def searchAStar: Int = {
    // Set of nodes already evaluated
    val closedSet = mutable.ArrayBuffer[Array[Array[Int]]]()

    // Keep states in priority queue with the smallest estimated costs appearing first
    implicit def orderedState(f: (Int, Array[Array[Int]])): Ordered[(Int, Array[Array[Int]])] =
      new Ordered[(Int, Array[Array[Int]])] {
        def compare(other: (Int, Array[Array[Int]])) = -1 * f._1.compare(other._1)
      }

    // Set of currently discovered states still to be evaluated
    val openSet = mutable.PriorityQueue.empty[(Int, Array[Array[Int]])]
    val initialState = this.getArrayState()
    val initialHeuristic = this.getStateHeuristic(initialState)
    openSet.enqueue((initialHeuristic, initialState))

    // Previous steps to the current "best" state
    val cameFrom = mutable.Map[Array[Array[Int]], Array[Array[Int]]]()

    // Costs of getting to each node (known lowest)
    val currentCost = mutable.HashMap[Array[Array[Int]], Int]()
    currentCost.put(initialState, 0)

    // Heuristic cost of getting from this node to the goal state
    val estimatedCost = mutable.HashMap[Array[Array[Int]], Int]()
    estimatedCost.put(initialState, initialHeuristic)

    // Search loop
    while (openSet.nonEmpty) {
      // Get the state with the lowest estimated score
      val stateTuple = openSet.dequeue()
      val state = stateTuple._2
      val cost = stateTuple._1

      if (this.stateIsGoal(state)) {
//        this.endState = state.copyState()
//        this.printPath(cameFrom)
//        openSet.clear()
        return cost
      }

      closedSet.append(state)
      val nextStates = this.getNextStates(state)

      for (nextState <- nextStates) {
        if (!closedSet.contains(nextState)) {
          val newScore = cost + 1

          if (!currentCost.contains(nextState) ||
            (currentCost.contains(nextState) && newScore < currentCost(nextState))) {
            currentCost.put(nextState, newScore)
            cameFrom.put(nextState, state)
            estimatedCost.put(nextState, this.getStateHeuristic(nextState))

//            nextState.setCurrentScore(newScoreEstimate)
//            cameFrom.put(nextState, state)
//            nextState.setEstimatedScore(nextState.getCurrentScore() + nextState.estimateHeuristic())
          }

          if (openSet.filter(p => p._2.equals(nextState)).isEmpty) {
            openSet.enqueue((estimatedCost(nextState), nextState))
          }

//          if (!openSet.contains(nextState)) {
//            openSet.enqueue((nextState.getEstimatedScore(), nextState))
//          }
        }
      }
    }

    // Failure case
    -1
  }
}

object Day22Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day22()
    puzzle.setNodeInformation(args(0))
    println("# nodes: " + puzzle.getNumberOfNodes())
    println("# viable pairs: " + puzzle.countViablePairs())
  }
}