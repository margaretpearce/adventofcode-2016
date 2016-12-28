import scala.collection.mutable

class Day22 {
  private val usedSpace = mutable.HashMap[(Int,Int), Int]()
  private val totalSpace = mutable.HashMap[(Int,Int), Int]()
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

  def getNumberOfNodes: Int = {
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

  def printGrid = {
    val nodeToMove = (this.x_max, 0)
    val emptyNode = this.findEmptyNode()
    val largeNodeCutoffSize = this.totalSpace(emptyNode)

    for (y <- 0 to this.y_max) {
      for (x <- 0 to this.x_max) {
        val used = this.usedSpace((x,y))

        if (x == 0 && y == 0) {
          print("X")
        }
        else if ((x,y) == nodeToMove) {
          print("G")
        }
        else if ((x,y) == emptyNode) {
          print("_")
        }
        else if (used <= largeNodeCutoffSize) {
          print(".")
        }
        else {
          print("#")
        }
      }
      print("\n")
    }
  }

  def findEmptyNode(): (Int,Int) = {
    val nodes = this.usedSpace.keySet.toList

    for (node <- nodes) {
      if (this.usedSpace(node) == 0) {
        return node
      }
    }

    (-1,-1)
  }

  def findWall(): List[(Int,Int)] = {
    val wallList = mutable.ListBuffer[(Int,Int)]()
    val emptyNode = this.findEmptyNode()

    for (y <- 0 to this.y_max) {
      for (x <- 0 to this.x_max) {
        val used = this.usedSpace((x,y))
        if (used > this.totalSpace(emptyNode)) wallList.append((x,y))
      }
    }

    wallList.toList
  }

  def moveTopRightToTopLeft(): Int = {
    // Empty node that can be used to swap data
    val emptyNode = this.findEmptyNode()

    // Wall nodes that we need to move around
    val wallNodes = this.findWall()
    val wallXMin = wallNodes.map(p => p._1).min
    val wallYMax = wallNodes.map(p => p._2).max

    // Move the empty node to the row below the wall
    val moveUpToWall = emptyNode._2 - wallYMax - 1

    // Move to the left wall node and then over one more so we can move up
    val moveLeftOfWall = emptyNode._1 - wallXMin + 1

    // Move up to the wall, then up to the top, then right to the goal
    val moveToTarget = 1 + wallYMax + (this.x_max - wallXMin + 1)

    // To move the target to the left, need to do a sequence of 5 moves around the target
    val moveTargetToGoal = 5*(this.x_max-1)

    moveUpToWall + moveLeftOfWall + moveToTarget + moveTargetToGoal
  }
}

object Day22Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day22()
    puzzle.setNodeInformation(args(0))

    // Part A
    println("# nodes: " + puzzle.getNumberOfNodes)
    println("# viable pairs: " + puzzle.countViablePairs())

    // Part B
    println("# moves target to goal: " + puzzle.moveTopRightToTopLeft())
    println("\nGrid:")
    println(puzzle.printGrid)
  }
}