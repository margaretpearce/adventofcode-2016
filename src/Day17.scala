import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter._
import scala.collection.mutable

class Route(path: String = "", distance: Int = Int.MaxValue, x: Int = 0, y: Int = 0) {
  private var pathString = path
  private var pathDistance = distance
  private var currentX = x
  private var currentY = y

  def getPath(): String = {
    this.pathString
  }

  def getDistance(): Int = {
    this.pathDistance
  }

  def getLocation(): (Int, Int) = {
    (this.currentX, this.currentY)
  }

  def updatePath(path: String, distance: Int, x: Int, y: Int) = {
    this.pathString = path
    this.pathDistance = distance
    this.currentX = x
    this.currentY = y
  }
}

class Day17 (puzzleInput:String) {
  private val doorOpen = mutable.Map[(Int,Int), (Int,Int)]()
  private val width = 4
  private val height = 4

  def findPathFromInput(input: String): Route = {
    val nodeDistance = mutable.Map[(Int,Int), Route]()
    val parent = mutable.Map[Route, Route]()
    val queue = mutable.Queue[Route]()

    // Add starting node to the queue
    val root = new Route(distance = 0)
    nodeDistance.put(root.getLocation(), root)
    queue.enqueue(root)

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      for ((neighbor, direction) <- this.getNeighbors(current)) {
        // Get the current distance to the node (default to "inf" if we haven't seen this node yet)
        var neighborDistance = Int.MaxValue

        // If we have seen this node before, look up its current best distance
        if (nodeDistance.contains(neighbor)) {
          val neighborRoute = nodeDistance(neighbor)
          neighborDistance = neighborRoute.getDistance()
        }

        // Check if we've found a better path
        if (neighborDistance == Int.MaxValue) {
          neighborDistance = current.getDistance() + 1
          val updatedPath = current.getPath().concat(direction)
          val updatedNeighbor = new Route(path = updatedPath, distance = neighborDistance, x = neighbor._1, y = neighbor._2)

          // Set distance and parent
          nodeDistance.put(neighbor, updatedNeighbor)
          parent.put(updatedNeighbor, current)

          // Add neighbor to the queue
          queue.enqueue(updatedNeighbor)
        }
      }
    }

    // Get the route that leads to the end state
    nodeDistance((3,3))
  }

  def getNeighbors(route: Route): List[((Int,Int), String)] = {
    // Form string to hash (puzzle input + current path)
    val hashInput = new StringBuilder
    hashInput ++= puzzleInput
    hashInput ++= route.getPath()

    // Get hash output
    val hashOutput = this.md5Hex(hashInput.toString)
    val startLocation = route.getLocation()

    val neighbors = mutable.ListBuffer[((Int,Int), String)]()
    val doorIsOpenCodes = "bcdef"

    // up
    if (doorIsOpenCodes.contains(hashOutput.charAt(0))) {
      if (startLocation._2 > 0) {
        val up = (startLocation._1, startLocation._2 - 1)
        val path = route.getPath().concat("U")
        neighbors.append((up, path))
      }
    }

    // down
    if (doorIsOpenCodes.contains(hashOutput.charAt(1))) {
      if (startLocation._2 < this.height-1) {
        val down = (startLocation._1, startLocation._2 + 1)
        val path = route.getPath().concat("D")
        neighbors.append((down, path))
      }
    }

    // left
    if (doorIsOpenCodes.contains(hashOutput.charAt(2))) {
      if (startLocation._1 > 0) {
        val left = (startLocation._1 - 1, startLocation._2)
        val path = route.getPath().concat("L")
        neighbors.append((left, path))
      }
    }

    // right
    if (doorIsOpenCodes.contains(hashOutput.charAt(3))) {
      if (startLocation._1 < this.width-1) {
        val right = (startLocation._1 + 1, startLocation._2)
        val path = route.getPath().concat("R")
        neighbors.append((right, path))
      }
    }

    neighbors.toList
  }

  def md5Hex(s: String): String = {
    // Get MD5 hash as a string of lowercase hex digits (four characters only)
    printHexBinary(MessageDigest.getInstance("MD5").digest(s.getBytes)).take(4).toLowerCase
  }

}

object Day17Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day17(args(0))


  }
}