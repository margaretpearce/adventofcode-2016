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

class Day17(puzzleInput: String) {
  private val width = 4
  private val height = 4

  def getShortestPathToLocation: Route = {
    // Create Route object representing the source
    val root = new Route(distance = 0)
    var longestRoute = root

    // Add to priority queue (with smallest items appearing first in the queue)
    implicit def orderedState(f: (Int, Route)): Ordered[(Int, Route)] = new Ordered[(Int, Route)] {
      def compare(other: (Int, Route)) = -1 * f._1.compare(other._1)
    }

    val queue = mutable.PriorityQueue.empty[(Int, Route)]
    queue.enqueue((root.getDistance(), root))

    while (queue.nonEmpty) {
      // Get route with shortest path (starts with source)
      val node_u: (Int, Route) = queue.dequeue()

      // Check if we have found the destination
      if (node_u._2.getLocation() ==(3, 3)) {
        return node_u._2
      }

      // Get neighbors of this node
      val neighbors = this.getNeighbors(node_u._2)

      for (n <- neighbors) {
        // Try exploring paths from this neighbor
        // (available doors will change based on particular paths, so we can't filter out previously seen locations)
        queue.enqueue((n.getDistance(), n))
      }
    }

    println("No route found")
    null
  }

  def getLongestPathToLocation: Route = {
    // Create Route object representing the source
    val root = new Route(distance = 0)

    // Keep track of the longest route we've seen so far
    var longestRoute = root

    // Create a regular queue
    val queue = mutable.Queue[(Int, Route)]()
    queue.enqueue((root.getDistance(), root))

    while (queue.nonEmpty) {
      // Get route with shortest path (starts with source)
      val node_u: (Int, Route) = queue.dequeue()

      // Check if we have found the destination
      if (node_u._2.getLocation() ==(3, 3)) {
        // If yes, see if we have found a longer path, then stop expanding this node
        if (node_u._1 > longestRoute.getDistance()) {
          longestRoute = node_u._2
        }
      } else {
        // Get neighbors of this node
        val neighbors = this.getNeighbors(node_u._2)

        for (n <- neighbors) {
          // Try exploring paths from this neighbor
          // (available doors will change based on particular paths, so we can't filter out previously seen locations)
          queue.enqueue((n.getDistance(), n))
        }
      }
    }

    return longestRoute
  }

  def getNeighbors(route: Route): List[Route] = {
    // Form string to hash (puzzle input + current path)
    val hashInput = new StringBuilder
    hashInput ++= puzzleInput
    hashInput ++= route.getPath()

    // Get hash output
    val hashOutput = this.md5Hex(hashInput.toString())
    val startLocation = route.getLocation()

    val neighbors = mutable.ListBuffer[Route]()
    val doorIsOpenCodes = "bcdef"

    val newPath = new StringBuilder

    // Up
    if (doorIsOpenCodes.contains(hashOutput.charAt(0))) {
      if (startLocation._2 > 0) {
        val up = (startLocation._1, startLocation._2 - 1)
        newPath.clear()
        newPath ++= route.getPath()
        newPath += 'U'
        val upRoute = new Route(path = newPath.toString(), distance = route.getDistance() + 1, x = up._1, y = up._2)
        neighbors.append(upRoute)
      }
    }

    // Down
    if (doorIsOpenCodes.contains(hashOutput.charAt(1))) {
      if (startLocation._2 < this.height - 1) {
        val down = (startLocation._1, startLocation._2 + 1)
        newPath.clear()
        newPath ++= route.getPath()
        newPath += 'D'
        val downRoute = new Route(path = newPath.toString(), distance = route.getDistance() + 1, x = down._1, y = down._2)
        neighbors.append(downRoute)
      }
    }

    // Left
    if (doorIsOpenCodes.contains(hashOutput.charAt(2))) {
      if (startLocation._1 > 0) {
        val left = (startLocation._1 - 1, startLocation._2)
        newPath.clear()
        newPath ++= route.getPath()
        newPath += 'L'
        val leftRoute = new Route(path = newPath.toString(), distance = route.getDistance() + 1, x = left._1, y = left._2)
        neighbors.append(leftRoute)
      }
    }

    // Right
    if (doorIsOpenCodes.contains(hashOutput.charAt(3))) {
      if (startLocation._1 < this.width - 1) {
        val right = (startLocation._1 + 1, startLocation._2)
        newPath.clear()
        newPath ++= route.getPath()
        newPath += 'R'
        val rightRoute = new Route(path = newPath.toString(), distance = route.getDistance() + 1, x = right._1, y = right._2)
        neighbors.append(rightRoute)
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

    // Part A
    val route = puzzle.getShortestPathToLocation
    println("Route length: " + route.getDistance())
    println("Route: " + route.getPath())

    // Part B
    val routePartB = puzzle.getLongestPathToLocation
    println("Route length: " + routePartB.getDistance())
    println("Route: " + routePartB.getPath())
  }
}