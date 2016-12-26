import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

class Day24 {
  private var map = ArrayBuffer[Array[Char]]()
  private var points = HashMap[Int, (Int,Int)]()
  private var startingPoint = (-1,-1)
  private val distances = HashMap[(Int,Int), Int]()

  // Read in the map to get coordinates of each point
  def setMap(fileName: String): Unit = {
    val mapUnparsed = scala.io.Source.fromFile(fileName)

    for (mapLines <- mapUnparsed.getLines()) {
      // Store the entire map so we know where the walls are
      val mapCharacters = mapLines.toCharArray
      this.map.append(mapCharacters)

      // Store any points on this line
      for (i <- mapCharacters.indices) {
        if (mapCharacters(i).isDigit) {
          val point = Integer.parseInt(mapCharacters(i).toString, 10)
          this.points.put(point, (i, this.map.length-1))
        }
      }
    }

    // Start at "0"
    this.startingPoint = this.points(0)

    // Initialize all distances to -1 (one sided only)
    val locationPairs = this.points.keys.toList.combinations(2)

    for (pair <- locationPairs) {
      val tuplePair = (pair(0), pair(1))
      this.distances.put(tuplePair, -1)
    }
  }

  // Debugging: print locations of each point
  def printLocations(): String = {
    val locations = new StringBuilder()
    val points = this.points.keys

    for (p <- points) {
      locations.append(p)
      locations.append(": (")
      locations.append(this.points(p)._1)
      locations.append(",")
      locations.append(this.points(p)._2)
      locations.append(")\n")
    }

    locations.toString()
  }

  def findPairwiseDistances(): Unit = {
    val pairs = this.distances.keys.toList

    // For each set of points, find the shortest distance between them and save it for both directions
    for (pair <- pairs) {
      val startLocation = this.points(pair._1)
      val endLocation = this.points(pair._2)
      val distance = this.getShortestPath(startLocation, endLocation)
      this.distances.put(pair, distance._1)
      this.distances.put((pair._2, pair._1), distance._1)
    }
  }

  def getShortestPath(start: (Int,Int), end: (Int,Int)): (Int, Map[(Int, Int), (Int, Int)]) = {
    val distances = mutable.Map[(Int, Int), Int]()
    val previous = mutable.Map[(Int,Int), (Int,Int)]()

    // Add to priority queue (with smallest items appearing first in the queue)
    var queue = mutable.PriorityQueue.empty[(Int, (Int,Int))](implicitly[Ordering[(Int, (Int, Int))]].reverse)
    for (p <- this.points.keySet) {
      distances.put(this.points(p), Int.MaxValue)

      if (this.points(p) == start) {
        distances.put(this.points(p), 0)
      }

      queue += Tuple2(distances(this.points(p)), this.points(p))
    }

    while (queue.nonEmpty) {
      // Get vertex with best distance (starts with source)
      val node_u: (Int, (Int, Int)) = queue.dequeue()

      // Check if we have found the destination
      if (node_u._2._1.equals(end._1) && node_u._2._2.equals(end._2)) {
        return (node_u._1, previous.toMap)
      }

      // Get neighbors of this node
      val neighbors = this.getNeighbors(node_u._2._1, node_u._2._2)

      for (n <- neighbors) {
        // Check if we found a closer distance to this neighboring node
        val distanceToNeighbor = node_u._1 + 1

        // Initialize the distance to infinity if this node wasn't included in the original set
        if (!distances.contains(n._1, n._2)) {
          distances.put(Tuple2(n._1, n._2), Int.MaxValue)
          queue += Tuple2(distances(n._1, n._2), (n._1, n._2))
        }

        if (!distances.contains(n._1, n._2) || distanceToNeighbor < distances(n._1, n._2)) {
          // Update the distance and previous node values to reflect the newly found shortest path
          distances.put(Tuple2(n._1, n._2), distanceToNeighbor)
          previous.put(Tuple2(n._1, n._2), Tuple2(node_u._2._1, node_u._2._2))

          // Update the queue
          queue = this.removeItemInQueue(queue, Tuple2(n._1, n._2))
          queue += Tuple2(distances(n._1, n._2), (n._1, n._2))
        }
      }
    }

    // If we never dequeued the destination node, return -1 to indicate an error occurred
    (-100, previous.toMap)
  }

  def getNeighbors(x: Int, y: Int): List[(Int, Int)] = {
    val neighbors = mutable.MutableList[(Int, Int)]()

    // North
    if (y - 1 >= 0 && !this.isCoordinateWall(x, y - 1)) {
      neighbors += Tuple2(x, y - 1)
    }
    // South (can go in infinite direction)
    if (y+1 < this.map.length && !this.isCoordinateWall(x, y + 1)) {
      neighbors += Tuple2(x, y + 1)
    }
    // East
    if (x+1 < this.map(0).length && !this.isCoordinateWall(x + 1, y)) {
      neighbors += Tuple2(x + 1, y)
    }
    // West
    if (x - 1 >= 0 && !this.isCoordinateWall(x - 1, y)) {
      neighbors += Tuple2(x - 1, y)
    }

    neighbors.toList
  }

  def isCoordinateWall(x: Int, y: Int): Boolean = {
    this.map(y)(x) == '#'
  }

  def removeItemInQueue(queue: mutable.PriorityQueue[(Int, (Int, Int))], item: (Int, Int)): mutable.PriorityQueue[(Int, (Int, Int))] = {
    val dequeuedItems: mutable.ArrayBuffer[(Int, (Int, Int))] = mutable.ArrayBuffer[(Int, (Int,Int))]()
    var found: Boolean = false

    // Dequeue items until you find the one that should be filtered out
    while (!found && queue.nonEmpty) {
      val dequeued = queue.dequeue()

      if (dequeued._2._1.equals(item._1) && dequeued._2._2.equals(item._2)) {
        found = true
      } else {
        dequeuedItems.append(dequeued)
      }
    }

    // Add the incorrectly dequeued items back
    for (i <- dequeuedItems) {
      queue.enqueue(i)
    }

    // Return the modified queue
    queue
  }

  def printShortestPath(previous: Map[(Int, Int), (Int, Int)]): Unit = {
    var sequence: String = this.startingPoint._1 + "," + this.startingPoint._2 + "\n"
    var u = this.points(3)
    while (previous.contains(u._1, u._2)) {
      sequence = previous(u._1, u._2)._2 + "," + previous(u._1, u._2)._1 + "\n" + sequence
      u = previous(u._1, u._2)
    }
    println(sequence)
  }

  def findShortestTour(): (Int, String) = {
    // Start at 0
    val pointstoVisit = this.points.keys.toList.filter(p => p != 0)
    val tourOptions = pointstoVisit.permutations.toList

    var shortestDistance = Int.MaxValue
    var shortestTour = ""

    // Try all tour permutations and find the shortest one based on pre-computed distances
    for (tour <- tourOptions) {
      var tourDistance = 0
      var startingPlace = 0

      for (i <- tour) {
        tourDistance += this.distances((startingPlace, i))
        startingPlace = i
      }

      if (tourDistance < shortestDistance) {
        shortestDistance = tourDistance
        shortestTour = "0," + tour.mkString(",")
      }
    }

    // Return the shortest distance and the route to take
    (shortestDistance, shortestTour)
  }
}

object Day24Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day24()

    // Build the map
    puzzle.setMap(args(0))
    puzzle.findPairwiseDistances()
    println(puzzle.printLocations())
    val shortestTour = puzzle.findShortestTour()
    println("Shortest tour: " + shortestTour._1 + " (" + shortestTour._2 + ")")
  }
}
