import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Day13 (favoriteNumber: Int) {
  private var destinationX = -1
  private var destinationY = -1
  private val startX = 1
  private val startY = 1
  private var distinctLocations = mutable.Set[(Int, Int)]()

  def isCoordinateWall(x: Int, y: Int): Boolean = {
    // Find x*x + 3*x + 2*x*y + y + y*y
    var formula = (x*x) + (3*x) + (2*x*y) + y + (y*y)

    // Add your puzzle input
    formula = formula + favoriteNumber

    // Find the binary representation
    val binaryRep = Integer.toBinaryString(formula)

    // Count the number of bits that are one
    val numberBitsOne = binaryRep.count(b => b == '1')

    // If the # of bits that are 1 is odd, it's a wall, else it's open space
    numberBitsOne % 2 == 1
  }

  def printGrid(xLimit: Int, yLimit: Int): Unit = {
    for (y <- 0 to yLimit) {
      print(y + " ")
      for (x <- 0 to xLimit) {
        if (this.isCoordinateWall(x,y)) {
          print("#")
        }  else {
          print(".")
        }
      }
      print("\n")
    }
  }

  def setDestination(x: Int, y: Int) = {
    this.destinationX = x
    this.destinationY = y
  }

  def getShortestPathToLocation: (Int, Map[Tuple2[Int,Int], Tuple2[Int,Int]]) = {
    val distances = mutable.Map[Tuple2[Int, Int], Int]()
    val previous = mutable.Map[Tuple2[Int,Int], Tuple2[Int,Int]]()

    // Add to priority queue (with smallest items appearing first in the queue)
    var queue = mutable.PriorityQueue.empty[(Int, (Int,Int))](implicitly[Ordering[(Int, (Int, Int))]].reverse)
    for (y <- 0 to this.destinationY) {
      for (x <- 0 to this.destinationX) {
        if (!this.isCoordinateWall(x,y)) {
          // Initialize distance to infinity
          if (!distances.contains(y,x)) {
            distances.put(Tuple2(y,x), Int.MaxValue)
          }

          // Distance to starting location is 0
          if (y == this.startY && x == this.startX) {
            distances.put((y,x), 0)
          }

          queue += Tuple2(distances(y,x), (y, x))
        }
      }
    }

    while (!queue.isEmpty) {
      // Get vertex with best distance (starts with source)
      val node_u: Tuple2[Int, (Int,Int)] = queue.dequeue()

      // Add to distinct locations if # steps to get to this node is under 50
      if (node_u._1 <= 50) {
        this.distinctLocations.add(node_u._2)
      }

      // Check if we have found the destination
      if (node_u._2._1.equals(this.destinationY) && node_u._2._2.equals(this.destinationX)) {
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
    (-1, previous.toMap)
  }

  def getNeighbors(y: Int, x: Int): List[(Int, Int)] = {
    val neighbors = mutable.MutableList[(Int, Int)]()

    // North
    if (y-1 >= 0 && !this.isCoordinateWall(x,y-1)) {
      neighbors += Tuple2(y-1,x)
    }
    // South (can go in infinite direction)
    if (!this.isCoordinateWall(x,y+1)) {
      neighbors += Tuple2(y+1,x)
    }
    // East
    if (!this.isCoordinateWall(x+1,y)) {
      neighbors += Tuple2(y,x+1)
    }
    // West
    if (x-1 >= 0 && !this.isCoordinateWall(x-1,y)) {
      neighbors += Tuple2(y,x-1)
    }

    neighbors.toList
  }

  def removeItemInQueue(queue: mutable.PriorityQueue[(Int, Tuple2[Int,Int])], item: Tuple2[Int, Int]): mutable.PriorityQueue[(Int, Tuple2[Int,Int])] = {
    var dequeuedItems: ArrayBuffer[(Int, Tuple2[Int, Int])] = ArrayBuffer[(Int, Tuple2[Int,Int])]()
    var found: Boolean = false

    // Dequeue items until you find the one that should be filtered out
    while (!found && !queue.isEmpty) {
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

  def printShortestPath(previous: Map[Tuple2[Int,Int], Tuple2[Int,Int]]): Unit = {
    var sequence: String = this.destinationX + "," + this.destinationY + "\n"
    var u = (this.destinationY, this.destinationX)
    while (previous.contains(u._1, u._2)) {
      sequence = previous(u._1, u._2)._2 + "," + previous(u._1, u._2)._1 + "\n" + sequence
      u = previous(u._1, u._2)
    }
    println(sequence)
  }

  def getDistinctLocationsNumber: Int = {
    this.distinctLocations.size
  }
}

object Day13Puzzle {
  def main(args: Array[String]): Unit = {
    val favoriteNum: Int = Integer.parseInt(args(0), 10)
    val x: Int = Integer.parseInt(args(1), 10)
    val y: Int = Integer.parseInt(args(2), 10)

    // Create the grid and do a sanity check to make sure we aren't being sent to the wall
    val puzzle = new Day13(favoriteNum)
    puzzle.printGrid(x, y)
    println(puzzle.isCoordinateWall(x, y))

    // Part A
    puzzle.setDestination(x,y)
    val shortedPath = puzzle.getShortestPathToLocation
    println("\nDistance: " + shortedPath._1 + "\n")
    println("Path:")
    puzzle.printShortestPath(shortedPath._2)

    // Part B
    val numDistinctNodes = puzzle.getDistinctLocationsNumber
    println("\n\nDistinct Nodes: " + numDistinctNodes)
  }
}