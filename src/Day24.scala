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
    val pairs = this.distances.keySet

    // For each set of points, find the shortest distance between them and save it for both directions
    for (pair <- pairs) {
      val distance = this.bfs(pair._1, pair._2)
      this.distances.put(pair, distance)
      this.distances.put((pair._2, pair._1), distance)
    }
  }

  def bfs(start: Int, end: Int): Int = {
    0
  }
}

object Day24Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day24()

    // Build the map
    puzzle.setMap(args(0))
    println(puzzle.printLocations())
  }
}
