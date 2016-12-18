import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter._
import scala.collection.mutable

class Route {
  private var pathString = ""
  private var pathDistance = 0
  private var currentX = 0
  private var currentY = 0

  def getPath(): (String, Int) = {
    (pathString, pathDistance)
  }

  def getLocation(): (Int, Int) = {
    (currentX, currentY)
  }

  def updatePath(path: String, distance: Int, x: Int, y: Int) = {
    this.pathString = path
    this.pathDistance = distance
    this.currentX = x
    this.currentY = y
  }
}

class Day17 {
  private val doorOpen = mutable.Map[(Int,Int), (Int,Int)]()
  // private var routesToExplore = mutable.PriorityQueue[(Int, (Int, Int))]()
  private var routesFollowed = mutable.Map[(Int,Int), String]()

  def findPathFromInput(input: String): String = {
    val distance = mutable.Map[(Int, Int), Int]()
    val parent = mutable.Map[(Int,Int), (Int,Int)]()

    val queue = mutable.Queue[((Int,Int), Int, String)]()

    val root = ((0,0), 0)
  }

  def md5Hex(s: String): String = {
    // Get MD5 hash as a string of lowercase hex digits
    printHexBinary(MessageDigest.getInstance("MD5").digest(s.getBytes)).toLowerCase
  }

}

object Day17Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day17()


  }
}