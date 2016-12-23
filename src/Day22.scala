import scala.collection.mutable.HashMap

class Day22 {
  private var usedSpace = HashMap[(Int,Int), Int]()
  private var totalSpace = HashMap[(Int,Int), Int]()

  def setNodeInformation(fileName: String): Unit = {
    val nodes = scala.io.Source.fromFile(fileName)

    for (node <- nodes.getLines()) {
      if (node.startsWith("/dev/grid/node")) {
        // Get x, y, size, and used
        val nodeInfo = node.split(" +")

        val xyInfo = nodeInfo(0).split("-")
        val x = Integer.parseInt(xyInfo(1).substring(1), 10)
        val y = Integer.parseInt(xyInfo(2).substring(1), 10)

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
}

object Day22Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day22()
    puzzle.setNodeInformation(args(0))
    println("# nodes: " + puzzle.getNumberOfNodes())
    println("# viable pairs: " + puzzle.countViablePairs())
  }
}