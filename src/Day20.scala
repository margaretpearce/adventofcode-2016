import scala.collection.mutable.SortedSet

class Day20 {
  private var blacklist: SortedSet[(Long,Long)] = SortedSet[(Long,Long)]()

  def processInput(fileName: String): Unit = {
    val input = scala.io.Source.fromFile(fileName)

    for (line <- input.getLines()) {
      this.addBlacklistIPs(line)
    }
  }

  def addBlacklistIPs(ipRange: String): Unit = {
    val ips = ipRange.split("-")
    val start = ips(0).trim.toLong
    val end = ips(1).trim.toLong
    this.blacklist+=((start,end))
  }

  def getSmallestAllowedIP(): Long = {
    var ip: Long = 0

    for (i <- this.blacklist) {
      if (ip >= i._1 && ip > i._2) {
        // Ranges are overlapping - we are already blacklisting this range
        // Check the next range
      }
      else if (ip >= i._1 && ip <= i._2) {
        // Current IP is in this range of blacklisted IPs
        ip = i._2 + 1
      } else {
        // This IP is not blacklisted
        return ip
      }
    }
    ip
  }

  def getNumberAllowedIPs(): Long = {
    // Start at the first IP range on the blacklist
    var lastIPEnd:Long = this.blacklist.head._2
    var count: Long = this.blacklist.head._1

    // Count the number of available IPs
    for (i <- this.blacklist) {
      // If we start at a farther ahead range than before, add the number of IPs in between
      if (i._1 > lastIPEnd + 1) {
        count += (i._1 - lastIPEnd - 1)
      }
      // Move ahead to the largest blacklisted IP we've seen so far
      lastIPEnd = Math.max(lastIPEnd, i._2)
    }

    // Return the number of 32 bit integers minus the number of blacklisted IPs (last blacklisted IP - # gaps)
    val numIPsTotal: Long = Math.pow(2,32).toLong - 1
    numIPsTotal - lastIPEnd + count
  }
}

object Day20Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day20()
    puzzle.processInput(args(0))

    // Part A
    val smallestIP = puzzle.getSmallestAllowedIP()
    println("The smallest IP is: " + smallestIP)

    // Part B
    val numAllowedIPs = puzzle.getNumberAllowedIPs()
    println(numAllowedIPs + " IP addresses are allowed")
  }
}