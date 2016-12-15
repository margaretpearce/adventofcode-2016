import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter.printHexBinary

import scala.collection.mutable

class Day14(salt: String) {
  private val hashes = mutable.Map[Int, String]()
  private val keys = mutable.ArrayBuffer[String]()
  private val keyIndex = mutable.ArrayBuffer[Int]()

  def findHashes(): Unit = {
    var i = 0
    while (keys.length < 64) {
      // Make sure the ith salt has been generated and saved
      this.generateAndSaveHash(i)

      // Check if this is a key
      if (this.hashIsKey(i)) {
        keys.append(this.hashes(i))
        keyIndex.append(i)
        println("Found key #" + i + ": " + this.hashes(i))
      }

      // Continue looking for keys
      i = i + 1
    }

    println("Index " + (i-1) + " generated the 64th hash")
  }

  def generateAndSaveHash(i: Int): String = {
    if (!hashes.contains(i)) {
      // Get the input to the hash algorithm
      val hashInput = salt.concat(i.toString)

      // Compute the MD5 hash
      val md5hash = this.md5Hex(hashInput)

      // Add to the collection
      this.hashes.put(i, md5hash)
    }

    this.hashes(i)
  }

  def md5Hex(s: String): String = {
    // Get MD5 hash as a string of lowercase hex digits
    printHexBinary(MessageDigest.getInstance("MD5").digest(s.getBytes))
  }

  def hashIsKey(hashIndex: Int): Boolean = {
    val hash = this.hashes(hashIndex)

    // Are there 3 repeating characters in this hash?
    for (i <- 0 to hash.length - 3) {
      if (hash.charAt(i) == hash.charAt(i+1) && hash.charAt(i+1) == hash.charAt(i+2)) {
        // If one is found, then check the next 1000 hashes to see if 5 in a row are found
        return this.nextThousandHashesContainFiveInARow(hashIndex+1, hash.charAt(i).toString * 5)
      }
    }
    // If no repeats of 3 are ever found, return false
    false
  }

  def nextThousandHashesContainFiveInARow(startingIndex: Int, fiveRepeating: String): Boolean = {
    // Loop through the next 1000 hashes to see if five of the same character appear in a row
    for (i <- startingIndex to startingIndex+1000) {
      val hash = this.generateAndSaveHash(i)
      if (hash.contains(fiveRepeating)) {
        return true
      }
    }
    return false
  }
}

object Day14Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day14(args(0))
    puzzle.findHashes()
  }
}