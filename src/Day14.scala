import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter.printHexBinary

import scala.collection.mutable

class Day14(salt: String) {
  // Part A
  private val hashes = mutable.Map[Int, String]()
  private val keys = mutable.ArrayBuffer[String]()
  private val keyIndex = mutable.ArrayBuffer[Int]()

  // Part B
  private val stretchedHashes = mutable.Map[Int, String]()
  private val keysStretched = mutable.ArrayBuffer[String]()
  private val keyStretchedIndex = mutable.ArrayBuffer[Int]()

  def findHashes(): Unit = {
    var i = 0
    while (this.keys.length < 64 || this.keysStretched.length < 64) {
      // Make sure the ith salt has been generated and saved
      this.generateAndSaveHash(i)
      this.generateAndSaveStretchedHash(i)

      // Check if this is a key
      if (keys.length < 64 && this.hashIsKey(i)) {
        this.keys.append(this.hashes(i))
        this.keyIndex.append(i)
      }

      // Check if the stretched hash is a key (part B)
      if (this.keysStretched.length < 64 && this.stretchedHashIsKey(i)) {
        this.keysStretched.append(this.stretchedHashes(i))
        this.keyStretchedIndex.append(i)
      }

      // Continue looking for keys
      i = i + 1
    }

    println("Index " + this.keyIndex(63) + " generated the 64th hash (part A)")
    println("Index " + this.keyStretchedIndex(63) + " generated the 64th hash (part B)")
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
    printHexBinary(MessageDigest.getInstance("MD5").digest(s.getBytes)).toLowerCase
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
    false
  }

  def generateAndSaveStretchedHash(i: Int): String = {
    if (!this.stretchedHashes.contains(i)) {
      // Get the original hash
      var hash = this.generateAndSaveHash(i)

      // Rehash this hash 2016 times to get the stretched hash
      for (stretchNum <- 1 to 2016) {
        val nextHashInput = hash.mkString
        val nextHash = this.md5Hex(nextHashInput)
        hash = nextHash
      }

      // Add to the collection of stretched hashes
      this.stretchedHashes.put(i, hash)
    }

    this.stretchedHashes(i)
  }

  def stretchedHashIsKey(hashIndex: Int): Boolean = {
    val hash = this.stretchedHashes(hashIndex)

    // Are there 3 repeating characters in this hash?
    for (i <- 0 to hash.length - 3) {
      if (hash.charAt(i) == hash.charAt(i+1) && hash.charAt(i+1) == hash.charAt(i+2)) {
        // If one is found, then check the next 1000 hashes to see if 5 in a row are found
        return this.nextThousandStretchedHashesContainFiveInARow(hashIndex+1, hash.charAt(i).toString * 5)
      }
    }
    // If no repeats of 3 are ever found, return false
    false
  }

  def nextThousandStretchedHashesContainFiveInARow(startingIndex: Int, fiveRepeating: String): Boolean = {
    // Loop through the next 1000 hashes to see if five of the same character appear in a row
    for (i <- startingIndex to startingIndex+1000) {
      val hash = this.generateAndSaveStretchedHash(i)
      if (hash.contains(fiveRepeating)) {
        return true
      }
    }
    false
  }
}

object Day14Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day14(args(0))
    puzzle.findHashes()
  }
}