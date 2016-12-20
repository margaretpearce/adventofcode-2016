import scala.collection.mutable.ArrayBuffer

class Day19(numElves: Int) {
  def passPresents: Int = {
    var elves: ArrayBuffer[(Int, Boolean)] = ArrayBuffer[(Int,Boolean)]()
    for (i <- 1 to numElves) {
      elves += ((i,true))
    }

    while (elves.length != 1) {
      var i: Int = 0
      var numIterations: Int = elves.length
      var nextElf: Int = 0

      while (i < numIterations) {
        // Each elf takes the present of the elf with the next highest #
        nextElf = (i+1) % elves.length

        // Skip elves with no presents
        if (elves(i)._2) {
          elves(i) = (elves(i)._1, elves(nextElf)._2)
          elves(nextElf) = (elves(nextElf)._1, false)
          i = i + 2
        } else {
          i = i + 1
        }
      }

      // Filter out the elves that no longer have presents
      elves = elves.filter(p => p._2)
      numIterations = elves.length
    }

    // Return the index of the elf with all the gifts
    elves.head._1
  }

  def passPresentsPartB: Int = {
    // Star with the first elf
    var elf: Int = 1
    var i: Int = 1

    // Follow pattern to go around the circle
    while (i < numElves) {
      elf = elf % i + 1
      if (elf > (i + 1)/2) {
        elf += 1
      }
      i += 1
    }
    elf
  }
}

object Day19Puzzle {
  def main(args: Array[String]): Unit = {
    // Part A
    val puzzle = new Day19(Integer.parseInt(args(0), 10))
    val elfWithPresents = puzzle.passPresents
    println("Part A: Elf " + elfWithPresents)

    // Part B
    val elfWithPresentsPartB = puzzle.passPresentsPartB
    println("Part B: Elf " + elfWithPresentsPartB)
  }
}
