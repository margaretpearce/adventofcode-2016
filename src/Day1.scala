import util.control.Breaks._

class Day1(partB: Boolean = false) {

  object Direction extends Enumeration {
    type Direction = Value
    val N, S, E, W = Value
  }

  import Direction._

  var x: Int = 0
  var y: Int = 0
  var direction: Direction = N
  var input = None: Option[String]
  val trackedLocations = scala.collection.mutable.ListBuffer.empty[(Int, Int)]


  def setInput(fileName: String): Unit = {
    input = Some(scala.io.Source.fromFile(fileName).mkString)
    trackedLocations.append((x, y))
  }

  def processInput(): Unit = {
    val inputArray = input.get.split(",")

    breakable {
      for (instruction <- inputArray) {
        val step = instruction.trim().charAt(0)
        val distanceChar = instruction.trim().substring(1)
        val distance = Integer.parseInt(distanceChar, 10)
        val previousX = x
        val previousY = y
        this.move(step, distance)

        // Check if we have visited any passed by location before (Part B)
        if (partB) {
          val repeatedLocation = this.checkForVisitedLocation(previousX, previousY)

          // If the location has been repeated, then exit
          if (repeatedLocation) {
            break()
          }
        }
      }
    }
  }

  def checkForVisitedLocation(previousX: Int, previousY: Int): Boolean = {
    // Moving east or west
    if (this.x != previousX) {
      var bystep = 1
      if (previousX > this.x) {
        bystep = -1
      }

      for (xstep <- previousX to this.x by bystep) {
        // Don't add the current location twice (once this time, once next move)
        if (xstep != previousX) {
          val location = Tuple2(xstep, this.y)

          if (this.trackedLocations.contains(location)) {
            this.x = location._1
            this.y = location._2
            return true
          } else {
            this.trackedLocations.append(location)
          }
        }
      }

      return false
    }
    // Moving north or south
    else if (this.y != previousY) {
      var bystep = 1
      if (previousY > this.y) {
        bystep = -1
      }
      for (ystep <- previousY to this.y by bystep) {
        // Don't add the current location twice (once this time, once next move)
        if (ystep != previousY) {
          val location = Tuple2(this.x, ystep)

          if (this.trackedLocations.contains(location)) {
            this.x = location._1
            this.y = location._2
            return true
          } else {
            this.trackedLocations.append(location)
          }
        }
      }

      return false
    }

    // If position hasn't changed, then the current location was visited on this move and on the last one
    true
  }

  def move(step: Char, distance: Int): Unit = {
    if (direction == N) {
      if (step == 'L') {
        direction = W
        x = x - distance
      } else if (step == 'R') {
        direction = E
        x = x + distance
      }
    } else if (direction == E) {
      if (step == 'L') {
        direction = N
        y = y + distance
      } else if (step == 'R') {
        direction = S
        y = y - distance
      }
    } else if (direction == S) {
      if (step == 'L') {
        direction = E
        x = x + distance
      } else if (step == 'R') {
        direction = W
        x = x - distance
      }
    } else if (direction == W) {
      if (step == 'L') {
        direction = S
        y = y - distance
      } else if (step == 'R') {
        direction = N
        y = y + distance
      }
    }
  }

  def run(): Int = {
    Math.abs(x) + Math.abs(y)
  }
}

object Question1 {
  def main(args: Array[String]): Unit = {
    val puzzle = new Day1(args(1).toBoolean)
    puzzle.setInput(args(0))
    puzzle.processInput()
    val output = puzzle.run()
    println(output)
  }
}

