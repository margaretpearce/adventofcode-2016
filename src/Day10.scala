class Factory {
  private val bots = collection.mutable.Map[Int, Bot]()
  private val outputBins = collection.mutable.Map[Int, Int]()
  private val instructions = collection.mutable.ArrayBuffer[(Int, String, Int, String, Int)]()
  private var completedInstructions = collection.mutable.ArrayBuffer[Boolean]()

  def initializeFactory(fileName: String): Unit = {
    val input = scala.io.Source.fromFile(fileName)

    for (line <- input.getLines()) {
      if (line.startsWith("value")) {
        val words = line.split(" ")
        val chip: Int = Integer.parseInt(words(1), 10)
        val botNumber: Int = Integer.parseInt(words(5), 10)

        // Add bot to the factory
        if (!this.bots.contains(botNumber)) {
          val bot = new Bot(botNumber)
          bot.receiveChip(chip)
          this.bots.put(botNumber, bot)
        }
        // Update bot in the factory
        else {
          this.bots(botNumber).receiveChip(chip)
        }

      } else {
        // Create empty bots so the instructions can be processed later
        val pieces = line.split(" ")
        val botNumber: Int = Integer.parseInt(pieces(1), 10)
        val lowRecipientType: String = pieces(5)
        val lowRecipientNumber: Int = Integer.parseInt(pieces(6), 10)
        val highRecipientType: String = pieces(10)
        val highRecipientNumber: Int = Integer.parseInt(pieces(11), 10)

        // Create the giving bot
        if (!this.bots.contains(botNumber)) {
          this.bots.put(botNumber, new Bot(botNumber))
        }

        // Create recipient bots
        if (lowRecipientType.equals("bot") && !this.bots.contains(lowRecipientNumber)) {
          this.bots.put(lowRecipientNumber, new Bot(lowRecipientNumber))
        }
        if (highRecipientType.equals("bot") && !this.bots.contains(highRecipientNumber)) {
          this.bots.put(highRecipientNumber, new Bot(highRecipientNumber))
        }

        // Queue up all the instructions until all bots have been added with their initial states
        this.instructions.append((botNumber, lowRecipientType, lowRecipientNumber, highRecipientType, highRecipientNumber))
      }
    }

    // Mark all instructions as incomplete
    this.completedInstructions = collection.mutable.ArrayBuffer.fill[Boolean](this.instructions.length)(false)
  }

  def runInstructions = {
    while (this.completedInstructions.contains(false)) {
      this.followPossibleInstructions
    }
  }

  def followPossibleInstructions = {
    for (i <- this.instructions.indices) {
      // Check if we've already ran this instruction
      if (!this.completedInstructions(i)) {
        // If not, get the instruction
        val instruction = this.instructions(i)

        val givingBot: Bot = this.bots(instruction._1)

        // Check if we can process the instruction now
        if (givingBot.hasBothChips) {
          val lowerChip = givingBot.giveChip(isLow = true)
          val higherChip = givingBot.giveChip(isLow = false)
          this.bots.put(givingBot.getBotNumber, givingBot)

          if (instruction._2.equals("bot")) {
            this.bots(instruction._3).receiveChip(lowerChip)
          } else {
            this.outputBins.put(instruction._3, lowerChip)
          }

          if (instruction._4.equals("bot")) {
            this.bots(instruction._5).receiveChip(higherChip)
          } else {
            this.outputBins.put(instruction._5, higherChip)
          }

          if (lowerChip == 17 && higherChip == 61) {
            println("Bot #" + givingBot.getBotNumber + " processed 17 and 61")
          }

          // Mark instruction as complete
          this.completedInstructions(i) = true
        }
      }
    }
  }

  def multiplyChipsInOutputBins(binNumbers: List[Int]): Int = {
    var multiplicationOutput = 1

    for (binNumber <- binNumbers) {
      multiplicationOutput = multiplicationOutput * this.outputBins(binNumber)
    }

    multiplicationOutput
  }
}

class Bot(botNumber: Int) {
  private var lowerChip = Option.empty[Int]
  private var higherChip = Option.empty[Int]
  private val bot = botNumber

  def getBotNumber: Int = {
    this.bot
  }

  def hasBothChips: Boolean = {
    this.lowerChip != Option.empty[Int] && this.higherChip != Option.empty[Int]
  }

  def getHighChip: Int = {
    this.higherChip.get
  }

  def getLowChip: Int = {
    this.lowerChip.get
  }

  def giveChip(isLow: Boolean): Int = {
    if (isLow) {
      val chipToGive = this.getLowChip
      this.lowerChip = Option.empty[Int]
      chipToGive
    } else {
      val chipToGive = this.getHighChip
      this.higherChip = Option.empty[Int]
      chipToGive
    }
  }

  def receiveChip(newChip: Int): Unit = {
    // Case 1: Both chips are empty
    if (this.higherChip == Option.empty[Int] && this.lowerChip == Option.empty[Int]) {
      this.lowerChip = Some(newChip)
    }
    // Case 2: Have high chip only
    else if (this.lowerChip == Option.empty[Int] && this.higherChip != Option.empty[Int]) {
      if (this.getHighChip > newChip) {
        this.lowerChip = Some(newChip)
      } else {
        this.lowerChip = this.higherChip
        this.higherChip = Some(newChip)
      }
    }
    // Case 3: Have low chip only
    else if (this.lowerChip != Option.empty[Int] && this.higherChip == Option.empty[Int]) {
      if (this.getLowChip < newChip) {
        this.higherChip = Some(newChip)
      } else {
        this.higherChip = this.lowerChip
        this.lowerChip = Some(newChip)
      }
    }
  }
}

object Day10Puzzle {
  def main(args: Array[String]): Unit = {
    val puzzle = new Factory()
    puzzle.initializeFactory(args(0))
    puzzle.runInstructions
    println(puzzle.multiplyChipsInOutputBins(List(0, 1, 2)))
  }
}
