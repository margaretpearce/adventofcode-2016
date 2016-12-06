import java.security.MessageDigest

class Day5(doorId: String) {

  def getPassword: String = {
    var index: Int = 0
    var currentLength: Int = 0
    var newPassword: String = ""

    while (currentLength != 8) {
      // Get the current input (id + index)
      val hashInput: String = doorId.concat(index.toString)

      // Compute the MD5 hash (hex representation)
      val md5hash = this.md5Hex(hashInput)

      // Check if this starts with five zeros
      if (md5hash.startsWith("00000")) {
        // If it does, the sixth character in the hash is the next character
        val nextChar = md5hash.charAt(5)
        newPassword = newPassword.concat(nextChar.toString)
        currentLength = currentLength + 1
      }

      index = index + 1
    }

    newPassword.toLowerCase
  }

  def getPasswordPartB: String = {
    var index: Int = 0
    var currentLength: Int = 0
    val newPassword: Array[Char] = Array('-','-','-','-','-','-','-','-')

    while (currentLength != 8) {
      // Get the current input (id + index)
      val hashInput: String = doorId.concat(index.toString)

      // Compute the MD5 hash (hex representation)
      val md5hash = this.md5Hex(hashInput)

      // Check if this starts with five zeros
      if (md5hash.startsWith("00000") && md5hash.charAt(5).isDigit) {

        // The new character's location is given by the sixth character in the hash
        val charIndex = Integer.parseInt(md5hash.charAt(5).toString, 10)

        if (charIndex <= 7 && newPassword(charIndex) == '-') {
          // If the index is valid, the seventh character in the hash is the next character
          val nextChar = md5hash.charAt(6)

          // Update the character if the index is valid
          newPassword(charIndex) = nextChar
          
          currentLength = currentLength + 1
        }
      }

      index = index + 1
    }

    newPassword.mkString.toLowerCase
  }

  def md5Hex(s: String): String = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).take(7).map("%02X" format _).mkString
  }
}

object Day5Puzzle {
  def main(args: Array[String]): Unit = {
    var input = "cxdnnyjw"

    if (args != null && args.length >= 1){
      input = args(0).toString
    }

    val puzzle = new Day5(input)
    print(puzzle.getPassword)
    print(puzzle.getPasswordPartB)
  }
}

