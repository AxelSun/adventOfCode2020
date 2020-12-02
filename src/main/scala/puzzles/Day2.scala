package adventofcode.puzzles

import adventofcode.SantasLittleHelper

object Day2 extends SantasLittleHelper {

  def run(): Unit = {
    val input: List[Password] = readInput("/day2.txt").map(toPassword(_))

    val nbrValid1 = input.map(_.isValid1).sum
    val nbrValid2 = input.map(_.isValid2).sum

    printPuzzleAnswer(nbrValid1.toString, nbrValid2.toString)
  }

  case class Password(policy: Range, char: Char, seq: String){

    def isValid1(): Int = {
      val nbrOfOccurrences = seq.count(_ == char)
      if(policy contains nbrOfOccurrences) 1 else 0
    }

    def isValid2(): Int = {
      val firstChar = seq(policy.start - 1)
      val secondChar = seq(policy.end - 1)
      if((firstChar == char || secondChar == char) && firstChar != secondChar) 1 else 0
    }

  }

  def toPassword(s: String): Password = {
    val a = s.split("[- :]").toList
    val range = a(0).toInt to a(1).toInt
    val char = a(2)(0)
    val seq = a(4).trim
    Password(range, char, seq)
  }

}
