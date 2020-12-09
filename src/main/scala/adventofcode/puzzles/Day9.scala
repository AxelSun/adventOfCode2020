package adventofcode.puzzles

import adventofcode.SantasLittleHelper

object Day9 extends SantasLittleHelper {

  def run(): Unit = {
    val input = readInput("/day9.txt").map(BigInt(_))

    val weakNbr = findFirstWeakNbr(input).get
    val weakness = findEncryptionWeakness(input, weakNbr, 2)

    printPuzzleAnswer(weakNbr, weakness)
  }

  def findEncryptionWeakness(input: List[BigInt], target: BigInt, order: Int): BigInt = {
    val validSet = input
      .sliding(order)
      .filter(_.sum == target)
      .flatten
      .toList
    if (validSet.length > 0) validSet.min + validSet.max else findEncryptionWeakness(input, target, order + 1)

  }

  def findFirstWeakNbr(input: List[BigInt], idx: Int = 25): Option[BigInt] = input match {
    case Nil => None
    case _ :: tail => {
      val target = input(idx)
      val preamble = input.slice(0, idx)
      preamble.foreach { nbr =>
        if (preamble.filter(_ != nbr) contains (target - nbr)) return findFirstWeakNbr(tail)
      }
      Some(target)
    }
  }


}
