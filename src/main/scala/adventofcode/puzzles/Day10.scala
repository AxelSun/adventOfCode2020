package adventofcode.puzzles

import adventofcode.SantasLittleHelper

object Day10 extends SantasLittleHelper {

  val testInput = List(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)

  def run(): Unit = {
    testInput.sorted.foreach(println(_))
    val input = readInput("/day10.txt").map(_.toInt)
    val inputWithAdapter = input ++ List(0, input.max + 3)

    val res1 = productDiffOneDiffThree(inputWithAdapter)
    printFirstAnswer(res1)

    val res2 = distinctAdapterArrangements(input)
    printSecondAnswer(res2)
  }

  def distinctAdapterArrangements(input: List[Int]) = {
    val sortedInput = input.sorted
    def loop(input: List[Int], res: Map[Int, BigInt]): BigInt = input match {
      case _ :: Nil => res.max._2
      case head :: tail => {
        val acc = res.getOrElse(head-1, BigInt(0)) + res.getOrElse(head-2, BigInt(0)) + res.getOrElse(head-3, BigInt(0))
        loop(tail, res + (head -> acc))
      }
    }
    loop(sortedInput, Map(0 -> 1))
  }

  def productDiffOneDiffThree(input: List[Int]): Int = {
    val sortedInput = input.sorted

    val joltDifferences = sortedInput
      .sliding(2)
      .foldLeft(List.empty[Int])((acc, adapters) => acc :+ (adapters(1) - adapters(0)))
      .groupBy(identity).view.mapValues(_.size)

    joltDifferences(1) * joltDifferences(3)
  }

}
