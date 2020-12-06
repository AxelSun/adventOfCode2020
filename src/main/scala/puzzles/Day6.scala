package adventofcode.puzzles

import adventofcode.SantasLittleHelper


object Day6 extends SantasLittleHelper{

  def run(): Unit = {
    val input = readInputAsStrig("/day6.txt")
      .split("\\n\\n")
      .toList

    val firstAnswer = input
      .map(_.replace("\n", ""))
      .map(_.toSet.size)
      .sum

    printFirstAnswer(firstAnswer.toString)

    val secondAnswer = input
      .map(_.split("\n").toList)
      .map(l => (l.length, l.flatMap(_.toCharArray).groupBy(identity).map(t => (t._1, t._2.length))))
      .map( item => item._2.filter(_._2 == item._1).size)
      .sum

    printSecondAnswer(secondAnswer.toString)

  }

}
