package adventofcode.puzzles

import adventofcode.SantasLittleHelper


object Day6 extends SantasLittleHelper{

  def run(): Unit = {
    val input = readInputAsStrig("/day6.txt")
    val inputFormatted = format(input)

    val firstAnswer = countYesAnswersInGroup(inputFormatted)
    val secondAnswer = countEntireGroupYesAnswers(inputFormatted)

    printPuzzleAnswer(firstAnswer, secondAnswer)

  }

  def format(s: String): List[String] = s.split("\\n\\n").toList

  def countYesAnswersInGroup(input: List[String]): Int = {
    input
      .map(_.replace("\n", ""))
      .map(_.toSet.size)
      .sum
  }

  def countEntireGroupYesAnswers(input: List[String]): Int = {
    input
      .map(_.split("\n").toList)
      .map(l => (l.length, l.flatMap(_.toCharArray).groupBy(identity).map(t => (t._1, t._2.length))))
      .map( item => item._2.filter(_._2 == item._1).size)
      .sum
  }

}
