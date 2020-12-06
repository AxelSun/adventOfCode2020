package adventofcode.puzzles

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._


class TestDay6 extends AnyFlatSpec with Matchers {

  val testData =
    """abc
      |
      |a
      |b
      |c
      |
      |ab
      |ac
      |
      |a
      |a
      |a
      |a
      |
      |b""".stripMargin

  "format" should "take input as string and break each group into a list" in {
    val formattedInput = Day6.format(testData)
    formattedInput.length should equal(5)
  }

  "countYesAnswersInGroup" should "sum all distinct yes answers" in {
    val input = Day6.format(testData)
     Day6.countYesAnswersInGroup(input) should equal(11)
  }

  "countEntireGroupYesAnswers" should "sum all distinct yes answers" in {
    val input = Day6.format(testData)
    Day6.countEntireGroupYesAnswers(input) should equal(6)
  }

}
