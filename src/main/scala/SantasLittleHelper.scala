package adventofcode

import scala.io.Source

trait SantasLittleHelper {

  def run(): Unit

  def readInput(p: String): List[String] = {
    val path = getClass.getResource(p).getPath
    Source.fromFile(path).getLines().toList
  }

  def printFirstAnswer(s: String) = println(s"Answer to first puzzle is: $s")

  def printSecondAnswer(s: String) = println(s"Answer to second puzzle is: $s")

  def printPuzzleAnswer(s1: String, s2: String): Unit = {
    printFirstAnswer(s1)
    printSecondAnswer(s2)
  }

}
