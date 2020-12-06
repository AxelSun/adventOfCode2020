package adventofcode

import scala.io.Source

trait SantasLittleHelper {

  def run(): Unit

  def readInput(p: String): List[String] = {
    val path = getClass.getResource(p).getPath
    Source.fromFile(path).getLines().toList
  }

  def readInputAsStrig(p: String): String = {
    val path = getClass.getResource(p).getPath
    Source.fromFile(path).getLines mkString "\n"
  }

  def printFirstAnswer(s: Any) = println(s"Answer to first puzzle is: $s")

  def printSecondAnswer(s: Any) = println(s"Answer to second puzzle is: $s")

  def printPuzzleAnswer(s1: Any, s2: Any): Unit = {
    printFirstAnswer(s1)
    printSecondAnswer(s2)
  }

}
