package adventofcode.puzzles

import adventofcode.SantasLittleHelper


object Day1 extends SantasLittleHelper {

  def run(): Unit = {
    val input = readInput("/day1.txt").map(_.toInt)

    val inputAsSet = input.toSet

    val productOfTwo = findProduct(inputAsSet)
    val productOfThree = findTrippleProduct(input)

    printPuzzleAnswer(
      productOfTwo.map(_.toString).get,
      productOfThree.map(_.toString).get
    )
  }


  def findProduct(input: Set[Int]): Option[Int] = {
    input.foreach(entry => {
      val target = 2020 - entry
      val targetExists = input contains target
      if(targetExists) return Some(target * entry)
    })
    None
  }

  def findTrippleProduct(input: List[Int]): Option[Int] = input match {
    case head :: tail => {
      val targetSet = tail.toSet
      tail.foreach(entry => {
        val target = 2020 - head - entry
        val targetExists = targetSet contains target
        if(targetExists) return Some(target * head * entry)
      })
      findTrippleProduct(tail)
    }
    case _ => None
  }

}