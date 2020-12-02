package adventofcode.puzzles
import adventofcode.SantasLittleHelper


object Day1 extends SantasLittleHelper {

  def run(): Unit = {
    val input = readInput("/day1.txt").map(_.toInt)

    val productOfTwo = findAndMultiplyTwoNumbers(input)
    printFirstAnswer(productOfTwo.toString)

    val productOfThree = findAndMultiplyThreeNumbers(input)
    printSecondAnswer(productOfThree.toString)
  }


  def findAndMultiplyTwoNumbers(l: List[Int]): Int = l match {
    case head :: tail => {
      tail.foreach(element => {
        if(element + head == 2020) return element * head
      })
      findAndMultiplyTwoNumbers(tail)
      }
    case _ => 0
    }


  def findAndMultiplyThreeNumbers(l: List[Int]): Int = l match {
    case head :: tail => {
      tail.foreach( e1 => {
        tail.tail.foreach(e2 => {
          if(head + e1 + e2 == 2020) return head * e1 * e2
        })
      })
      findAndMultiplyThreeNumbers(tail)
    }
    case _ => 0
  }

}