package adventofcode.puzzles

import adventofcode.SantasLittleHelper
import scala.util.matching.Regex

object Day7 extends SantasLittleHelper {

  def run(): Unit = {

    val input = readInput("/day7.txt")
    val contents = formatInput(input)

    val bagsOfInterest = findBags(List("shiny gold"), contents)
    printFirstAnswer(bagsOfInterest.size)
  }

  def formatInput(input: List[String]): List[(String, Map[String, Int])] =
    input
      .map(_.split(" bags contain").toList)
      .map{rules =>
        val content = rules(1)
        val re = """([0-9*]\s[a-zA-Z]*\s[a-zA-Z]*)""".r
        val matches = re.findAllIn(content).toList
        val contents = matches.foldLeft(Map[String, Int]()){(a, b) =>
          a.+((b.substring(2), b.head.asDigit))
        }
        (rules(0), contents)
      }

  def findBags(target: List[String], contents: List[(String, Map[String, Int])]): Set[String] = {
    def loop(target: List[String], res: Set[String]): Set[String] = target match {
      case Nil => res
      case _ => {
        val eventualBags = contents.filter{ content =>
          (content._2.keySet intersect target.toSet).size > 0
        }.map(_._1)
        loop(eventualBags, res ++ eventualBags)
      }
    }
    loop(target, Set.empty)
  }

}
