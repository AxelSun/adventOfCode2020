package adventofcode.puzzles

import adventofcode.SantasLittleHelper


object Day7 extends SantasLittleHelper {

  def run(): Unit = {

    val input = readInput("/day7.txt")
    val contents = parse(input)

    val bagsOfInterest = findBagColors("shiny gold", contents)
    printFirstAnswer(bagsOfInterest.size)

    val res = nbrBags("shiny gold", 1, contents)
    printSecondAnswer(res - 1)
  }

  type Color = String
  type Content = (Color, Map[Color, Int])

  def parse(input: List[String]): List[Content] =
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

  def findBagColors(target: Color, contents: List[Content]): Set[Color] = {
    def loop(target: List[Color], res: Set[Color]): Set[Color] = target match {
      case Nil => res
      case _ => {
        val eventualBags = contents.filter{ content =>
          (content._2.keySet intersect target.toSet).size > 0
        }.map(_._1)
        loop(eventualBags, res ++ eventualBags)
      }
    }
    loop(List(target), Set.empty)
  }


  def nbrBags(target: Color, multiplier: Int, input: List[Content]): Int = {
    val children = input.filter(_._1 == target).head._2
    children match {
      case m: Map[Color, Int] if(m.isEmpty) => multiplier
      case _ => {
        val res = children.map{ child =>
          val (nextTargetColor, nextMultiplier) = child
          multiplier * nbrBags(nextTargetColor, nextMultiplier, input)
        }
        multiplier :: res.toList
      }.sum
    }
  }

}
