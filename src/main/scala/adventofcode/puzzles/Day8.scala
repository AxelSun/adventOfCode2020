package adventofcode.puzzles

import adventofcode.SantasLittleHelper

object Day8 extends SantasLittleHelper {

  case class Instruction(desc: String, nbr: Int)

  def run: Unit = {
    val input = readInput("/day8.txt")
      .map(_.split("\\s") match {
        case Array(desc, nbr) => Instruction(desc, nbr.toInt)
      })
      .toArray

    val res = accAfterIteration(input)
    printFirstAnswer(res._2)

    val res2 = (0 until input.length).map{ flipIdx =>
      val instruction = input(flipIdx)
      instruction match {
        case Instruction("nop", _) => accAfterIteration(input.updated(flipIdx, instruction.copy("jmp")))
        case Instruction("jmp", _) => accAfterIteration(input.updated(flipIdx, instruction.copy("nop")))
        case _ => accAfterIteration(input)
      }
    }.filter(_._1)

    printSecondAnswer(res2.head._2)
  }

  def accAfterIteration(input: Array[Instruction]): (Boolean, Int) = {
    def loop(idx: Int, acc: Int, visited: Set[Int]): (Boolean, Int) = {
      if(idx + 1 == input.length) (true, acc) // terminated
      else if( visited contains idx ) (false, acc)
      else input(idx) match {
        case Instruction("nop", _) => loop(idx+1, acc, visited + idx)
        case Instruction("acc", nbr) => loop(idx+1, acc+nbr, visited + idx)
        case _ => loop(idx + input(idx).nbr, acc, visited + idx)
      }
    }
    loop(0, 0, Set())
  }

}
