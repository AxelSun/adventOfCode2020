package adventofcode.puzzles

import adventofcode.SantasLittleHelper

object Day3 extends SantasLittleHelper {
  def run(): Unit = {
    val input = readInput("/day3.txt").map(_.toCharArray.toList)

    val nbrOfTrees = forestWalk(input, 3, 1)

    val treeProduct = List(
      forestWalk(input, 1, 1),
      forestWalk(input, 3, 1),
      forestWalk(input, 5, 1),
      forestWalk(input, 7, 1),
      forestWalk(input, 1, 2)
    ).map(BigInt(_)).product

    printPuzzleAnswer(nbrOfTrees.toString, treeProduct.toString)
  }


  def forestWalk(input: List[List[Char]], right: Int, down: Int): Int = {
    def loop(idx: Int, forest: List[List[Char]], nbrTrees: Int): Int = forest match {
      case head :: tail => {
        val isTree = head(idx) == '#'
        val newIdx = if(idx + right >= head.length) idx + right - head.length else idx + right
        val nextRow = tail.slice(down - 1, tail.length)
        if(isTree) loop(newIdx, nextRow, nbrTrees  + 1) else loop(newIdx, nextRow, nbrTrees)
      }
      case Nil => nbrTrees
    }
    loop(0, input, 0)
  }
}
