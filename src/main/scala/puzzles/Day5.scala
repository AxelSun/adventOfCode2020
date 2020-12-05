package adventofcode.puzzles


import adventofcode.SantasLittleHelper

object Day5 extends SantasLittleHelper {

  def run(): Unit = {

    val seatIds = readInput("/day5.txt")
      .map(_.splitAt(7))
      .map(boardingPass => rowNbr(boardingPass._1) * 8 + seatNbr(boardingPass._2))

    val maxSeatId = seatIds.max
    printFirstAnswer(maxSeatId.toString)

    val allSeats = seatIds.min to maxSeatId
    val mySeat = allSeats.toSet diff seatIds.toSet
    printSecondAnswer(mySeat.toString)
  }

  def rowNbr(boardingPass: String): Int = {
    def loop(s: List[Char], lower: Int, upper: Int): Int = s match {
      case head :: tail => {
        val idx = (lower + upper) / 2
        if (head == 'F') loop(tail, lower, idx) else loop(tail, idx + 1, upper)
      }
      case Nil => lower
    }
    loop(boardingPass.toList, 0, 127)
  }

  def seatNbr(boardingPass: String): Int = {
    def loop(s: List[Char], lower: Int, upper: Int): Int = s match {
      case head::tail => {
        val idx = (lower + upper) / 2
        if (head == 'L') loop(tail, lower, idx) else loop(tail, idx + 1, upper)
      }
      case Nil => lower
    }
    loop(boardingPass.toList, 0, 7 )
  }


}
