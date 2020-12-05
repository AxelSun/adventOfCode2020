package adventofcode.puzzles
import adventofcode.SantasLittleHelper

object Day5 extends SantasLittleHelper {

  def run(): Unit = {
    val seatIds = readInput("/day5.txt")
        .map( boardingPass => {
          boardingPass.map(c => if(c == 'F' || c == 'L') 0 else 1).splitAt(7)
        })
        .map(s => findNbrFromBits(s._1) * 8 + findNbrFromBits(s._2, 7))

    val maxSeatId = seatIds.max
    printFirstAnswer(maxSeatId.toString)

    val mySeat = (seatIds.min to seatIds.max).toSet diff seatIds.toSet
    printSecondAnswer(mySeat.head.toString)
  }


  def findNbrFromBits(bits: IndexedSeq[Int], upper: Int = 127): Int = {
    def loop(bits: List[Int], lower: Int, upper: Int): Int = bits match {
      case head :: tail => {
        val idx = (lower + upper) / 2
        if (head == 0) loop(tail, lower, idx) else loop(tail, idx + 1, upper)
      }
      case Nil => lower
    }
    loop(bits.toList, 0, upper)
  }

}
