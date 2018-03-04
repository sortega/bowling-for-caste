package bowling

import scala.annotation.tailrec

object Bowling {
  type Roll = Int
  type Score = Int

  def scoreForRow(rolls: List[Roll]): Score = {
    @tailrec
    def go(rolls: List[Roll], score: Int = 0): Score = rolls match {
      case first :: second :: rest => go(rest, score + first + second)
      case _                       => score
    }
    go(rolls)
  }
}
