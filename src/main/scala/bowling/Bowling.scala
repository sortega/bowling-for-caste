package bowling

import scala.annotation.tailrec

object Bowling {
  type Roll = Int
  type Score = Int

  def scoreForRow(rolls: List[Roll]): Score = {
    @tailrec
    def go(rolls: List[Roll], score: Int = 0): Score = rolls match {
      case 10 :: rest                                      => go(rest, score + 10 + rest.take(2).sum)
      case first :: second :: rest if first + second == 10 => go(rest, score + 10 + rest.head)
      case first :: second :: rest                         => go(rest, score + first + second)
      case _                                               => score
    }
    go(rolls)
  }
}
