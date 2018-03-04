package bowling

import scala.annotation.tailrec

object Bowling {
  type Roll = Int
  type Score = Int

  val NumPins = 10
  val NumFrames = 10

  def scoreForRow(rolls: List[Roll]): Score = {
    @tailrec
    def go(rolls: List[Roll], score: Int = 0): Score = rolls match {
      case NumPins :: rest =>
        go(rest, score + NumPins + rest.take(2).sum)
      case first :: second :: rest if first + second == NumPins =>
        go(rest, score + NumPins + rest.head)
      case first :: second :: rest =>
        go(rest, score + first + second)
      case _ => score
    }
    go(rolls)
  }
}
