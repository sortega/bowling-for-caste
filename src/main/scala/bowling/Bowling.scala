package bowling

import scala.annotation.tailrec

object Bowling {
  type Roll = Int
  type Score = Int

  val NumPins = 10
  val NumFrames = 10

  def scoreForRow(rolls: List[Roll]): Score = {
    @tailrec
    def go(rolls: List[Roll], frame: Int = 1, score: Score = 0): Score = rolls match {
      case _ if frame == NumFrames =>
        score + rolls.sum
      case NumPins :: rest =>
        go(rest, frame + 1, score + NumPins + rest.take(2).sum)
      case first :: second :: rest if first + second == NumPins =>
        go(rest, frame + 1, score + NumPins + rest.head)
      case first :: second :: rest =>
        go(rest, frame + 1, score + first + second)
    }
    go(rolls)
  }
}
