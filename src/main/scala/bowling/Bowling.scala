package bowling

import scala.annotation.tailrec

object Bowling {
  type Roll = Int
  type Score = Int

  val NumPins = 10
  val NumFrames = 10

  def scoreForRow(rolls: List[Roll]): Score = {
    @tailrec
    def go(rolls: List[Roll], frame: Int = 1, score: Score = 0): Score =
      if (frame == NumFrames) score + rolls.sum
      else {
        val (frameScore, rest) = (matchStrike orElse matchSpare orElse matchSplit).apply(rolls)
        go(rest, frame + 1, score + frameScore)
      }
    go(rolls)
  }

  private val matchStrike: PartialFunction[List[Roll], (Score, List[Roll])] = {
    case NumPins :: rest => (NumPins + rest.take(2).sum, rest)
  }

  private val matchSpare: PartialFunction[List[Roll], (Score, List[Roll])] = {
    case first :: second :: rest if first + second == NumPins => (NumPins + rest.head, rest)
  }

  private val matchSplit: PartialFunction[List[Roll], (Score, List[Roll])] = {
    case first :: second :: rest => (first + second, rest)
  }
}
