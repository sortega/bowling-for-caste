package bowling

import scala.annotation.tailrec

object Bowling2 {
  type Roll = Int
  type Score = Int

  val NumPins = 10
  val NumFrames = 10

  sealed abstract class Frame extends Product with Serializable {
    def score: Score
  }

  object Frame {
    final case class Strike(bonusRolls: List[Roll]) extends Frame {
      override def score: Score = NumPins + bonusRolls.sum
    }

    final case class Spare(bonusRoll: Roll) extends Frame {
      override def score: Score = NumPins + bonusRoll
    }

    final case class Split(first: Roll, second: Roll) extends Frame {
      override def score: Score = first + second
    }

    def parseRow(row: List[Roll]): List[Frame] = unfoldWithIndex(row) {
      case (rolls, index) =>
        if (index >= NumFrames) None
        else (matchStrike orElse matchSpare orElse matchSplit).lift.apply(rolls)
    }

    private val matchStrike: PartialFunction[List[Roll], (List[Roll], Strike)] = {
      case NumPins :: rest => (rest, Strike(rest.take(2)))
    }

    private val matchSpare: PartialFunction[List[Roll], (List[Roll], Spare)] = {
      case first :: second :: rest if first + second == NumPins => (rest, Spare(rest.head))
    }

    private val matchSplit: PartialFunction[List[Roll], (List[Roll], Split)] = {
      case first :: second :: rest => (rest, Split(first, second))
    }

    private def unfoldWithIndex[A, B](initialSeed: A)(f: (A, Int) => Option[(A, B)]): List[B] =
      unfold((initialSeed, 0)) {
        case (seed, index) =>
          f(seed, index).map {
            case (a, b) => ((a, index + 1), b)
          }
      }

    private def unfold[A, B](initialSeed: A)(f: A => Option[(A, B)]): List[B] = {
      @tailrec def go(seed: A, list: List[B] = Nil): List[B] =
        f(seed) match {
          case None                  => list.reverse
          case Some((newSeed, elem)) => go(newSeed, elem :: list)
        }
      go(initialSeed)
    }
  }

  def scoreForRow(rolls: List[Roll]): Score = Frame.parseRow(rolls).map(_.score).sum
}
