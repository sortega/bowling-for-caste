package bowling

import bowling.Bowling2._
import org.scalatest.{FlatSpec, Matchers}

class Bowling2Test extends FlatSpec with Matchers {

  private def frame(rolls: Roll*): List[Roll] = rolls.toList

  private val strike = frame(10)

  private implicit class FrameOps(val frame: List[Roll]) {
    def *(times: Int): List[Roll] = List.fill(times)(frame).flatten
  }

  "A gutter game" should "award no points" in {
    Bowling2.scoreForRow(frame(0, 0) * NumFrames) shouldBe 0
  }

  "A game without spares nor strikes" should "score all pins hit down" in {
    Bowling2.scoreForRow(frame(1, 2) ++ frame(0, 0) * 9) shouldBe 3
    Bowling2.scoreForRow(frame(4, 2) * NumFrames) shouldBe 6 * 10
  }

  "A game without strikes" should "count twice the pins hit down after a spare" in {
    Bowling2.scoreForRow(frame(5, 5) ++ frame(1, 1) * 9) shouldBe 10 + 18 + 1
    Bowling2.scoreForRow(frame(2, 2) ++ frame(9, 1) ++ frame(2, 2) * 8) shouldBe 10 + 9 * 4 + 2
  }

  it should "have an extra roll if the tenth frame is a spare" in {
    Bowling2.scoreForRow(frame(1, 2) * 9 ++ frame(5, 5, 5)) shouldBe 3 * 9 + 15
  }

  "A game" should "count twice the two rolls after a strike" in {
    Bowling2.scoreForRow(strike ++ frame(1, 1) * 9) shouldBe 10 + 18 + 2
  }

  it should "have two extra rolls if the tenth frame is a strike" in {
    Bowling2.scoreForRow(frame(1, 2) * 9 ++ frame(10, 5, 5)) shouldBe 3 * 9 + 20
  }

  "The perfect game" should "be scored correctly" in {
    Bowling2.scoreForRow(frame(10) * 9 ++ frame(10, 10, 10)) shouldBe 30 * 10
  }
}
