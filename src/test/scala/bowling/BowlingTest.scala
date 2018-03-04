package bowling

import org.scalatest.{FlatSpec, Matchers}

class BowlingTest extends FlatSpec with Matchers {

  "A gutter game" should "award no points" in {
    Bowling.scoreForRow(List.fill(20)(0)) shouldBe 0
  }

  "A game without spares nor strikes" should "score all pins hit down" in {
    Bowling.scoreForRow(1 :: 2 :: List.fill(18)(0)) shouldBe 3
    Bowling.scoreForRow(List.fill(10)(List(4, 2)).flatten) shouldBe 6 * 10
  }

  "A game without strikes" should "count twice the pins hit down after a spare" in {
    Bowling.scoreForRow(5 :: 5 :: List.fill(18)(1)) shouldBe 10 + 18 + 1
    Bowling.scoreForRow(2 :: 2 :: 9 :: 1 :: List.fill(16)(2)) shouldBe 10 + 18 * 2 + 2
  }

  "A game" should "count twice the two rolls after a strike" in {
    Bowling.scoreForRow(10 :: List.fill(18)(1)) shouldBe 10 + 18 + 2
  }
}
