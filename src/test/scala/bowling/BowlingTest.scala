package bowling

import org.scalatest.{FlatSpec, Matchers}

class BowlingTest extends FlatSpec with Matchers {

  "A gutter game" should "award no points" in {
    Bowling.scoreForRow(List.fill(20)(0)) shouldBe 0
  }

  "A game without spares nor strikes" should "score all pins hit down" in {
    Bowling.scoreForRow(1 :: 2 :: List.fill(18)(0)) shouldBe 3
  }
}
