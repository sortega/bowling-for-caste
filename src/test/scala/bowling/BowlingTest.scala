package bowling

import org.scalatest.{FlatSpec, Matchers}

class BowlingTest extends FlatSpec with Matchers {

  "A gutter game" should "award no points" in {
    Bowling.scoreForRow(List.fill(20)(0)) shouldBe 0
  }
}
