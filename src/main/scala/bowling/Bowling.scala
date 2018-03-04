package bowling

object Bowling {
  type Roll = Int
  type Score = Int

  def scoreForRow(rolls: List[Roll]): Score = rolls.head + rolls(1)
}
