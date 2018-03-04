package bowling

object Bowling {
  type Roll = Int
  type Score = Int

  def scoreForRow(rolls: List[Roll]): Score =
    rolls.sum + (if (rolls.take(2).sum == 10) rolls(3) else 0)
}
