package com.evolution.homework.adt

// -> string
// -> board: Board, cases: List[TestCase]
//    val pokerCombos = Nil
// -> foreach case in cases: (actually it is hand-agnostic)
//      val pokerCombo = StraightFlush(case).orElse(FullHouse(case)) ... :: pokerCombos
//    val testResult = TestResult(pokerCombos)

sealed trait TestCase {
  def board: Board
  def hand: Hand
}
sealed case class TexasCase(board: Board, hand: TexasHand) extends TestCase
sealed case class OmahaCase(board: Board, hand: OmahaHand) extends TestCase
