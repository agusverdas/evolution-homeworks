package com.evolution.homework.adt

// actually a board is a hand of stickman, but extending Hand could lead to strange exhaustiveness checks
sealed abstract case class Board private(value: List[Card])

object Board extends {
  def apply(list: List[Card]): Option[Board] = {
    val predicate = list.length == Hand.BoardSize && Hand.validateCardsOfSameRank(list) && Hand.isHandUnique(list)
    Option.when(predicate)(new Board(list) {})
  }
  def apply(first: Card, second: Card, third: Card, fourth: Card, fifth: Card): Option[Board] = {
    apply(first :: second :: third :: fourth :: fifth :: Nil)
  }
  def apply(five: (Card, Card, Card, Card, Card)): Option[Board] = {
    apply(five._1, five._2, five._3, five._4, five._5)
  }
}
