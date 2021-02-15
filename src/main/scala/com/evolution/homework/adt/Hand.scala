package com.evolution.homework.adt

object Hand {
  val BoardSize = 5
  val OmahaHandSize = 4
  val MaxCardsOfSameRank = 4
  val TexasHandSize = 2

  val isHandUnique: List[Card] => Boolean = l => l.length == Set(l).size
  val validateCardsOfSameRank: List[Card] => Boolean = _.groupBy(_.suit).values.forall(_.size <= Hand.MaxCardsOfSameRank)
}
sealed abstract class Hand protected(val value: List[Card]) {
  def canEqual(other: Any): Boolean = other.isInstanceOf[Hand]

  override def equals(other: Any): Boolean = other match {
    case that: Hand =>
      (that canEqual this) &&
        value == that.value
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(value)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

sealed abstract case class TexasHand private(first: Card, second: Card) extends Hand(List(first, second))
object TexasHand {
  def apply(list: List[Card]): Option[TexasHand] = {
    // I assume the game is played on a single deck
    Option.when(list.length == Hand.TexasHandSize
      && Hand.validateCardsOfSameRank(list)
      && Hand.isHandUnique(list)
    )(new TexasHand(list.head, list.last) {})
  }
}

sealed abstract case class OmahaHand private(override val value: List[Card]) extends Hand(value)
object OmahaHand {
  def apply(list: List[Card]): Option[OmahaHand] = {
    // I assume the game is played on a single deck
    Option.when(list.length == Hand.OmahaHandSize
      && Hand.validateCardsOfSameRank(list)
      && Hand.isHandUnique(list)
    )(new OmahaHand(list) {})
  }
}
