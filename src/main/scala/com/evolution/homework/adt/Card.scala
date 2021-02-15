package com.evolution.homework.adt

final case class Card private(rank: Rank, suit: Suit)

object Card {
  def apply(rank: Rank, suit: Suit) = {
    new Card(rank, suit)
  }
}
