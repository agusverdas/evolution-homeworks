package com.evolution.homework.adt

sealed trait Rank {
  def value: String
}

object Rank {
  def apply(rank: String): Option[Rank] = {
    rank match {
      case "2" => Some(Two)
      case "3" => Some(Three)
      case "4" => Some(Four)
      case "5" => Some(Five)
      case "6" => Some(Six)
      case "7" => Some(Seven)
      case "8" => Some(Eight)
      case "9" => Some(Nine)
      case "10" => Some(Ten)
      case "J" => Some(Jack)
      case "Q" => Some(Queen)
      case "K" => Some(King)
      case "A" => Some(Ace)
      case _ => None
    }
  }
}

case object Two extends Rank {
  override val value: String = "2"
}
case object Three extends Rank {
  override val value: String = "3"
}
case object Four extends Rank {
  override val value: String = "4"
}
case object Five extends Rank {
  override val value: String = "5"
}
case object Six extends Rank {
  override val value: String = "6"
}
case object Seven extends Rank {
  override val value: String = "7"
}
case object Eight extends Rank {
  override val value: String = "8"
}
case object Nine extends Rank {
  override val value: String = "9"
}
case object Ten extends Rank {
  override val value: String = "T"
}
case object Jack extends Rank {
  override val value: String = "J"
}
case object Queen extends Rank {
  override val value: String = "Q"
}
case object King extends Rank {
  override val value: String = "K"
}
case object Ace extends Rank {
  override val value: String = "A"
}
