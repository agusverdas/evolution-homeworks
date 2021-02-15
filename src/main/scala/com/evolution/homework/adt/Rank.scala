package com.evolution.homework.adt

sealed trait Rank {
  def value: String
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
