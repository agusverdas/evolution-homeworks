package com.evolution.homework.adt

sealed trait Combination {
  protected def name: String
  def testCase: TestCase
  protected[this] def combination: List[Card]
}

sealed abstract case class HighCard(testCase: TestCase) extends Combination {
  override val name: String = "High Card"
  // find most heavy card in a hand
  val card: Card = combination.head
}

object HighCard {
  // find most heavy card in a hand
  def evaluate(list: List[Card]): Card = ???
  def apply(testCase: TestCase): Option[HighCard] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    Option.when(Hand.isHandUnique(allCards))(new HighCard(testCase) {
      override protected[this] val combination: List[Card] = List(evaluate(allCards))
    })
  }
}

sealed abstract case class Pair private(testCase: TestCase) extends Combination {
  override protected val name: String = "Pair"
}
object Pair {
  def evaluate(list: List[Card]): Option[List[Card]] = ???
  def apply(testCase: TestCase): Option[Pair] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val pair = evaluate(allCards)
    Option.when(Hand.isHandUnique(allCards) && pair.isDefined)(new Pair(testCase) {
      override val combination: List[Card] = pair.get
    })
  }
}

sealed abstract case class TwoPairs private(testCase: TestCase) extends Combination {
  override protected val name: String = "Two Pairs"
}
object TwoPairs {
  def evaluate(list: List[Card]): Option[List[Card]] = ???
  def apply(testCase: TestCase): Option[TwoPairs] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val twoPairs = evaluate(allCards)
    Option.when(Hand.isHandUnique(allCards) && twoPairs.isDefined)(new TwoPairs(testCase) {
      override val combination: List[Card] = twoPairs.get
    })
  }
}

sealed abstract case class ThreeOfKind private(testCase: TestCase) extends Combination {
  override protected val name: String = "Three of Kind"
}
object ThreeOfKind {
  def evaluate(list: List[Card]): Option[List[Card]] = ???
  def apply(testCase: TestCase): Option[ThreeOfKind] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val threeOfKind = evaluate(allCards)
    Option.when(Hand.isHandUnique(allCards) && threeOfKind.isDefined)(new ThreeOfKind(testCase) {
      override val combination: List[Card] = threeOfKind.get
    })
  }
}

sealed abstract case class Flush private(testCase: TestCase) extends Combination {
  override protected val name: String = "Flush"
}
object Flush {
  def evaluate(list: List[Card]): Option[List[Card]] = ???
  def apply(testCase: TestCase): Option[Flush] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val flush = evaluate(allCards)
    Option.when(Hand.isHandUnique(allCards) && flush.isDefined)(new Flush(testCase) {
      override val combination: List[Card] = flush.get
    })
  }
}

sealed abstract case class FullHouse private(testCase: TestCase) extends Combination {
  override protected val name: String = "Full House"
}
object FullHouse {
  def evaluate(list: List[Card]): Option[List[Card]] = ???
  def apply(testCase: TestCase): Option[FullHouse] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val fullHouse = evaluate(allCards)
    Option.when(Hand.isHandUnique(allCards) && fullHouse.isDefined)(new FullHouse(testCase) {
      override val combination: List[Card] = fullHouse.get
    })
  }
}

sealed abstract case class FourOfAKind private(testCase: TestCase) extends Combination {
  override protected val name: String = "Four of a kind"
}
object FourOfAKind {
  def evaluate(list: List[Card]): Option[List[Card]] = ???
  def apply(testCase: TestCase): Option[FourOfAKind] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val fourOfKind = evaluate(allCards)
    Option.when(Hand.isHandUnique(allCards) && fourOfKind.isDefined)(new FourOfAKind(testCase) {
      override val combination: List[Card] = fourOfKind.get
    })
  }
}

sealed abstract case class Straight private(testCase: TestCase) extends Combination {
  override protected val name: String = "Straight"
}
object Straight {
  def evaluate(list: List[Card]): Option[List[Card]] = ???
  def apply(testCase: TestCase): Option[Straight] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val straight = evaluate(allCards)
    Option.when(Hand.isHandUnique(allCards) && straight.isDefined)(new Straight(testCase) {
      override val combination: List[Card] = straight.get
    })
  }
}

sealed abstract case class StraightFlush private(testCase: TestCase) extends Combination {
  override protected val name: String = "Straight"
}
object StraightFlush {
  def evaluate(list: List[Card]): Option[List[Card]] = ???
  def apply(testCase: TestCase): Option[StraightFlush] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val straightFlush = evaluate(allCards)
    Option.when(Hand.isHandUnique(allCards) && straightFlush.isDefined)(new StraightFlush(testCase) {
      override val combination: List[Card] = straightFlush.get
    })
  }
}
