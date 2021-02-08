package com.evolution.homework.basics

import com.evolution.homework.basics.CollectionsHomework._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers._

class CollectionsHomeworkTest extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "running sum" should "be correct" in {
    runningSum(Array(1, 2, 3, 4)) shouldEqual Array(1, 3, 6, 10)
  }

  "shuffle" should "be correct" in {
    shuffle(Array(2, 5, 1, 3, 4, 7), 3) shouldEqual Array(2, 3, 5, 4, 1, 7)
  }

  "max wealth" should "be correct" in {
    maximumWealth(Array(Array(1, 2, 3), Array(2, 1, 3))) shouldEqual 6
  }

  "kids with candies" should "be correct" in {
    kidsWithCandies(Array(2, 3, 5, 1, 3), 3) shouldEqual Array(true, true, true, false, true)
  }

  "max width of vertical area" should "be correct" in {
    maxWidthOfVerticalArea(Array(Array(8, 7), Array(9, 9), Array(7, 4), Array(9, 7))) shouldEqual 1
  }

  "matrixBlockSum1" should "be correct" in {
    matrixBlockSum(Array(Array(1,2,3), Array(4,5,6), Array(7,8,9)), 1) shouldEqual Array(Array(12,21,16), Array(27,45,33), Array(24,39,28))
  }

  "matrixBlockSum2" should "be correct" in {
    matrixBlockSum(
      Array(
        Array(67,64,78),
        Array(99,98,38),
        Array(82,46,46),
        Array(6,52,55),
        Array(55,99,45)), 3) shouldEqual
      Array(
        Array(731,731,731),
        Array(930,930,930),
        Array(930,930,930),
        Array(930,930,930),
        Array(721,721,721)
      )
  }

  "balancedStringSplit" should "be correct" in {
    balancedStringSplit("RLRRLLRLRL") shouldEqual 4
  }

  "maxDepth" should "be correct" in {
    maxDepth("(1+(2*3)+((8)/4))+1") shouldEqual 3
  }

  "sort considering equal values" should "be correct on example 1" in {
    val input = Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)
    val expected = List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)
    val obtained = sortConsideringEqualValues(input)
    obtained shouldEqual expected
  }

  it should "be correct on example 2" in {
    val values = Set("a1", "a2", "b1", "c1", "c2", "d1").map { x =>
      x -> x.head.toInt
    }.toMap

    sortConsideringEqualValues(values) shouldEqual List(
      Set("a1", "a2") -> 'a'.toInt,
      Set("b1") -> 'b'.toInt,
      Set("c1", "c2") -> 'c'.toInt,
      Set("d1") -> 'd'.toInt,
    )
  }

  "scanLeft" should "work correctly on numbers" in {
    val numbers = (1 to 100).toList
    scanLeft(0)(numbers)(_ + _) shouldEqual numbers.scanLeft(0)(_ + _)
  }

  "scanLeft" should "work correctly on letters" in {
    val letters = ('a' to 'z').toList.map(_.toString)
    scanLeft("")(letters)(_ + _) shouldEqual letters.scanLeft("")(_ + _)
  }

  "count" should "pass" in {
    count("aaaabbbcca") shouldEqual List(('a', 4), ('b', 3), ('c', 2), ('a', 1))
  }

  "allSubSetsOfSizeN" should "work correctly on 2 from Set(1, 2, 3)" in {
    allSubsetsOfSizeN(Set(1, 2, 3), 2) shouldEqual Set(Set(1, 2), Set(2, 3), Set(1, 3))
  }

  it should "work correctly" in {
    def fact(num: Int): BigDecimal = {
      (1 to num).map(x => BigDecimal.valueOf(x.toLong)).foldLeft(BigDecimal.valueOf(1)) ((a, b) => a * b)
    }

    val set = (0 until 16).toSet
    (1 to 4) foreach { k =>
      val obtained = allSubsetsOfSizeN(set, k)
      val n = set.size
      val expectedSize = fact(n) / (fact(k) * fact(n - k))
      obtained.size shouldEqual expectedSize.toLong
      obtained.forall(_.size == k) shouldEqual true
    }
  }

  "totalVegetableCost" should "be correct" in {
    totalVegetableCost shouldEqual 5012
  }

  "totalVegetableWeights" should "be correct" in {
    totalVegetableWeights shouldEqual Map(
      "cucumbers" -> 6460,
      "olives" -> 64,
    )
  }

  "allEqual" should "work for lists which are all equal" in {
    allEqual(List("a", "a", "a", "a")) shouldBe true
  }

  "allEqual" should "work on 1 element list" in {
    allEqual(List("a")) shouldBe true
  }

  "allEqual" should "work for lists which are NOT all equal" in {
    allEqual(List("a", "a", "b", "a")) shouldBe false
  }
}
