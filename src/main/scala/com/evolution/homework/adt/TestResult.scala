package com.evolution.homework.adt

sealed abstract case class TestResult private(list: List[Combination]) {
  private val sortedCombinations: List[Combination] = ??? // Here need to Ordering class to be implemented and call list.sorted

  // For printing it should be like this (Again need Ordering)
  /*def printableString(): String = {
    if (sortedCombinations.length == 1) sortedCombinations.head
    else sortedCombinations.tail.foldLeft((sortedCombinations.head, s"${sortedCombinations.head}")){
      case ((prev, string), cur) => if (prev == cur) (cur, string + "=" + cur) else (cur, string + " " + cur)
    }
  }*/
}

object TestResult {
  def apply(list: List[Combination]): Option[TestResult] = {
    Option.when(list.nonEmpty)(new TestResult(list) {})
  }
}
