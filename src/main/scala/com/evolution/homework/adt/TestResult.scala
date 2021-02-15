package com.evolution.homework.adt

sealed case class TestResult(list: List[Combination]) {
  private val sortedCombinations: List[Combination] = ??? // Here need to Ordering class to be implemented and call list.sorted

  // For printing it should be like this (Again need Ordering)
  /*def printableString(): String = {
    if (sortedCombinations.length == 1) sortedCombinations.head
    else sortedCombinations.tail.foldLeft((sortedCombinations.head, s"${sortedCombinations.head}")){
      case ((prev, string), cur) => if (prev == cur) (cur, string + "=" + cur) else (cur, string + " " + cur)
    }
  }*/
}
