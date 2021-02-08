package com.evolution.homework.basics

object CollectionsHomework {
  // Leetcode simple tasks
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scanLeft(0)((x, s) => x + s).tail
  }

  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    val (beforeN, afterN) = nums.splitAt(n)
    beforeN zip afterN flatMap (e => e._1 :: e._2 :: Nil)
  }

  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(x => x.sum).max
  }

  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    val max = candies.max
    candies.map(x => (x + extraCandies) >= max)
  }

  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val l = points.map(x => x(0)).sorted
    l.zip(l.tail).map { case (x, y) => y - x }.max
  }

  // Leetcode harder tasks
  // 1. https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def maxDepth(s: String): Int = {
    val chars = s.toList
    chars.foldLeft((0,0)) {
      (acc, it) => it match {
        case '(' => val (seq, depth) = acc; (seq + 1, depth max (seq + 1))
        case ')' => val (seq, depth) = acc; (seq - 1, depth)
        case _ => acc
      }
    }._2
  }

  // 2. https://leetcode.com/problems/split-a-string-in-balanced-strings
  def balancedStringSplit(s: String): Int = {
    val chars = s.toList
    chars.foldLeft((0, 0)){
      (acc, it) => acc match {
        case (-1, balanced) if it == 'R' => (0, balanced + 1)
        case (1, balanced) if it == 'L' => (0, balanced + 1)
        case (queue, balanced) if it == 'L' => (queue - 1, balanced)
        case (queue, balanced) if it == 'R' => (queue + 1, balanced)
      }
    }._2
  }

  // 3. https://leetcode.com/problems/matrix-block-sum/
  // Both solutions are bad, however solution with for cycle is better in terms of memory consumption
  // Todo: enhance if have time
  def matrixBlockSum(mat: Array[Array[Int]], K: Int): Array[Array[Int]] = {
    val m = mat.length
    val n = mat(0).length

    val result = Array.fill(m)(Array.fill(n)(0))

    for (
      x <- 0 until m;
      y <- 0 until n;
      ix <- (x - K) to (x + K)
      if ix >= 0 && ix < m;
      iy <- (y - K) to (y + K)
      if iy >= 0 && iy < n
    ) {
      result(x)(y) += mat(ix)(iy)
    }
    result

    /*val flatMatrix = mat
      .zipWithIndex
      .flatMap { case (a, ind1) => a.zipWithIndex.map { case (b, ind2) => (b, (ind1, ind2)) } }.view
    val miniMatrixMap = flatMatrix
      .map { case (_, (ind1, ind2)) =>
        flatMatrix.filter { case (_, (sind1, sind2)) => sind1 >= ind1 - K && sind1 <= ind1 + K && sind2 >= ind2 - K && sind2 <= ind2 + K }
          .map(_._1)
      }.view
    miniMatrixMap.map(x => x.sum).grouped(n).toArray.map(x => x.toArray)*/
  }

  // DataStructures Homework
  // Homework
  //
  // Implement a special sort which sorts the keys of a map (K) according to their associated
  // values (V).
  //
  // In case of "ties" (equal values) it should group these keys K into Set-s in the results.
  //
  // The input is a map where keys (K) are the values to be sorted and values are their associated numeric
  // values.
  //
  // The output is a list (in ascending order according to the associated `Int` values) of tuples of `Set`-s
  // with values from K, and the associated value V for these values in the `Set`.
  //
  // For example:
  //
  // Input `Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)` should result in
  // output `List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)`.
  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = {
    map
      .groupMap(_._2)(x => x._1)
      .toList
      .sortBy(_._1)
      .map {
        case (i, s) => s.toSet -> i
      }
  }

  // Collections tasks

  // Implement scanLeft (not using scans ofc)
  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] = {
    list match {
      case Nil => List(zero)
      case x :: xs => zero :: scanLeft(f(zero, x))(xs)(f)
    }
  }

  // https://twitter.com/allenholub/status/1357115515672555520/photo/1
  // pass the interview
  def count(s: String): List[(Char, Int)] = {
    def inner(l: List[Char]): List[(Char, Int)] = {
      l match {
        case Nil => Nil
        case x :: _ => (x, l.takeWhile(_ == x).length) :: inner(l.dropWhile(_ == x))
      }
    }

    inner(s.toList)
  }

  // DataStructures tasks

  // Exercise. Write a function that checks if all values in a `List` are equal.
  // Think about what you think your function should return if `list` is empty, and why.
  def allEqual[T](list: List[T]): Boolean =
    list match {
      case Nil => true
      case xs => xs.forall(_ == xs.head)
    }

  val vegetableWeights = Map(
    ("pumpkins", 10),
    ("cucumbers", 20),
    ("olives", 2),
  )

  val vegetablePrices = Map(
    "tomatoes" -> 4,
    "peppers" -> 5,
    "olives" -> 17,
  )

  val vegetableAmounts = Map(
    "tomatoes" -> 17,
    "peppers" -> 234,
    "olives" -> 32,
    "cucumbers" -> 323,
  )

  val tomatoAmount: Int = vegetableAmounts("tomatoes")
  val tomatoAmountOpt: Option[Int] = vegetableAmounts.get("tomatoes")
  val carrotAmountWithDefault: Int = vegetableAmounts.getOrElse("carrots", 0)

  // Exercise. Calculate the total cost of all vegetables, taking vegetable amounts (in units) from
  // `vegetableAmounts` and prices per unit from `vegetablePrices`. Assume the price is 10 if not available
  // in `vegetablePrices`.
  val totalVegetableCost: Int =
  vegetableAmounts.map(e => e._2 * vegetablePrices.getOrElse(e._1, 10)).sum

  // Exercise. Given the vegetable weights (per 1 unit of vegetable) in `vegetableWeights` and vegetable
  // amounts (in units) in `vegetableAmounts`, calculate the total weight per type of vegetable, if known.
  //
  // For example, the total weight of "olives" is 2 * 32 == 64.
  val totalVegetableWeights: Map[String, Int] = // implement here
  for {
    (vegetable, weight) <- vegetableWeights
    amount <- vegetableAmounts.get(vegetable)
  } yield (vegetable, weight * amount)

  // Exercise: Return a set with all subsets of the provided set `set` with `n` elements
  // For example, `allSubsetsOfSizeN(Set(1, 2, 3), 2) == Set(Set(1, 2), Set(2, 3), Set(1, 3))`.
  // Hints for implementation:
  //   - Handle the trivial case where `n == 1`.
  //   - For other `n`, for each `set` element `elem`, generate all subsets of size `n - 1` from the set
  //     that don't include `elem`, and add `elem` to them.
  def allSubsetsOfSizeN[A](set: Set[A], n: Int): Set[Set[A]] =
    n match {
      case 1 => set.map(Set(_))
      case _ => for {
        x <- set
        s <- allSubsetsOfSizeN(set - x, n - 1)
      } yield s + x
    }
}
