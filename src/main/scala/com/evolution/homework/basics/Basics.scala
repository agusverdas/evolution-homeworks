package com.evolution.homework.basics


import scala.annotation.tailrec

object Basics {
  def lcm(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) 0
    else (a.abs / gcd(a, b)) * b.abs
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a.abs
    else gcd(b, a % b)
  }
}
