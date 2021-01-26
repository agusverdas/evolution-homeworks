package com.evolution.homework.basics

import com.evolution.homework.basics.Basics.{gcd, lcm}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BasicsTest extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "lcm(0, 1)" should "throw exception" in {
    assertThrows[UnsupportedOperationException] {
      lcm(0, 1)
    }
  }

  "lcm(7919, 7907)" should "be 62615533" in {
    lcm(7919, 7907) shouldEqual 62615533
  }

  "lcm(23, 76)" should "be 1748" in {
    lcm(23, 76) shouldEqual 1748
  }

  "lcm(-23, 76)" should "be 1748" in {
    lcm(-23, 76) shouldEqual 1748
  }

  it should "work for all numbers" in {
    forAll { (a: Int, b: Int) =>
      gcd(a, b) shouldEqual BigInt(a).gcd(BigInt(b))
    }
  }
}
