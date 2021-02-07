package com.evolution.homework.basics

import com.evolution.homework.basics.ControlStructuresHomework.Command.{Divide, Sum}
import com.evolution.homework.basics.ControlStructuresHomework._
import org.scalatest.Inspectors.forEvery
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ControlStructuresHomeworkTest extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "parse command" should "be incorrect" in {
    forEvery(List("unknown", "x", "1.02f")) { x =>
      parseCommand(x) shouldEqual Left(ErrorMessage(s"Unknown command : $x"))
    }
  }

  "parse command divide" should "be correct" in {
    val dd = List[(Double, Double)]((4, 5.0), (1, 2), (-1, 2), (1.6, 0))
    forEvery(dd) { t =>
      val (dividend, divisor) = t
      val in = s"divide $dividend $divisor"
      parseCommand(in) shouldEqual Right(Divide(dividend, divisor))
    }
  }

  "parse list" should "be correct" in {
    val ie = List(
      (List("1", "2", "3"), List(1.0, 2.0, 3.0)),
      (List("1", "2", "3"), List(1.0, 2.0, 3.0)),
      (List("-1.0", "1.0"), List(-1.0, 1.0)))
    forEvery(ie) { t =>
      val (in, out) = t
      parseList(in) shouldEqual Right(out)
    }
  }

  "parse list" should "be incorrect" in {
    def errMes: String => String = (x: String) => s"For input string: ${"\"" + x + "\""}"

    val ie = List[(List[String], Left[_, ErrorMessage])](
      (List("error", "2", "3"), Left(ErrorMessage(List(errMes("error")).mkString("; ")))),
      (List("one", "two"), Left(ErrorMessage(List(errMes("one"), errMes("two")).mkString("; ")))))
    forEvery(ie) { t =>
      val (in, expected) = t
      parseList(in) shouldEqual expected
    }
  }

  "calculate" should "be correct" in {
    calculate(Divide(1.0, 2.0)) shouldEqual Right(DivisionResult(1.0, 2.0, 1.0 / 2.0))
    calculate(Divide(1.0, 0)) shouldEqual Left(ErrorMessage("Division by 0"))
    calculate(Sum(List(1.0, 0))) shouldEqual Right(SumResult(List(1.0, 0.0), 1.0))
  }
}
