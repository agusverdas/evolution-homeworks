package com.evolution.homework.testing

import com.evolution.homework.testing.Calculator.Operation
import org.scalatest.freespec.AnyFreeSpec

class CalculatorTest extends AnyFreeSpec {

  "calculator" - {
    "enters the number correctly" in {
      val calculator = Calculator()
      assert(calculator.enter(1) == Right(Calculator(0, 1, None)))
      assert(calculator.enter(7) == Right(Calculator(0, 7, None)))
      assert(calculator.enter(12) == Left("digit out of range"))
    }

    "minus works correctly" in {
      val calculator = Calculator()
      val minus = for {
        _1 <- calculator.enter(1)
        _12 <- _1.enter(2)
        _minus = _12.minus
        _3 <- _minus.flatMap(_.enter(3))
        _34 <- _3.enter(4)
      } yield _34.calculate
      assert(minus.flatten == Right(Calculator(-22, -22, Some(Operation.Minus), evaluated = true)))
      val minus2 = for {
        _1 <- calculator.enter(1)
        _12 <- _1.enter(2)
        _minus = _12.minus
        _3 <- _minus.flatMap(_.enter(3))
        _34 <- _3.enter(4)
        _minus2 = _34.minus
        _1 <- _minus2.flatMap(_.enter(1))
      } yield _1.calculate
      assert(minus2.flatten == Right(Calculator(-23, -23, Some(Operation.Minus), evaluated = true)))
    }

    "plus works correctly" in {
      val calculator = Calculator()
      val plus = for {
        _1 <- calculator.enter(1)
        _12 <- _1.enter(2)
        _minus = _12.minus
        _3 <- _minus.flatMap(_.enter(3))
        _34 <- _3.enter(4)
        _plus = _34.plus
        _3 <- _plus.flatMap(_.enter(3))
        _34 <- _3.enter(4)
      } yield _34.calculate
      assert(plus.flatten == Right(Calculator(12, 12, Some(Operation.Plus), evaluated = true)))
      val plus2 = for {
        _1 <- calculator.enter(1)
        _14 <- _1.enter(4)
        _plus = _14.plus
        _3 <- _plus.flatMap(_.enter(3))
      } yield _3.calculate
      assert(plus2.flatten == Right(Calculator(17, 17, Some(Operation.Plus), evaluated = true)))
    }

    "multiply works correctly" in {
      val calculator = Calculator()
      val mul = for {
        _2 <- calculator.enter(2)
        _22 <- _2.enter(2)
        _mul = _22.mul
        _3 <- _mul.flatMap(_.enter(3))
      } yield _3.calculate
      assert(mul.flatten == Right(Calculator(66, 66, Some(Operation.Multiply), evaluated = true)))
    }

    "div works correctly" in {
      val calculator = Calculator()
      val div = for {
        _2 <- calculator.enter(2)
        _22 <- _2.enter(2)
        _div = _22.div
        _2 <- _div.flatMap(_.enter(2))
      } yield _2.calculate
      assert(div.flatten == Right(Calculator(11, 11, Some(Operation.Div), evaluated = true)))
    }

    "mod works correctly" in {
      val calculator = Calculator()
      val mod = for {
        _2 <- calculator.enter(2)
        _22 <- _2.enter(2)
        _mod = _22.mod
        _2 <- _mod.flatMap(_.enter(2))
      } yield _2.calculate
      assert(mod.flatten == Right(Calculator(0, 0, Some(Operation.Mod), evaluated = true)))
    }

    "does nothing" - {
      "when you just repeat pressing `=`" in {
        val calculator = Calculator()
        assert(calculator.calculate.flatMap(_.calculate).flatMap(_.calculate).flatMap(_.calculate) == Right(calculator))
      }
    }
  }
}
