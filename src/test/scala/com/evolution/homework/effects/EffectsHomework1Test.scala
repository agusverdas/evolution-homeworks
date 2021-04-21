package com.evolution.homework.effects

import com.evolution.homework.effects.EffectsHomework1.IO
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EffectsHomework1Test extends AnyFlatSpec with Matchers {
  "map" should "apply function to a value" in {
    forAll { (x: Int, y: Int) =>
      val xIO = IO.pure(x)
      val mIO = xIO.map(x => x + y)
      mIO.unsafeRunSync() shouldEqual x + y
    }
  }

  "flatMap" should "apply function to a value" in {
    forAll { (x: Int, y: Int) =>
      val xIO = IO.pure(x)
      val mIO = xIO.flatMap(x => IO.pure(x + y))
      mIO.unsafeRunSync() shouldEqual x + y
    }
  }

  "*>" should "be same as flatMap, but ignore the first result" in {
    forAll { (x: Int, y: Int) =>
      val xIO = IO.pure(x)
      val mIO = xIO *> IO.pure(y)
      mIO.unsafeRunSync() shouldEqual y
    }
  }

  "as" should "should initialize IO" in {
    forAll { (x: Int, y: Int) =>
      val xIO = IO.pure(x)
      val mIO = xIO.as(y)
      mIO.unsafeRunSync() shouldEqual y
    }
  }

  "void" should "should initialize empty IO" in {
    forAll { (x: Int) =>
      val mIO = IO.pure(x).void
      mIO.unsafeRunSync() shouldEqual ()
    }
  }

  "attempt" should "create Either" in {
    forAll { (x: Int) =>
      val mIO = IO.pure(x).attempt
      mIO.unsafeRunSync() shouldEqual Right(x)
    }
  }

  "option" should "create Option" in {
    forAll { (x: Int) =>
      val mIO = IO.pure(x).option
      mIO.unsafeRunSync() shouldEqual Some(x)
    }
  }

  "handleErrorWith" should "handle Error with default value" in {
    val gen = Gen.oneOf(List("s", "asd", "r"))
    forAll(gen) { (x: String) =>
      val mIO = IO.delay(x.toInt).handleErrorWith[Int](_ => IO.pure(1))
      mIO.unsafeRunSync() shouldEqual 1
    }
  }

  "redeem" should "handle or map" in {
    val gen1 = Gen.oneOf(List("s", "asd", "r"))
    val gen2 = Gen.oneOf(List("1", "2", "3"))
    forAll(gen1) { (x: String) =>
      val mIO = IO.delay(x.toInt).redeem(_ => 1, _ => 2)
      mIO.unsafeRunSync() shouldEqual 1
    }
    forAll(gen2) { (x: String) =>
      val mIO = IO.delay(x.toInt).redeem(_ => 1, _ => 2)
      mIO.unsafeRunSync() shouldEqual 2
    }
  }

  "redeemWith" should "handle or map" in {
    val gen1 = Gen.oneOf(List("s", "asd", "r"))
    val gen2 = Gen.oneOf(List("1", "2", "3"))
    forAll(gen1) { (x: String) =>
      val mIO = IO.delay(x.toInt).redeemWith(_ => IO.pure(1), _ => IO.pure(2))
      mIO.unsafeRunSync() shouldEqual 1
    }
    forAll(gen2) { (x: String) =>
      val mIO = IO.delay(x.toInt).redeemWith(_ => IO.pure(1), _ => IO.pure(2))
      mIO.unsafeRunSync() shouldEqual 2
    }
  }

  "raiseError" should "throw an exception" in {
    val mIO = IO.raiseError(new IllegalArgumentException)
    assertThrows[IllegalArgumentException] {
      mIO.unsafeRunSync()
    }
  }

  "raiseWhen" should "throw an exception" in {
    val mIO = IO.raiseWhen(true)(new IllegalArgumentException)
    assertThrows[IllegalArgumentException] {
      mIO.unsafeRunSync()
    }
  }

  "fromEither" should "create IO" in {
    val mIO = IO.fromEither(Left(new IllegalArgumentException))
    assertThrows[IllegalArgumentException] {
      mIO.unsafeRunSync()
    }
  }

  "fromEsither" should "create IO" in {
    IO(println("boom")).flatMap(_ => IO(123))
  }
}
