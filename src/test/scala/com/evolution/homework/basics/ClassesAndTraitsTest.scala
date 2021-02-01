package com.evolution.homework.basics


import cats.implicits._
import com.evolution.homework.basics.ClassesAndTraits._
import org.scalacheck.Gen._
import org.scalacheck.cats.implicits._
import org.scalactic.TolerantNumerics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class ClassesAndTraitsTest extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "Circle" should "be correct" in {
    val intervalGen = choose(1.0, 10)
    val gen = (intervalGen, intervalGen, intervalGen).tupled

    forAll(gen) { case (x, y, r) =>
      val circle = Circle(x, y, r)
      circle.map(_.minX) shouldEqual Some(x - r)
      circle.map(_.maxX) shouldEqual Some(x + r)
      circle.map(_.minY) shouldEqual Some(y - r)
      circle.map(_.maxY) shouldEqual Some(y + r)
    }
  }

  "Origin3D" should "be correct" in {
    Origin3D.x shouldEqual 0
    Origin3D.y shouldEqual 0
    Origin3D.z shouldEqual 0
  }

  "Point3D" should "be correct" in {
    val intervalGen = choose(-10, 10)
    val gen = (intervalGen, intervalGen, intervalGen).tupled

    forAll(gen) { case (x, y, z) =>
      val point = Point3D(x, y, z)
      point.maxX shouldEqual x
      point.minX shouldEqual x
      point.maxY shouldEqual y
      point.minY shouldEqual y
      point.maxZ shouldEqual z
      point.minZ shouldEqual z
      point.surfaceArea shouldEqual 0
      point.volume shouldEqual 0
      point.move(x, y, z) shouldEqual Point3D(2 * x, 2 * y, 2 * z)
      point.move(0, 0, 0) shouldEqual point
    }
  }

  "Sphere" should "be correct" in {
    val intervalGenPoint = choose(-10, 10)
    val intervalGenRadius = choose(1.0, 10)
    val gen = (intervalGenPoint, intervalGenPoint, intervalGenPoint, intervalGenRadius).tupled

    forAll(gen) { case (x, y, z, r) =>
      val sphere: Option[Sphere] = Sphere(x, y, z, r)
      sphere.map(_.surfaceArea) shouldEqual Some(4 * math.Pi * r * r)
      sphere.map(_.volume) shouldEqual Some(4.0 / 3.0 * math.Pi * math.pow(r, 3))
      sphere.map(_.move(x, y, z)) shouldEqual Sphere(2 * x, 2 * y, 2 * z, r)
      sphere.map(_.move(0, 0, 0)) shouldEqual sphere
    }
  }

  "Sphere" should "be incorrect" in {
    val intervalGenPoint = choose(-10, 10)
    val intervalGenRadius = choose(-100, 0)
    val gen = (intervalGenPoint, intervalGenPoint, intervalGenPoint, intervalGenRadius).tupled

    forAll(gen) { case (x, y, z, r) =>
      val sphere: Option[Sphere] = Sphere(x, y, z, r)
      sphere shouldEqual None
    }
  }

  "Rectangle" should "be correct" in {
    val intervalGenPoint = choose(-10, 10)
    val intervalGenSide = choose(1, 100)
    val gen = (intervalGenPoint, intervalGenPoint, intervalGenSide, intervalGenSide).tupled

    forAll(gen) { case (x, y, l, w) =>
      val rectangle: Option[Rectangle] = Rectangle(x, y, l, w)
      rectangle.map(_.area) shouldEqual Some(l * w)
      rectangle.map(_.move(x, y)) shouldEqual Rectangle(2 * x, 2 * y, l, w)
      rectangle.map(_.move(0, 0)) shouldEqual Rectangle(x, y, l, w)
    }
  }

  "Rectangle" should "be incorect" in {
    val intervalGenPoint = choose(-10, 10)
    val intervalGenSide = choose(-100, 0)
    val gen = (intervalGenPoint, intervalGenPoint, intervalGenSide, intervalGenSide).tupled
    forAll(gen) { case (x, y, l, w) =>
      val rectangle: Option[Rectangle] = Rectangle(x, y, l, w)
      rectangle shouldEqual None
    }
  }

  "Square" should "be correct" in {
    val intervalGenPoint = choose(-10, 10)
    val intervalGenSide = choose(1, 100)
    val gen = (intervalGenPoint, intervalGenPoint, intervalGenSide).tupled

    forAll(gen) { case (x, y, side) =>
      val square: Option[Square] = Square(x, y, side)
      square.map(_.area) shouldEqual Some(side * side)
      square.map(_.move(x, y)) shouldEqual Square(2 * x, 2 * y, side)
      square.map(_.move(0, 0)) shouldEqual Square(x, y, side)
      square shouldEqual Rectangle(x, y, side, side)
    }
  }

  "Square" should "be incorrect" in {
    val intervalGenPoint = choose(-10, 10)
    val intervalGenSide = choose(-100, 0)
    val gen = (intervalGenPoint, intervalGenPoint, intervalGenSide).tupled

    forAll(gen) { case (x, y, side) =>
      val square: Option[Square] = Square(x, y, side)
      square shouldEqual None
    }
  }

  "Cuboid" should "be correct" in {
    val intervalGenPoint = choose(-10, 10)
    val intervalGenSide = choose(1, 100)
    val gen = (intervalGenPoint, intervalGenPoint, intervalGenPoint, intervalGenSide, intervalGenSide, intervalGenSide).tupled

    forAll(gen) { case (x, y, z, l, w, h) =>
      val cuboid: Option[Cuboid] = Cuboid(x, y, z, l, w, h)
      cuboid.map(_.surfaceArea) shouldEqual Some(2 * l * w + 2 * l * h + 2 * w * h)
      cuboid.map(_.volume) shouldEqual Some(l * w * h)
      cuboid.map(_.move(x, y, z)) shouldEqual Cuboid(2 * x, 2 * y, 2 * z, l, w, h)
      cuboid.map(_.move(0, 0, 0)) shouldEqual Cuboid(x, y, z, l, w, h)
    }
  }

  "Cuboid" should "be incorrect" in {
    val intervalGenPoint = choose(-10, 10)
    val intervalGenSide = choose(-100, 0)
    val gen = (intervalGenPoint, intervalGenPoint, intervalGenPoint, intervalGenSide, intervalGenSide, intervalGenSide).tupled

    forAll(gen) { case (x, y, z, l, w, h) =>
      val cuboid: Option[Cuboid] = Cuboid(x, y, z, l, w, h)
      cuboid shouldEqual None
    }
  }

  "Cube" should "be correct" in {
    val intervalGenPoint = choose(-10, 10)
    val intervalGenSide = choose(1, 100)
    val gen = (intervalGenPoint, intervalGenPoint, intervalGenPoint, intervalGenSide).tupled

    forAll(gen) { case (x, y, z, side) =>
      val cube: Option[Cube] = Cube(x, y, z, side)
      cube.map(_.surfaceArea) shouldEqual Some(6 * side * side)
      cube.map(_.volume) shouldEqual Some(math.pow(side, 3))
      cube.map(_.move(x, y, z)) shouldEqual Cuboid(2 * x, 2 * y, 2 * z, side, side, side)
      cube shouldEqual Cuboid(x, y, z, side, side, side)
    }
  }

  "Cube" should "be incorrect" in {
    val intervalGenPoint = choose(-10, 10)
    val intervalGenSide = choose(-100, 0)
    val gen = (intervalGenPoint, intervalGenPoint, intervalGenPoint, intervalGenSide).tupled

    forAll(gen) { case (x, y, z, side) =>
      val cube: Option[Cube] = Cube(x, y, z, side)
      cube shouldEqual None
    }
  }

  "Triangle2D" should "be correct" in {
    // TODO: can be improved
    // Since idk how to create dependencies in generators and generate values based on solving equation, i provide a subset
    val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.00000000000001)
    val x1 = 10
    val x2 = 7
    val x3 = 4
    val y1 = 5
    val y2 = 3
    val y3 = 9
    val point1 = Point2D(x1, y1)
    val point2 = Point2D(x2, y2)
    val point3 = Point2D(x3, y3)
    val triangle: Option[Triangle2D] = Triangle2D(point1, point2, point3)
    assert(doubleEq.areEqual(triangle.map(_.area).get, 12.0))
    triangle.map(_.move(0, 0)) shouldEqual triangle
    val deltaPoint1 = Point2D(2 * x1, 2 * y1)
    val deltaPoint2 = Point2D(x2 + x1, y2 + y1)
    val deltaPoint3 = Point2D(x3 + x1, y3 + y1)
    triangle.map(_.move(x1, y1)) shouldEqual Triangle2D(deltaPoint1, deltaPoint2, deltaPoint3)
  }

  "Triangle2D" should "be incorrect" in {
    val intervalGenPoint = choose(-10, 10)
    val zeroGen = choose(0, 0)
    val gen = (intervalGenPoint, zeroGen, intervalGenPoint, zeroGen, intervalGenPoint, zeroGen).tupled

    forAll(gen) { case (x1, y1, x2, y2, x3, y3) =>
      val point1 = Point2D(x1, y1)
      val point2 = Point2D(x2, y2)
      val point3 = Point2D(x3, y3)
      val triangle: Option[Triangle2D] = Triangle2D(point1, point2, point3)
      triangle shouldEqual None
    }
  }

  "Triangle3D" should "be correct" in {
    // TODO: can be improved
    // Since idk how to create dependencies in generators and generate values based on solving equation, i provide a subset
    val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.00000000000001)
    val x1 = 10
    val x2 = 7
    val x3 = 4
    val y1 = 5
    val y2 = 3
    val y3 = 9
    val z1 = 4
    val z2 = 5
    val z3 = 7
    val point1 = Point3D(x1, y1, z1)
    val point2 = Point3D(x2, y2, z2)
    val point3 = Point3D(x3, y3, z3)
    val triangle: Option[Triangle3D] = Triangle3D(point1, point2, point3)
    triangle.map(_.volume) shouldEqual Some(0)
    assert(doubleEq.areEqual(triangle.map(_.surfaceArea).get, 13.086252328302399))
    triangle.map(_.move(0, 0, 0)) shouldEqual triangle
    val deltaPoint1 = Point3D(2 * x1, 2 * y1, 2 * z1)
    val deltaPoint2 = Point3D(x2 + x1, y2 + y1, z2 + z1)
    val deltaPoint3 = Point3D(x3 + x1, y3 + y1, z3 + z1)
    triangle.map(_.move(x1, y1, z1)) shouldEqual Triangle3D(deltaPoint1, deltaPoint2, deltaPoint3)
  }

  "Triangle3D" should "be incorrect" in {
    val intervalGenPoint = choose(-10, 10)
    val zeroGen = choose(0, 0)
    val gen = (intervalGenPoint, zeroGen, zeroGen, intervalGenPoint, zeroGen, zeroGen,
      intervalGenPoint, zeroGen, zeroGen).tupled

    forAll(gen) { case (x1, y1, z1, x2, y2, z2, x3, y3, z3) =>
      val point1 = Point3D(x1, y1, z1)
      val point2 = Point3D(x2, y2, z2)
      val point3 = Point3D(x3, y3, z3)
      val triangle: Option[Triangle3D] = Triangle3D(point1, point2, point3)
      triangle shouldEqual None
    }
  }
}
