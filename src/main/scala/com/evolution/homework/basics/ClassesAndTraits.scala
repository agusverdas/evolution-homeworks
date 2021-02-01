package com.evolution.homework.basics

// Homework
//
// Add additional 2D shapes such as triangle and square.
//
// In addition to the 2D shapes classes, add also 3D shapes classes
// (origin, point, sphere, cube, cuboid, 3D triangle - you can add
// others if you think they are a good fit).
//
// Add method `area` to 2D shapes.
//
// Add methods `surfaceArea` and `volume` to 3D shapes.
//
// If some of the implementation involves advanced math, it is OK
// to skip it (leave unimplemented), the primary intent of this
// exercise is modelling using case classes and traits, and not math.

object ClassesAndTraits {
  def distance2D(point1: Point2D, point2: Point2D): Double =
    math.sqrt(math.pow(point2.x - point1.x, 2) + math.pow(point2.y - point1.y, 2))

  def distance3D(point1: Point3D, point2: Point3D): Double =
    math.sqrt(math.pow(point2.x - point1.x, 2) + math.pow(point2.y - point1.y, 2) + math.pow(point2.z - point1.z, 2))

  sealed trait Located

  sealed trait Located2D extends Located {
    def x: Double

    def y: Double
  }

  sealed trait Located3D extends Located2D {
    def z: Double
  }

  sealed trait Bounded

  sealed trait Bounded2D extends Bounded {
    def minX: Double

    def maxX: Double

    def minY: Double

    def maxY: Double
  }

  sealed trait Bounded3D extends Bounded2D {
    def minZ: Double

    def maxZ: Double
  }

  sealed trait Shape extends Located with Bounded

  sealed trait Shape2D extends Shape with Movable2D with Located2D with Bounded2D {
    def area: Double
  }

  sealed trait Shape3D extends Shape with Movable3D with Located3D with Bounded3D {
    def surfaceArea: Double

    def volume: Double
  }

  sealed trait Movable2D {
    def move(dx: Double, dy: Double): Movable2D
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Movable3D
  }

  object Origin2D extends Located2D {
    override def x: Double = 0

    override def y: Double = 0
  }

  object Origin3D extends Located3D {
    override def x: Double = 0

    override def y: Double = 0

    override def z: Double = 0
  }

  final case class Point2D(x: Double, y: Double) extends Shape2D {
    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y

    override def move(dx: Double, dy: Double): Point2D = Point2D(x + dx, y + dy)

    override def area: Double = 0
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y

    override def minZ: Double = z

    override def maxZ: Double = z

    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy, z + dz)

    override def surfaceArea: Double = 0

    override def volume: Double = 0
  }

  sealed abstract case class Circle private(x: Double, y: Double, radius: Double) extends Shape2D {
    override def minX: Double = x - radius

    override def maxX: Double = x + radius

    override def minY: Double = y - radius

    override def maxY: Double = y + radius

    // I can unwrap the option here since the move of valid circle always produces a valid circle
    override def move(dx: Double, dy: Double): Circle = Circle(x + dx, y + dy, radius).get

    override def area: Double = math.Pi * radius * radius
  }

  object Circle {
    def apply(x: Double, y: Double, radius: Double): Option[Circle] = {
      if (radius > 0) Some(new Circle(x, y, radius) {})
      else None
    }
  }

  sealed abstract case class Sphere private(x: Double, y: Double, z: Double, radius: Double) extends Shape3D {
    override def minX: Double = x - radius

    override def maxX: Double = x + radius

    override def minY: Double = y - radius

    override def maxY: Double = y + radius

    override def minZ: Double = z - radius

    override def maxZ: Double = z + radius

    // I can unwrap the option here since the move of valid sphere always produces a valid sphere
    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(x + dx, y + dy, z + dz, radius).get

    override def surfaceArea: Double = 4 * math.Pi * radius * radius

    override def volume: Double = 4.0 / 3.0 * math.Pi * math.pow(radius, 3)
  }


  object Sphere {
    def apply(x: Double, y: Double, z: Double, radius: Double): Option[Sphere] = {
      if (radius > 0) Some(new Sphere(x, y, z, radius) {})
      else None
    }
  }

  // I forced to do it not a case class since I would like to have as simple Square class as possible,
  // please provide me other way to deal with the problem of inheritance disallowance for case classes if you know
  // more elegant way.
  sealed abstract class Rectangle protected(val x: Double, val y: Double, val l: Double, val w: Double) extends Shape2D {
    override def minX: Double = x

    override def maxX: Double = x + l

    override def minY: Double = y - w

    override def maxY: Double = y

    // I can unwrap the option here since the move of valid rectangle always produces a valid rectangle
    override def move(dx: Double, dy: Double): Rectangle = Rectangle(x + dx, y + dy, l, w).get

    override def area: Double = l * w


    def canEqual(other: Any): Boolean = other.isInstanceOf[Rectangle]

    override def equals(other: Any): Boolean = other match {
      case that: Rectangle =>
        (that canEqual this) &&
          x == that.x &&
          y == that.y &&
          l == that.l &&
          w == that.w
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(x, y, l, w)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }


    override def toString = s"Rectangle($x, $y, $l, $w)"
  }

  object Rectangle {
    def apply(x: Double, y: Double, l: Double, w: Double): Option[Rectangle] = {
      if (l > 0 && w > 0) Some(new Rectangle(x, y, l, w) {})
      else None
    }
  }

  sealed abstract case class Square private(override val x: Double, override val y: Double, side: Double)
    extends Rectangle(x, y, side, side) {
    override def move(dx: Double, dy: Double): Square = Square(x + dx, y + dy, side).get
  }

  object Square {
    def apply(x: Double, y: Double, side: Double): Option[Square] = {
      if (side > 0) Some(new Square(x, y, side) {})
      else None
    }
  }

  /*
        z
        ^
        |
        |
        *-----*   The vertex 1 is the constructor argument
       /     /|
      *-----1 |
      |     | *--------->x
      |     |/
      *-----*
     /
    / y

    I forced to do it not a case class since I would like to have as simple Cube class as possible,
    please provide me other way to deal with the problem of inheritance disallowance for case classes if you know
    more elegant way.
   */
  sealed abstract class Cuboid protected(val x: Double, val y: Double, val z: Double, val l: Double, val w: Double, val h: Double) extends Shape3D {
    override def minX: Double = x - l

    override def maxX: Double = x

    override def minY: Double = y - w

    override def maxY: Double = y

    override def minZ: Double = z - h

    override def maxZ: Double = z

    // I can unwrap the option here since the move of valid cuboid always produces a valid cuboid
    override def move(dx: Double, dy: Double, dz: Double): Cuboid = Cuboid(x + dx, y + dy, z + dz, l, w, h).get

    override def surfaceArea: Double = 2 * l * w + 2 * l * h + 2 * w * h

    override def volume: Double = l * w * h

    def canEqual(other: Any): Boolean = other.isInstanceOf[Cuboid]

    override def equals(other: Any): Boolean = other match {
      case that: Cuboid =>
        (that canEqual this) &&
          x == that.x &&
          y == that.y &&
          z == that.z &&
          l == that.l &&
          w == that.w &&
          h == that.h
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(x, y, z, l, w, h)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  object Cuboid {
    def apply(x: Double, y: Double, z: Double, l: Double, w: Double, h: Double): Option[Cuboid] = {
      if (l > 0 && w > 0 && h > 0) Some(new Cuboid(x, y, z, l, w, h) {})
      else None
    }
  }

  sealed abstract case class Cube private(override val x: Double, override val y: Double, override val z: Double, side: Double)
    extends Cuboid(x, y, z, side, side, side) {
    override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(x + dx, y + dy, z + dz, side).get
  }

  object Cube {
    def apply(x: Double, y: Double, z: Double, side: Double): Option[Cube] = {
      if (side > 0) Some(new Cube(x, y, z, side) {})
      else None
    }
  }

  sealed abstract case class Triangle2D private(point1: Point2D, point2: Point2D, point3: Point2D) extends Shape2D {
    override def x: Double = (point1.x + point2.x + point3.x) / 3

    override def y: Double = (point1.y + point2.y + point3.y) / 3

    override def minX: Double = point1.x min point2.x min point3.x

    override def maxX: Double = point1.x max point2.x max point3.x

    override def minY: Double = point1.y min point2.y min point3.y

    override def maxY: Double = point1.y max point2.y max point3.y

    // I can unwrap the option here since the move of valid triangle always produces a valid triangle
    override def move(dx: Double, dy: Double): Triangle2D = Triangle2D(point1.move(dx, dy), point2.move(dx, dy), point3.move(dx, dy)).get

    override def area: Double = {
      val a = distance2D(point1, point2)
      val b = distance2D(point2, point3)
      val c = distance2D(point1, point3)
      val p = 0.5 * (a + b + c)
      math.sqrt(p * (p - a) * (p - b) * (p - c))
    }
  }

  object Triangle2D {
    def apply(point1: Point2D, point2: Point2D, point3: Point2D): Option[Triangle2D] = {
      val a = distance2D(point1, point2)
      val b = distance2D(point2, point3)
      val c = distance2D(point1, point3)
      if (a < b + c && b < a + c && c < a + b) Some(new Triangle2D(point1, point2, point3) {})
      else None
    }
  }

  sealed abstract case class Triangle3D private(point1: Point3D, point2: Point3D, point3: Point3D) extends Shape3D {
    override def x: Double = (point1.x + point2.x + point3.x) / 3

    override def y: Double = (point1.y + point2.y + point3.y) / 3

    override def z: Double = (point1.z + point2.z + point3.z) / 3

    override def minX: Double = point1.x min point2.x min point3.x

    override def maxX: Double = point1.x max point2.x max point3.x

    override def minY: Double = point1.y min point2.y min point3.y

    override def maxY: Double = point1.y max point2.y max point3.y

    override def minZ: Double = point1.z min point2.z min point3.z

    override def maxZ: Double = point1.z max point2.z max point3.z

    // I can unwrap the option here since the move of valid triangle always produces a valid triangle
    override def move(dx: Double, dy: Double, dz: Double): Triangle3D = {
      Triangle3D(point1.move(dx, dy, dz), point2.move(dx, dy, dz), point3.move(dx, dy, dz)).get
    }

    override def surfaceArea: Double = {
      val a = distance3D(point1, point2)
      val b = distance3D(point2, point3)
      val c = distance3D(point1, point3)
      val p = 0.5 * (a + b + c)
      math.sqrt(p * (p - a) * (p - b) * (p - c))
    }

    // It is 0 since in real it is flat shape, but in 3 dimensions
    override def volume: Double = 0
  }

  object Triangle3D {
    def apply(point1: Point3D, point2: Point3D, point3: Point3D): Option[Triangle3D] = {
      val a = distance3D(point1, point2)
      val b = distance3D(point2, point3)
      val c = distance3D(point1, point3)
      if (a < b + c && b < a + c && c < a + b) Some(new Triangle3D(point1, point2, point3) {})
      else None
    }
  }

}
