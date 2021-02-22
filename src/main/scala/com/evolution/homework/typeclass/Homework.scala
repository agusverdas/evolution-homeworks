package com.evolution.homework.typeclass

object Task1 {
  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = (m1, m2) => m1.amount.compareTo(m2.amount)
}

object Task2 {
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  // TODO: create Show instance for User
  implicit val showUser: Show[User] = x => s"{ id : ${x.id}, name : ${x.name} }"

  // TODO: create syntax for Show so i can do User("1", "Oleg").show
  implicit class RichUser(user: User) {
    def show(implicit showUser: Show[User]): String = showUser.show(user)
  }

  User("1", "Oleg").show
}

object Task3 {
  type Error = String
  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)

  // TODO: create Parse instance for User
  implicit val parseUser: Parse[User] = { x =>
    val parsed  = x.trim.split(",")
    if (parsed.size < 2) Left(s"Not enough values to create user : ${parsed.mkString("Array(", ", ", ")")}")
    else if (parsed.size > 2) Left(s"Too much values for user creation : ${parsed.mkString("Array(", ", ", ")")}")
    else {
      Right(User(parsed(0), parsed(1)))
    }
  }

  // TODO: create syntax for Parse so i can do "lalala".parse[User] (and get an error because it is obviously not a User)
  implicit class Parser(str : String) {
    def parse[T](implicit parseI: Parse[T]): Either[Error, T] = parseI.parse(str)
  }

  "lalala".parse[User]
}

object Task4 {
  // TODO: design a typesafe equals so i can do a === b, but it won't compile if a and b are of different types
  // define the typeclass (think of a method signature)
  // remember `a method b` is `a.method(b)`

  trait Eq[T] {
    def === (x: T, y: T): Boolean
  }

  implicit val eqString: Eq[String] = _.equals(_)

  implicit class RichEq[T](x : T) {
    def ===(y: T)(implicit eq: Eq[T]): Boolean = x === y
  }
}

object AdvancedHomework {
  // TODO: create a typeclass for flatMap method

  trait Monad[M[_]] {
    def flatMap[A, B](i: M[A])(kleisli: A => M[B]): M[B]
  }

  implicit class MonadSyntax[F[_], A](i: F[A]) {
    def >>=[B](kleisli: A => F[B])(implicit monad: Monad[F]): F[B] = monad.flatMap(i)(kleisli)
  }

  case class Identity[T](x : T)
  implicit val identityMonad: Monad[Identity] = new Monad[Identity] {
    override def flatMap[A, B](i: Identity[A])(kleisli: A => Identity[B]): Identity[B] = kleisli(i.x)
  }
  def mul2: Int => Identity[Int] = (x: Int) => Identity(x * 2)
  Identity(1) >>= mul2
}


