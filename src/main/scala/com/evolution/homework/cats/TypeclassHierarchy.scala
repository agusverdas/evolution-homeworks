package com.evolution.homework.cats

import cats.{Functor, Monad, Semigroupal}

object Monads {

  val optionMonad: Monad[Option] = new Monad[Option] {
    // implement me
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case None => None
      case Some(x) => f(x)
    }

    // implement me
    override def pure[A](x: A): Option[A] = Option(x)

    // cats-only method, don't implement me
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = ???
  }

  def eitherMonad[T]: Monad[Either[T, *]] = new Monad[Either[T, *]] {
    // implement me
    override def flatMap[A, B](fa: Either[T, A])(f: A => Either[T, B]): Either[T, B] = fa match {
      case Left(t) => Left(t)
      case Right(a) => f(a)
    }

    // implement me
    override def pure[A](x: A): Either[T, A] = Right(x)

    // cats-only method, don't implement me
    override def tailRecM[A, B](a: A)(f: A => Either[T, Either[A, B]]): Either[T, B] = ???
  }

  def functionMonad[T]: Monad[T => *] = new Monad[T => *] {
    // implement me
    override def flatMap[A, B](fa: T => A)(f: A => T => B): T => B = t => f(fa(t))(t)

    // implement me
    override def pure[A](x: A): T => A = _ => x

    // cats-only method, don't implement me
    override def tailRecM[A, B](a: A)(f: A => T => Either[A, B]): T => B = ???
  }
}

object Excercises {

  trait Applicative[F[_]] extends Functor[F] with Semigroupal[F] {
    def unit[A](a: => A): F[A]

    // implement methods using each other
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fa, fab){ (a: A, f: A => B) => f(a) }

    def map[A,B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

    def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldLeft[F[List[B]]](unit(List.empty[B])){
      (acc: F[List[B]], x: A) => {
        val fa: F[B] = f(x)
        map2(fa, acc) { (elem: B, inAcc: List[B]) => elem :: inAcc }
      }
    }
  }

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]

    // implement methods using each other
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => unit(f(x)))

    def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

    def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = flatMap(ma)(x => flatMap(mb)(y => unit(f(x, y))))
  }

}
