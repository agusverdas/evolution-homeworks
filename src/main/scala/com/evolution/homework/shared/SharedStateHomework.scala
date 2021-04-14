package com.evolution.homework.shared

import cats.Monad
import cats.data.OptionT
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Timer}
import cats.syntax.all._

import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 */
object SharedStateHomework extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_] : Clock : Monad, K, V](
                                              state: Ref[F, Map[K, (Long, V)]],
                                              expiresIn: FiniteDuration
                                            ) extends Cache[F, K, V] {
    private val unitsOfMeasure = MICROSECONDS

    def get(key: K): F[Option[V]] = {
      val optionT: OptionT[F, V] = for {
        map <- OptionT.liftF(state.get)
        (time, value) <- OptionT(Monad[F].pure(map.get(key)))
        currentTime <- OptionT.liftF(Clock[F].realTime(unitsOfMeasure))
        if time + expiresIn.toUnit(unitsOfMeasure).toLong >= currentTime
      } yield value
      optionT.value
    }

    def put(key: K, value: V): F[Unit] = for {
      currentTime <- Clock[F].realTime(unitsOfMeasure)
      _ <- state.update(map => map + (key -> (currentTime -> value)))
    } yield ()

    def checkOnExpirations(): F[Unit] = {
      for {
        currentTime <- Clock[F].realTime(unitsOfMeasure)
        _ <- state.update { map =>
          map.filter { case (_, (timestamp, _)) =>
            val expirationTime = timestamp + expiresIn.toUnit(unitsOfMeasure).toLong
            expirationTime <= currentTime
          }
        }
      } yield ()
    }
  }


  object Cache {
    def of[F[_] : Clock : Monad, K, V](
                                expiresIn: FiniteDuration,
                                checkOnExpirationsEvery: FiniteDuration
                              )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {
      for {
        ref <- Ref.of(Map.empty[K, (Long, V)])
        cache <- Monad[F].pure(new RefCache[F, K, V](ref, expiresIn))
        _ <- C.start(T.sleep(checkOnExpirationsEvery).flatMap(_ => {
          cache.checkOnExpirations()
        }).foreverM[Unit])
      } yield cache
    }


  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
    } yield ExitCode.Success
  }
}