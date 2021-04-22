package com.evolution.homework.http

import cats.data.Kleisli
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.syntax.all._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.util.Random

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// The exact protocol and message format to use is not specified and should be designed while working on the task.

sealed trait SessionError
object SessionError {
  case object SessionAlreadyEnded extends SessionError
}

final case class GameConditions(min: Int, max: Int)
final case class GameGuess(guess: Int)
final case class Game(conditions: GameConditions, tries: Int, server: Int)

case class Session(sessionId: String, game: Game)
trait SessionWarehouse[F[_]] {
  def get(sessionId: UUID): F[Option[Session]]
  def put(session: Session): F[Unit]
}

object SessionsWarehouse {
  def of[F[_]: Sync]: F[SessionWarehouse[F]] = {
    Ref.of(Map.empty[String, Session]).map { state =>
      new SessionWarehouse[F] {
        override def get(sessionId: UUID): F[Option[Session]] = {
          for {
            sessions <- state.get
            _ <- Sync[F].delay(println(sessions))
            session = sessions.get(sessionId.toString)
          } yield session
        }

        override def put(session: Session): F[Unit] = state.update { sessions =>
          println(sessions)
          if (session.game.tries == 0) {
            println(s"Remove : $sessions")
            sessions.removed(session.sessionId)
          }
          else {
            println(s"Put : $sessions")
            sessions + (session.sessionId -> session)
          }
        }
      }
    }
  }
}

object GuessServer extends IOApp {
  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._
  val Session = "guessServerChatSession"
  val CookieMaxAge: Option[Long] = Some(300L)
  val MaxTries = 5
  val random = new Random()

  private def gameRoutes(sessions: SessionWarehouse[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "start" =>
      req.as[GameConditions].flatMap { conditions =>
        val sessionIDOpt: Option[String] = req.cookies.find(_.name == Session).map(_.content)
        sessionIDOpt match {
          case Some(_) => BadRequest("The game has already started for you!")
          case None =>
            for {
              uuid <- IO.pure(UUID.randomUUID())
              serverValue = random.between(conditions.min, conditions.max)
              _ <- sessions.put(new Session(uuid.toString, Game(conditions, MaxTries, serverValue)))
              ses <- sessions.get(uuid)
              _ <- IO(println(s"Session : ${ses}"))
              resp <- Ok().map(resp =>
                resp.addCookie(ResponseCookie(Session, uuid.toString, maxAge = CookieMaxAge))
              )
            } yield resp
        }
      }
    case req @ POST -> Root / "guess" =>
      req.as[GameGuess].flatMap { gameGuess =>
        val sessionIDOpt: Option[String] = req.cookies.find(_.name == Session).map(_.content)
        sessionIDOpt match {
          case Some(sessionID) =>
            val sessionT = for {
              _ <- IO(println(sessions))
              session <- sessions.get(UUID.fromString(sessionID))
            } yield session
            sessionT.flatMap(optSession => optSession match {
              case None => BadRequest("There is no such session! You are a hacker or your session has expired!")
              case Some(session) =>
                val game = session.game
                if (game.tries == 0) BadRequest("You don't have any more tries")
                else if (game.server < gameGuess.guess) {
                  val putSession = for {
                    _ <- sessions.put(session.copy(game = game.copy(tries = game.tries - 1)))
                  } yield ()
                  putSession *> BadRequest("Number is too large")
                } else if (game.server < gameGuess.guess) {
                  val putSession = for {
                    _ <- sessions.put(session.copy(game = game.copy(tries = game.tries - 1)))
                  } yield ()
                  putSession *> BadRequest("Number is too small")
                }
                else {
                  val putSession = for {
                    _ <- sessions.put(session.copy(game = game.copy(tries = 0)))
                  } yield ()
                  putSession *>  Ok("You won!")
                }
            })
          case None => BadRequest("You didn't start a game!")
        }
      }

  }

  private[http] def httpApp(sessions: SessionWarehouse[IO]): Kleisli[IO, Request[IO], Response[IO]] = {
    gameRoutes(sessions)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      state <- SessionsWarehouse.of[IO]
      _ <- BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9001, host = "localhost")
        .withHttpApp(httpApp(state))
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    } yield (ExitCode.Success)
  }
}