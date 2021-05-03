package com.evolution.homework.crud

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.toSemigroupKOps
import com.evolution.homework.crud.Repository.{ddl1, ddl2, dml, fetchAllAuthors, fetchAllBooks, fetchAllBooksAndAuthors, fetchAuthorById, fetchBookAndAuthorById, fetchBookById, insertBookWithAuthor, updateBookWithAuthor}
import doobie.ConnectionIO
import doobie.implicits._
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext

object Server extends IOApp {
  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._

  private val transactor = DbTransactor.make[IO]

  private def fetchWithIO[A, F[_]](f: => ConnectionIO[F[A]]): IO[F[A]] = {
    transactor
      .use { xa =>
        for {
          fetch <- f.transact(xa)
        } yield fetch
      }
  }

  private val selectRoutes = HttpRoutes.of[IO] {
    case GET -> Root / "books" =>
      fetchWithIO(fetchAllBooks).flatMap(books => {
        Ok(books)
      })

    case GET -> Root / "book" / UUIDVar(uuid) =>
      fetchWithIO(fetchBookById(uuid).option).flatMap {
        case Some(x) => Ok(x)
        case None => NotFound(s"Book $uuid was not found")
      }

    case GET -> Root / "publications" =>
      fetchWithIO(fetchAllBooksAndAuthors).flatMap(books => {
        Ok(books)
      })

    case GET -> Root / "publication" / UUIDVar(uuid) =>
      fetchWithIO(fetchBookAndAuthorById(uuid)).flatMap {
        case Some(x) => Ok(x)
        case None => NotFound(s"Book $uuid was not found")
      }

    case GET -> Root / "authors" =>
      fetchWithIO(fetchAllAuthors).flatMap(authors => {
        Ok(authors)
      })

    case GET -> Root / "author" / UUIDVar(uuid) =>
      fetchWithIO(fetchAuthorById(uuid)).flatMap {
        case Some(x) => Ok(x)
        case None => NotFound(s"Author $uuid was not found")
      }
  }

  private val createRoutes = HttpRoutes.of[IO] {
    case req @ POST -> Root / "publication" =>
      req.as[BookWithAuthorDTO].flatMap(publication => transactor.use(xa => {
        for {
          insert <- insertBookWithAuthor(publication).transact(xa)
        } yield insert
      }.flatMap {
        case (num, book) if num == 1 => Created(book)
        case _ => InternalServerError("Can't insert a book")
      }))
  }

  private val updateRoutes = HttpRoutes.of[IO] {
        // I assume that by "update" means full update, not patch update
    case req @ PUT -> Root / "publication" / UUIDVar(uuid) =>
      req.as[BookWithAuthorDTO].flatMap(publication => transactor.use(xa => {
        for {
          book <- updateBookWithAuthor(uuid, publication).transact(xa)
        } yield book
      }.flatMap {
        book => Ok(book)
      }.handleErrorWith { ex =>
        NotFound(ex.getMessage)
      }))
  }

  private val httpApp = {
    createRoutes <+> selectRoutes <+> updateRoutes
  }.orNotFound


  def setup(): ConnectionIO[Unit] =
    for {
      _ <- ddl1.update.run
      _ <- ddl2.update.run
      _ <- dml.update.run
    } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- DbTransactor
        .make[IO]
        .use { xa =>
          for {
            _ <- setup().transact(xa)
          } yield ()
        }
        .as(ExitCode.Success)
      _ <- BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9001, host = "localhost")
        .withHttpApp(httpApp)
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    } yield (ExitCode.Success)
  }
}
