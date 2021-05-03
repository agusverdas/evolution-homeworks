package com.evolution.homework.crud

import cats.Applicative
import cats.data.NonEmptyList
import cats.free.Free
import cats.implicits.catsSyntaxApplicativeId
import com.evolution.homework.crud.DbCommon.{createTableAuthorsSql, createTableBooksSql, populateDataSql}
import doobie.free.connection
import doobie.implicits._
import doobie.implicits.javatime._
import doobie.{ConnectionIO, Fragment, Fragments, Meta}

import java.time.{LocalDate, Year}
import java.util.UUID

object Repository {
  implicit val uuidMeta: Meta[UUID] = Meta[String].timap(UUID.fromString)(_.toString)
  implicit val yearMeta: Meta[Year] = Meta[Int].timap(Year.of)(_.getValue)

  // setup
  val ddl1 = Fragment.const(createTableAuthorsSql)
  val ddl2 = Fragment.const(createTableBooksSql)
  val dml = Fragment.const(populateDataSql)

  def setup(): ConnectionIO[Unit] =
    for {
      _ <- ddl1.update.run
      _ <- ddl2.update.run
      _ <- dml.update.run
    } yield ()

  val authors: Fragment =
    fr"SELECT id, name, birthday FROM authors"

  val books: Fragment =
    fr"SELECT id, author, title, year FROM books"

  def fetchAuthorById(id: UUID): ConnectionIO[Option[Author]] =
    (authors ++ fr"WHERE id = $id").query[Author].option

  val fetchBooksAndAuthor: Fragment =
    fr"""SELECT b.id, a.id, a.name, a.birthday, b.title, b.year FROM books b
            INNER JOIN authors a ON b.author = a.id"""

  def fetchAllBooks: doobie.ConnectionIO[List[Book]] = {
    books.query[Book].to[List]
  }

  def fetchAllAuthors: doobie.ConnectionIO[List[Author]] = {
    authors.query[Author].to[List]
  }

  def fetchAllBooksAndAuthors: doobie.ConnectionIO[List[BookWithAuthor]] = {
    fetchBooksAndAuthor.query[BookWithAuthor].to[List]
  }

  def fetchBooksByAuthors(ids: NonEmptyList[UUID]): doobie.Query0[BookWithAuthor] = {
    val queryBooks = fetchBooksAndAuthor ++ fr"WHERE" ++ Fragments.in(fr"author", ids)
    queryBooks.query[BookWithAuthor]
  }

  def fetchBookById(id: UUID): doobie.Query0[Book] = {
    val selectBook = books ++ fr"WHERE id = $id"
    selectBook.query[Book]
  }

  def fetchAuthorByName(name: String): ConnectionIO[Option[Author]] = {
    val selectAuthor = authors ++ fr"WHERE name = $name"
    selectAuthor.query[Author].option
  }

  def fetchBookAndAuthorById(id: UUID): ConnectionIO[Option[BookWithAuthor]] = {
    val selectBookAndAuthor = fetchBooksAndAuthor ++ fr"WHERE b.id = $id"
    selectBookAndAuthor.query[BookWithAuthor].option
  }

  def fetchBooksByYear(year: Int): doobie.ConnectionIO[List[Book]] = {
    val selectBooks = books ++ fr"WHERE year = $year"
    selectBooks.query[Book].to[List]
  }

  def fetchBooksByYearRange(yearFrom: Int, yearTo: Int): doobie.ConnectionIO[List[Book]] = {
    val selectBooks = books ++ fr"WHERE year BETWEEN $yearFrom AND $yearTo"
    selectBooks.query[Book].to[List]
  }

  def insertBook(id: UUID, title: String, authorId: UUID, year: Year): doobie.ConnectionIO[Int] = {
    val insertBook = fr"INSERT INTO books(id, title, author, year) VALUES ($id, $title, $authorId, $year)"
    insertBook.update.run
  }

  def insertAuthor(id: UUID, name: String, birthday: LocalDate): doobie.ConnectionIO[Int] = {
    val insertAuthor = fr"INSERT INTO authors(id, name, birthday) VALUES ($id, $name, $birthday)"
    insertAuthor.update.run
  }

  private def insertBookWithNewAuthor(bookWithAuthor: BookWithAuthorDTO) = {
    for {
      // It is wrapped here only because for comprehensions can't start with =
      authorId <- UUID.randomUUID().pure[ConnectionIO]
      _ <- insertAuthor(authorId, bookWithAuthor.author.name, bookWithAuthor.author.birthday)
      insert <- insertBookWithExistingAuthor(authorId, bookWithAuthor)
    } yield insert
  }

  def insertBookWithAuthor(publication: BookWithAuthorDTO) = {
    fetchAuthorByName(publication.author.name).flatMap {
      case None => insertBookWithNewAuthor(publication)
      case Some(x) => insertBookWithExistingAuthor(x.id, publication)
    }
  }

  private def insertBookWithExistingAuthor(authorId: UUID, bookWithAuthor: BookWithAuthorDTO) = {
    for {
      bookId <- UUID.randomUUID().pure[ConnectionIO]
      bookInsert <- insertBook(bookId, bookWithAuthor.title, authorId, bookWithAuthor.year)
      selectBookWithAuthor <- fetchBookById(bookId).option
    } yield (bookInsert, selectBookWithAuthor)
  }

  def updateBookWithAuthor(bookId: UUID, bookWithAuthor: BookWithAuthorDTO) = {
    for {
      book <- fetchBookById(bookId).unique
      _ <- for {
        authorOpt <- fetchAuthorByName(bookWithAuthor.title)
        authorId <- authorOpt match {
          case None => for {
            authorId <- UUID.randomUUID().pure[ConnectionIO]
            _ <- insertAuthor(authorId, bookWithAuthor.author.name, bookWithAuthor.author.birthday)
          } yield authorId
          case Some(author) => for {
            _ <- updateAuthor(author)
          } yield author.id
        }
        _ <- updateBook(book.copy(authorId = authorId))
        select <- fetchBookById(book.id).unique
      } yield select
    } yield book
  }


  private def updateAuthor(author: Author): doobie.ConnectionIO[Int] = {
    val updateAuthor = fr"UPDATE authors SET name = ${author.name}, birthday = ${author.birthday} WHERE id = ${author.id}"
    updateAuthor.update.run
  }

  def updateBook(book: Book): doobie.ConnectionIO[Int] = {
    val updateBook = fr"UPDATE books SET year = ${book.year}, title = ${book.title}, author = ${book.authorId} WHERE id = ${book.id}"
    updateBook.update.run
  }

  def updateYearOfBook(id: UUID, year: Year): doobie.ConnectionIO[Int] = {
    val updateBook = fr"UPDATE books SET year = $year WHERE id = $id"
    updateBook.update.run
  }
}
