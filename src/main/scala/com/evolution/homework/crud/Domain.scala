package com.evolution.homework.crud

import enumeratum.{CirceEnum, EnumEntry}
import enumeratum.values.{StringCirceEnum, StringDoobieEnum, StringEnum, StringEnumEntry}

import java.time.{LocalDate, Year}
import java.util.UUID

final case class Author(id: UUID, name: String, birthday: LocalDate)
final case class AuthorDTO(name: String, birthday: LocalDate)

final case class Book(id: UUID, authorId: UUID, title: String, year: Year, genre: Genre)

final case class BookWithAuthor(id: UUID, author: Author, title: String, year: Year, genre: Genre) {
  override def toString: String = s"$title ($year) by ${author.name}"
}
final case class BookWithAuthorDTO(author: AuthorDTO, title: String, year: Year, genre: Genre)

sealed abstract class Genre(val value: String) extends StringEnumEntry with EnumEntry

// I didn't get how to make work doobie + circe integration in a more human-readable way
case object Genre extends StringEnum[Genre] with StringDoobieEnum[Genre] with StringCirceEnum[Genre] {
  case object Fiction  extends Genre("fiction")
  case object Tutorial extends Genre("tutorial")

  val values = findValues
}
