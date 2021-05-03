package com.evolution.homework.crud

import java.time.{LocalDate, Year}
import java.util.UUID

final case class Author(id: UUID, name: String, birthday: LocalDate)
final case class AuthorDTO(name: String, birthday: LocalDate)

final case class Book(id: UUID, authorId: UUID, title: String, year: Year)

final case class BookWithAuthor(id: UUID, author: Author, title: String, year: Year) {
  override def toString: String = s"$title ($year) by ${author.name}"
}
final case class BookWithAuthorDTO(author: AuthorDTO, title: String, year: Year)