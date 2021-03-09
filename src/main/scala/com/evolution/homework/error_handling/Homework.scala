package com.evolution.homework.error_handling

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxApply, catsSyntaxTuple4Semigroupal, catsSyntaxValidatedIdBinCompat0}

import java.time.Instant
import java.util.{Calendar, Date}
import scala.util.Try

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object Homework {
  case class PaymentCard(cardNumber: String, name: String, securityCode: String, expirationDate: Date)

  sealed trait ValidationError
  object ValidationError {
    final case object CardNumberIsEmpty extends ValidationError {
      override def toString: String = "Card number is empty"
    }
    final case object CardNumberIsLessThan16symbols extends ValidationError {
      override def toString: String = "Card number length is less than 16 symbols"
    }
    final case object CardNumberIsMoreThan16symbols extends ValidationError {
      override def toString: String = "Card number length is more than 16 symbols"
    }
    final case object CardNumberIsNotValid extends ValidationError {
      override def toString: String = "Card number isn't valid"
    }
    final case object OwnerNameIsEmpty extends ValidationError {
      override def toString: String = "Owner name is empty"
    }
    final case object OwnerShouldContainBothNameAndSurname extends ValidationError {
      override def toString: String = "Owner does not have either name or surname"
    }
    final case object OwnerShouldContainOnlyNameAndSurname extends ValidationError {
      override def toString: String = "Owner full name should contain only two values(name and surname)"
    }
    final case object OwnerNameIsNotValid extends ValidationError {
      override def toString: String = "Owner name is not a valid name"
    }
    final case object OwnerSurnameIsNotValid extends ValidationError {
      override def toString: String = "Owner surname is not a valid surname"
    }
    final case object SecurityCodeIsEmpty extends ValidationError {
      override def toString: String = "Security code is empty"
    }
    final case object SecurityCodeIncorrectSize extends ValidationError {
      override def toString: String = "Security code has invalid size"
    }
    final case object SecurityCodeIsNotValid extends ValidationError {
      override def toString: String = "Security code isn't valid"
    }
    final case object ExpirationDateIsEmpty extends ValidationError {
      override def toString: String = "Expiration date is empty"
    }
    final case object ExpirationDateIsNotValid extends ValidationError {
      override def toString: String = "Expiration date isn't valid date"
    }
  }

  object PaymentCardValidator {
    import com.evolution.homework.error_handling.Homework.ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private[error_handling] def validateUsername(username: String): AllErrorsOr[String] = {
      val nameRegex = "^[A-Z]+$"
      val trimmedUsername = username.trim.toUpperCase
      val splittedUsername = trimmedUsername.split("\\s+")
      def validateUsernameHasBothNameAndSurname: AllErrorsOr[String] =
        if (splittedUsername.length < 2) OwnerShouldContainBothNameAndSurname.invalidNec
        else if (splittedUsername.length > 2) OwnerShouldContainOnlyNameAndSurname.invalidNec
        else trimmedUsername.validNec
      def validateNameRegex: AllErrorsOr[String] =
        if (!splittedUsername(0).matches(nameRegex)) OwnerNameIsNotValid.invalidNec
        else trimmedUsername.validNec
      def validateSurnameRegex: AllErrorsOr[String] =
        if (splittedUsername(1).matches(nameRegex)) trimmedUsername.validNec
        else OwnerSurnameIsNotValid.invalidNec

      if (trimmedUsername.isEmpty) OwnerNameIsEmpty.invalidNec
      else {
        if (splittedUsername.length == 2) validateNameRegex.productR(validateSurnameRegex)
        else validateUsernameHasBothNameAndSurname
      }
    }

    private[error_handling] def validateCardNumber(cardNumber: String): AllErrorsOr[String] = {
      val cardLength = 16
      val cardRegex = "^[A-Z0-9]+$"
      val trimmedNumber = cardNumber.trim.toUpperCase
      def validateNumberLessThan16: AllErrorsOr[String] =
        if (trimmedNumber.length < cardLength) CardNumberIsLessThan16symbols.invalidNec
        else trimmedNumber.validNec
      def validateNumberMoreThan16: AllErrorsOr[String] =
        if (trimmedNumber.length > cardLength) CardNumberIsMoreThan16symbols.invalidNec
        else trimmedNumber.validNec
      def validateNumberRegex: AllErrorsOr[String] =
        if (!trimmedNumber.matches(cardRegex)) CardNumberIsNotValid.invalidNec
        else trimmedNumber.validNec

      if (trimmedNumber.isEmpty) CardNumberIsEmpty.invalidNec
      else {
        validateNumberRegex.productR(validateNumberMoreThan16.productR(validateNumberLessThan16))
      }
    }

    private lazy val calendar: Calendar = Calendar.getInstance()
    calendar.set(1970, 1, 1)
    private lazy val DateBottomLine = calendar.getTime

    private[error_handling] def validateExpirationDate(date: String): AllErrorsOr[Date] = {
      val trimmedDate = date.trim
      val parsedDateOpt = Try(Date.from(Instant.parse(date))).toOption
      if (trimmedDate.isEmpty) ExpirationDateIsEmpty.invalidNec
      else {
        if (parsedDateOpt.isDefined && parsedDateOpt.get.before(DateBottomLine)) parsedDateOpt.get.validNec
        else ExpirationDateIsNotValid.invalidNec
      }
    }

    private[error_handling] def validateSecurityCode(securityCode: String): AllErrorsOr[String] = {
      val cardRegex = "^[0-9]+$"
      val trimmedCode = securityCode.trim
      def validateCodeSize: AllErrorsOr[String] =
        if (trimmedCode.length != 3) SecurityCodeIncorrectSize.invalidNec
        else trimmedCode.validNec
      def validateCodeRegex: AllErrorsOr[String] =
        if (!trimmedCode.matches(cardRegex)) SecurityCodeIsNotValid.invalidNec
        else trimmedCode.validNec

      if (trimmedCode.isEmpty) SecurityCodeIsEmpty.invalidNec
      else {
        validateCodeRegex.productR(validateCodeSize)
      }
    }

    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] = {
      (
        validateCardNumber(number),
        validateUsername(name),
        validateSecurityCode(securityCode),
        validateExpirationDate(expirationDate)
      ).mapN(PaymentCard)
    }
  }
}
