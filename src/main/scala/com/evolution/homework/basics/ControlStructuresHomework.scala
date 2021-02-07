package com.evolution.homework.basics

import com.evolution.homework.basics.ControlStructuresHomework.Command._

import scala.io.Source
import scala.util.Try

object ControlStructuresHomework {
  val Epsilon = 0.00001

  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command

  object Command {

    final case class Divide(dividend: Double, divisor: Double) extends Command

    final case class Sum(numbers: List[Double]) extends Command

    final case class Average(numbers: List[Double]) extends Command

    final case class Min(numbers: List[Double]) extends Command

    final case class Max(numbers: List[Double]) extends Command

  }

  final case class ErrorMessage(value: String)

  sealed trait Result

  final case class DivisionResult(dividend: Double, divisor: Double, value: Double) extends Result

  final case class SumResult(numbers: List[Double], value: Double) extends Result

  final case class AverageResult(numbers: List[Double], value: Double) extends Result

  final case class MinResult(numbers: List[Double], value: Double) extends Result

  final case class MaxResult(numbers: List[Double], value: Double) extends Result

  private[basics] val parseList: List[String] => Either[ErrorMessage, List[Double]] = { list: List[String] =>
    list.map(x => Try(x.toDouble)).partitionMap(_.toEither) match {
      case (Nil, right) => Right(right)
      case (left, _) => Left(ErrorMessage(left.map(_.getMessage).mkString("; ")))
    }
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    x.trim.toLowerCase.replaceAll("\\s+", " ").split(" ").toList match {
      case l@"divide" :: _ :: _ :: Nil => parseList(l.tail) match {
        case Right(x :: y :: Nil) => Right(Divide(x, y))
        case Left(m) => Left(m)
      }
      case "divide" :: _ => Left(ErrorMessage(s"Incorrect number of arguments for divide"))
      case l@"sum" :: _ :: _ => parseList(l.tail).map(Sum)
      case "sum" :: Nil => Left(ErrorMessage("Incorrect number of arguments for sum"))
      case l@"average" :: _ :: _ => parseList(l.tail).map(Average)
      case "average" :: Nil => Left(ErrorMessage("Incorrect number of arguments for average"))
      case l@"min" :: _ :: _ => parseList(l.tail).map(Min)
      case "min" :: Nil => Left(ErrorMessage("Incorrect number of arguments for min"))
      case l@"max" :: _ :: _ => parseList(l.tail).map(Max)
      case "min" :: Nil => Left(ErrorMessage("Incorrect number of arguments for max"))
      case "" :: Nil => Left(ErrorMessage(s"Command string is empty"))
      case in => Left(ErrorMessage(s"Unknown command : ${in.head}"))
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Divide(_, 0) => Left(ErrorMessage("Division by 0"))
      case Divide(x, y) => Right(DivisionResult(x, y, x / y))
      case Sum(l) => Right(SumResult(l, l.sum))
      case Average(l) => Right(AverageResult(l, l.sum / l.length))
      case Min(l) => Right(MinResult(l, l.min))
      case Max(l) => Right(MaxResult(l, l.max))
    }
  }

  def renderResult(x: Result): String = {
    def renderDoubleAsInt(x: Double): String = {
      val round = x.round
      if ((x - round).abs < Epsilon) round.toString
      else x.toString
    }

    x match {
      case DivisionResult(x, y, z) => s"${renderDoubleAsInt(x)} divided by ${renderDoubleAsInt(y)} is ${renderDoubleAsInt(z)}"
      case SumResult(args, v) => s"the sum of ${args.map(renderDoubleAsInt).mkString(" ")} is ${renderDoubleAsInt(v)}"
      case AverageResult(args, v) => s"the average of ${args.map(renderDoubleAsInt).mkString(" ")} is ${renderDoubleAsInt(v)}"
      case MinResult(args, v) => s"the minimum of ${args.map(renderDoubleAsInt).mkString(" ")} is ${renderDoubleAsInt(v)}"
      case MaxResult(args, v) => s"the maximum of ${args.map(renderDoubleAsInt).mkString(" ")} is ${renderDoubleAsInt(v)}"
    }
  }

  def process(x: String): String = {
    import cats.implicits._
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.
    val render: Either[ErrorMessage, String] = for {
      command <- parseCommand(x)
      result <- calculate(command)
      render = renderResult(result)
    } yield render
    render.leftMap("Error : " + _.value).merge
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}

