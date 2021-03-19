package com.evolution.homework.testing

import com.evolution.homework.testing.Calculator.Operation

case class Calculator(memory: Int = 0, screen: Int = 0, operation: Option[Operation] = None, evaluated: Boolean = false) {

  def enter(digit: Int): Either[String, Calculator] = {
    if (digit >= 0 && digit <= 9) {
      if (evaluated) Right(this.copy(screen = digit, evaluated = false))
      else Right(this.copy(screen = screen * 10 + digit, evaluated = false))
    } else {
      Left("digit out of range")
    }
  }

  private def chain(op: Operation): Either[String, Calculator] = operation match {
    case None => Right(Calculator(memory = screen, operation = Some(op)))
    case _ => this.calculate.map(_.copy(operation = Some(op)))
  }

  def plus: Either[String, Calculator] = chain(Operation.Plus)
  def minus: Either[String, Calculator] = chain(Operation.Minus)
  def mul: Either[String, Calculator] = chain(Operation.Multiply)
  def div: Either[String, Calculator] = chain(Operation.Div)
  def mod: Either[String, Calculator] = chain(Operation.Mod)

  def calculate: Either[String, Calculator] = operation match {
    case Some(Operation.Plus) =>
      val eval = memory + screen
      Right(this.copy(memory = eval, screen = eval, evaluated = true))
    case Some(Operation.Minus) =>
      val eval = memory - screen
      Right(this.copy(memory = eval, screen = eval, evaluated = true))
    case Some(Operation.Multiply) =>
      val eval = memory * screen
      Right(this.copy(memory = eval, screen = eval, evaluated = true))
    case Some(Operation.Div) =>
      if (screen == 0) Left("Zero Division")
      else {
        val eval = memory / screen
        Right(this.copy(memory = eval, screen = eval, evaluated = true))
      }
    case Some(Operation.Mod) =>
      val eval = memory % screen
      Right(this.copy(memory = eval, screen = eval, evaluated = true))
    case None => Right(this)
  }

}
object Calculator {
  sealed trait Operation
  object Operation {
    object Plus extends Operation
    object Minus extends Operation
    object Multiply extends Operation
    object Div extends Operation
    object Mod extends Operation
  }
}