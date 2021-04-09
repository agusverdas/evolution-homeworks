package com.evolution.homework.effects_concurent

import cats.data.Validated
import cats.effect.{Blocker, ContextShift, ExitCode, IO, IOApp, Sync}
import cats.implicits._

import java.nio.file.{Files, Path}
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.io.StdIn


/*
  Additional assignment:
  1. Read from the console the file path.
    1.1 Use Blocking Thread Pool
    1.2 Check the transmitted data(Error Handling + Validation).
  2. Read from the console the seed.
    2.1 Use Blocking Thread Pool
    2.2 Check the transmitted data(Error Handling + Validation).
  3. Read the data from the file.
  4. Calculate the signature (in parallel if possible).
    4.1 Use Separate Thread Pool(ContextShift)
    4.2 Split text into words
    4.3 Calculate hash for each word
    4.4 Take the minimal hash
    4.5* Repeat the process for n different hash functions.
  5. Save the signature in memory(think about storage).
  6. Terminate the application.

  def javaHash(word: String, seed: Int = 0): Int = {
    var hash = 0

    for (ch <- word.toCharArray)
      hash = 31 * hash + ch.toInt

    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }

  def knuthHash(word: String, constant: Int): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % constant
  }
 */
object EffectsContinued extends IOApp{

  sealed trait ValidationError
  final case class EmptyFile(filename: String) extends ValidationError
  final case class NotPositiveInt(number: Int) extends ValidationError
  val defaultBlocker: Blocker = Blocker.liftExecutionContext(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))

  def readFilePath(blocker: Blocker)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[String] = blocker.delay{
    StdIn.readLine()
  }

  def readSeed(blocker: Blocker)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[Int] = blocker.delay{
    val stringNumber = StdIn.readLine()
    stringNumber.toInt
  }.handleErrorWith {
    e => IO.raiseError(new RuntimeException("Seed can't be parsed to number", e))
  }

  def javaHash(word: String): Int = {
    var hash = 0

    for (ch <- word.toCharArray)
      hash = 31 * hash + ch.toInt

    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }

  def knuthHash(constant: Int)(word: String): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % constant
  }

  def functions(n: Int): IO[List[String => Int]] = {
    val listF = 1.to(n).map(x => knuthHash(x) _).toList
    IO.pure(javaHash _ :: listF)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      // blocker since IO operations
      _ <- defaultBlocker.blockOn(IO(println("What is a file path?")))
      path <- readFilePath(defaultBlocker).map[Path](Path.of(_))
      _ <- IO.raiseWhen(!Files.exists(path))(new RuntimeException("File path doesn't exist"))
      _ <- defaultBlocker.blockOn(IO(println("Enter seed : ")))
      seed <- readSeed(defaultBlocker)
      fileContent <- defaultBlocker.blockOn(
        IO{
          Files.readString(path)
        }.handleErrorWith(err => IO.raiseError(new RuntimeException("Unable to read file as String", err)))
      )
      // I tried hard to combine Validated and IO somehow not in a painful way and didn't get how to do it.
      // Would appreciate if you provide an example
      _ <- IO.raiseWhen(fileContent.trim.isEmpty)(new RuntimeException("File content is empty"))
      _ <- IO.raiseWhen(seed <= 0)(new RuntimeException(s"Seed is less or equal to 0: $seed"))
      // processing, so no blocker
      words <- IO {
        fileContent.split("\\s+").toList
      }
      hashFunctions <- functions(seed)
      fibers <- IO {
        // Each hash function runs in parallel on all the words and words are paralleled too
        hashFunctions.map(f => words.map(x => IO(f(x)).start))
        // result is List[ - for hash function
        //            List[ - hashes
        //              IO[Fiber, A]]]
      }
      results <- IO {
        fibers.map(x => x.map(iofiber => iofiber.flatMap(fiber => fiber.join)))
      }
      minHashes <- IO {
        results.map(x => x.sequence.flatMap(x => IO.pure(x.min)))
      }
      dict <- IO {
        // I didn't invent a formula for n hash functions
        val key = minHashes.take(1)
        Map(key -> (minHashes, path))
      }
      _ <- IO {
        println(dict)
      }
    } yield ExitCode.Success
  }
}
