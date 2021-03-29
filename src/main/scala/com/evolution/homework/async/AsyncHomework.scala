package com.evolution.homework.async

import java.net.URL
import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import cats.implicits._

/**
 * Application:
 * - takes a web-page URL from arguments (args array)
 * - loads the web-page body, extracts HTTP links from it
 * - for all the found links, tries to fetch a server name header if there is one
 * - prints all the encountered unique server name values in alphabetical order
 *
 * Each link processing should be done in parallel.
 * Validation of arguments is not needed.
 *
 * Try to test it on http://google.com!
 */
object AsyncHomework extends App {
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  val site = args(0)

  val pageBodyF: Future[String] = fetchPageBody(site)
  val linkUrlsF: Future[List[String]] = pageBodyF.flatMap(findLinkUrls)
  val serverNamesF: Future[List[Option[String]]] = linkUrlsF.flatMap(x => x.map(fetchServerName).sequence)
  serverNamesF.onComplete(t => t.toEither.map(l => l.distinct.sorted.flatten) match {
    case Left(x) => println(x.getMessage)
    case Right(l) => l.foreach(println)
  })

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    Future {
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }
  }


  private def findLinkUrls(html: String): Future[List[String]] = Future {
    val linkPattern = """href="(http[^"]+)"""".r
    linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
  }
}
