package com.evolution.homework.json

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}
import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import io.circe
import io.circe.Decoder
import io.circe.parser._
import io.circe.generic.JsonCodec
import io.circe.generic.extras.{Configuration, ConfiguredJsonCodec, JsonKey}
import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scalaj.http.Http

import scala.io.Source
import scala.util.Try

class HomeworkSpec extends AnyWordSpec with Matchers with EitherValues {
  import HomeworkSpec._

  "NBA JSON API client" should {
    "get info about today games" in {
      val date = LocalDate.now()
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      succeed
    }

    "fetch games for 14 Feb 2020" in {
      val date = LocalDate.of(2020, 2, 14)
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      val gameInfos = gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      gameInfos.size must be(1)
    }
  }

}

object HomeworkSpec {
  implicit val config: Configuration = Configuration.default
  implicit val dateDecoder: Decoder[LocalDate] = Decoder.decodeLocalDateWithFormatter(DateTimeFormatter.BASIC_ISO_DATE)

  @ConfiguredJsonCodec final case class TeamTotals(assists: String, @JsonKey("full_timeout_remaining") fullTimeoutRemaining: String, plusMinus: String)
  @JsonCodec final case class TeamBoxScore(totals: TeamTotals)
  @JsonCodec final case class GameStats(hTeam: TeamBoxScore, vTeam: TeamBoxScore)
  @JsonCodec final case class PrevMatchup(gameDate: LocalDate, gameId: String)
  @JsonCodec final case class BoxScore(
                                        basicGameData: Game,
                                        previousMatchup: PrevMatchup,
                                        stats: Option[GameStats],
                                      )
  @JsonCodec final case class JustScore(score: String)
  @JsonCodec final case class TeamStats(
                                         linescore: List[JustScore],
                                         loss: String,
                                         score: String,
                                         teamId: String,
                                         triCode: String
                                       )
  @JsonCodec final case class GameDuration(hours: String, minutes: String)
  @JsonCodec final case class Arena(
                                     city: String,
                                     country: String,
                                     isDomestic: Boolean,
                                     name: String,
                                     stateAbbr: String
                                   )
  @JsonCodec final case class Game(
                                    arena: Arena,
                                    attendance: String,
                                    endTimeUTC: Option[ZonedDateTime],
                                    gameDuration: GameDuration,
                                    gameId: String,
                                    gameUrlCode: String,
                                    hTeam: TeamStats,
                                    isBuzzerBeater: Boolean,
                                    startTimeUTC: ZonedDateTime,
                                    vTeam: TeamStats,
                                  )
  @JsonCodec final case class Scoreboard(games: List[Game], numGames: Int)

  private def fetchScoreboard(date: LocalDate): Either[circe.Error, Scoreboard] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val jsonString = Try {
      val src = Source.fromResource(s"scoreboard_$dateString.json")
      val s = src.mkString
      src.close()
      s
    }.getOrElse {
      val url = s"https://data.nba.net/10s/prod/v1/$dateString/scoreboard.json"
      Http(url).asString.body
    }
    decode[Scoreboard](jsonString)
  }

  private def fetchGameInfo(date: LocalDate, gameId: String): Either[circe.Error, BoxScore] = {
    val jsonString = Try {
      val src = Source.fromResource(s"${gameId}_boxscore.json")
      val s = src.mkString
      src.close()
      s
    }.getOrElse {
      val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
      val url = s"https://data.nba.net/10s/prod/v1/$dateString/${gameId}_boxscore.json"
      Http(url).asString.body
    }
    decode[BoxScore](jsonString)
  }
}

