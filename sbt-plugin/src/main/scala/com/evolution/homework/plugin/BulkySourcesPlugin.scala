package com.evolution.homework.plugin

import sbt.Keys._
import sbt.{AutoPlugin, Compile, Def, File, Setting, Test, settingKey, taskKey}

object BulkySourcesPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    lazy val bulkyThresholdInLines = settingKey[Int]("Threshold in lines")
    lazy val bulkySources = taskKey[Seq[(Int, File)]]("Task gets sorted collection of files with number of lines more than threshold")
  }

  import autoImport._

  override lazy val globalSettings: Seq[Def.Setting[_]] = Seq(
    bulkyThresholdInLines := 100
  )

  override val projectSettings: Seq[Setting[_]] = Seq(
    bulkySources := lines((Compile / sources).value, bulkyThresholdInLines.value),
    (Test / bulkySources) := lines((Test / sources).value, bulkyThresholdInLines.value)
  )

  def lines(files: Seq[File], threshold: Int): Seq[(Int, File)] = {
    files.map { file =>
      val lines = sbt.IO.readLines(file).size
      (lines, file)
    }.filter { case (lines, _) => lines >=  threshold}.sortBy {
      case (l, _) => l
    }(Ordering[Int].reverse)
  }
}
