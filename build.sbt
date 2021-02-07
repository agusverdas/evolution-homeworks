name := "evolution-bootcamp-homework"

version := "0.1"

scalaVersion := "2.13.4"
val scalaTestVersion = "3.1.0.0-RC2"
val catsScalacheckVersion = "0.2.0"
val catsVersion = "2.2.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test,
  "org.scalatestplus" %% "selenium-2-45" % scalaTestVersion % Test,
  "io.chrisdavenport" %% "cats-scalacheck" % catsScalacheckVersion % Test,
)
