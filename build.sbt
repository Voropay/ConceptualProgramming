ThisBuild / version := "1.0"

ThisBuild / scalaVersion := "2.11.8"

lazy val ConceptualProgramming = (project in file("."))
  .settings(
    name := "ConceptualProgramming",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    libraryDependencies += "org.scala-lang" % "scala-parser-combinators" % "2.11.+",
    libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % "3.4.0+",
    libraryDependencies += "org.seleniumhq.selenium" % "selenium-support" % "3.4.0",
  )

