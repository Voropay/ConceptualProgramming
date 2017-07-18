packSettings

packMain := Map("conpro" -> "org.conceptualprogramming.core.statements.ProgramExecutor")

name := "ConceptualProgramming"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "org.scala-lang" % "scala-parser-combinators" % "2.11.+"
libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % "3.4.0+"
libraryDependencies += "org.seleniumhq.selenium" % "selenium-support" % "3.4.0"