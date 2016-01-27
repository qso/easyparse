import sbt._
import Keys._



lazy val commonSettings = Seq(
  scalaVersion := "2.11.7",
  organization := "com.qso",
  libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
)

lazy val easyparse = (project in file("easyparse")).
  dependsOn(util).
  settings(commonSettings: _*).
  settings(
    name := "easyparse",
    version := "1.0"
)

lazy val util = (project in file("util")).settings(commonSettings: _*).
  settings(
    name := "util"
  )

lazy val scame = (project in file("scame")).
  dependsOn(easyparse).
  settings(commonSettings: _*).
  settings(
    name := "scame"
  )