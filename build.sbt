ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

name := "advent-of-code-2021"

libraryDependencies ++= List(
  "dev.zio"       %% "zio-prelude"            % "1.0.0-RC8",
  "org.scalatest" %% "scalatest-freespec"     % "3.2.10" % Test,
  "org.scalatest" %% "scalatest-mustmatchers" % "3.2.10" % Test
)
