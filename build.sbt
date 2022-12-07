name := "aoc2022"
version := "0.1.0-SNAPSHOT"

scalaVersion := "3.2.1"

val zioVersion = "2.0.5"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-streams" % zioVersion,
  "dev.zio" %% "zio-test" % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
  "dev.zio" %% "zio-test-magnolia" % zioVersion % Test
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
