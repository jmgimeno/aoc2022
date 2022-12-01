name := "aoc2022"
version := "0.1.0-SNAPSHOT"

scalaVersion := "3.2.1"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % "2.0.4",
  "dev.zio" %% "zio-streams" % "2.0.4",
  "dev.zio" %% "zio-test" % "2.0.4" % Test,
  "dev.zio" %% "zio-test-sbt" % "2.0.4" % Test,
  "dev.zio" %% "zio-test-magnolia" % "2.0.4" % Test
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
