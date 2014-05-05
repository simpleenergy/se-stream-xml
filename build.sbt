organization := "com.simpleenergy"

name := "se-stream-xml"

version := "0.2"

scalaVersion := "2.10.4"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.0-M6",
  "org.scalaz.stream" %% "scalaz-stream" % "0.4.1a",
  "org.specs2" %% "specs2" % "2.3.10-scalaz-7.1.0-SNAPSHOT" % "test" exclude("org.scalaz", "scalaz-core_2.10") exclude("org.scalaz", "scalaz-concurrent_2.10")
)

scalacOptions ++= Seq(
  "-Xexperimental",
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint",
  "-Yno-adapted-args",
  "-Yno-imports",
  "-Ywarn-all",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:higherKinds"
)

homepage := Some(url("https://github.com/simpleenergy/se-stream-xml"))

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

bintrayPublishSettings
