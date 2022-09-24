name := "nam-controller"

organization := "com.github.memo33"

version := "45-SNAPSHOT"

scalaVersion := "2.11.12"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  //"-Yinline-warnings",
  //"-optimize",
  "-encoding", "UTF-8",
  "-target:jvm-1.6")

Compile / mainClass := Some("metarules.module.CompileAllMetarules")

console / initialCommands := """
import metarules._, metarules.meta._
import Implicits._, Network._, Flag._, Flags._, RotFlip._, Tile.{CopyTile => %}
implicit val resolve = module.Main.resolve
def transduce(rule: Rule[Tile]): Unit = RuleTransducer(rule) foreach println
"""

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test"


// the following are transitive dependencies of metarules

libraryDependencies += "com.github.memo33" %% "scalaenum" % "0.1.4" from "https://github.com/memo33/scalaenum/releases/download/v0.1.4/scalaenum_2.11-0.1.4.jar"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"

libraryDependencies += "org.parboiled" %% "parboiled-scala" % "1.1.6"

libraryDependencies += "com.propensive" %% "rapture-io" % "0.9.1"

libraryDependencies += "com.propensive" %% "rapture-core" % "0.9.0"

libraryDependencies += "com.github.memo33" %% "scala-unsigned" % "0.1.3" from "https://github.com/memo33/scala-unsigned/releases/download/v0.1.3/scala-unsigned_2.11-0.1.3.jar"

libraryDependencies += "com.github.memo33" % "jsquish" % "2.0.1" from "https://github.com/memo33/jsquish/releases/download/v2.0.1/jsquish-2.0.1.jar"

libraryDependencies += "ps.tricerato" %% "pureimage" % "0.1.1" from "https://github.com/memo33/scdbpf/releases/download/v0.1.7/pureimage_2.11-0.1.1.jar"

libraryDependencies += "com.github.memo33" %% "scdbpf" % "0.1.8" from "https://github.com/memo33/scdbpf/releases/download/v0.1.8/scdbpf_2.11.jar"

libraryDependencies += "com.github.memo33" %% "metarules" % "0.1.4-SNAPSHOT"
