name := "nam-controller"

organization := "com.github.memo33"

version := "32.2.0-SNAPSHOT"

scalaVersion := "2.11.0"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  //"-Yinline-warnings",
  //"-optimize",
  "-encoding", "UTF-8",
  "-target:jvm-1.6")

mainClass in Compile := Some("metarules.module.CompileAllMetarules")

initialCommands in console := """
import metarules._, meta._
import Implicits._, Network._, Flag._, Flags._, RotFlip._, Tile.{CopyTile => %}
implicit val resolve = module.Main.resolve
def transduce(rule: Rule[Tile]): Unit = RuleTransducer(rule) foreach println
"""

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test"


resolvers += "memo33-bintray" at "https://dl.bintray.com/memo33/maven"

// for some reason, sbt cannot seem to handle repository locations of transitive dependencies
resolvers += "stephenjudkins-bintray" at "https://dl.bintray.com/stephenjudkins/maven"

libraryDependencies += "com.github.memo33" %% "metarules" % "0.1.2"
