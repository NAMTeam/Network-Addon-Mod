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

console / initialCommands := """
import metarules._, metarules.meta._
import Implicits._, Network._, Flag._, Flags._, RotFlip._, Tile.{CopyTile => %}, Group.SymGroup._
lazy val resolve = module.Main.resolve
implicit lazy val context = RuleTransducer.Context(resolve, module.RegenerateTileOrientationCache.loadCache(), module.MirrorVariants.preprocessor)
def transduce(rule: Rule[SymTile]): Unit = RuleTransducer(rule)(context) foreach println
"""

def wrapWithJLogger(logger: sbt.util.Logger): sbt.util.Logger = {
  // the following is a workaround to highlight the logged warnings with sbt
  val jLogger = java.util.logging.Logger.getLogger("networkaddonmod")
  jLogger.setUseParentHandlers(false)  // avoids printing to console twice
  class MyHandler extends java.util.logging.Handler {
    def close() = {}
    def flush() = {}
    def publish(record: java.util.logging.LogRecord): Unit = record.getLevel match {
      case java.util.logging.Level.SEVERE => logger.error(record.getMessage)
      case java.util.logging.Level.WARNING => logger.warn(record.getMessage)
      case java.util.logging.Level.INFO => logger.info(record.getMessage)
    }
  }
  jLogger.addHandler(new MyHandler())
  logger
}

def runMainWithJLogger(main: String) = Def.inputTask {
  val args: Seq[String] = sbt.complete.Parsers.spaceDelimited("<arg>").parsed
  (Compile / runner).value.run(
    mainClass = if (main == null) args(0) else main,
    classpath = (Compile / fullClasspath).value.files,
    log = wrapWithJLogger(streams.value.log),
    options = Seq.empty[String])
}

// Compile / mainClass := Some("metarules.module.CompileAllMetarules")  // execute with `sbt run`
run := runMainWithJLogger("metarules.module.CompileAllMetarules").evaluated

runMain := runMainWithJLogger(null).evaluated

lazy val generateLocales = inputKey[scala.util.Try[Unit]]("Generates the locale .dat files from .po files")
generateLocales := runMainWithJLogger("networkaddonmod.localization.GenerateLocales").evaluated

lazy val regenerateTileOrientationCache = inputKey[scala.util.Try[Unit]]("Regenerates the cache used for translating metarules to RUL2")
regenerateTileOrientationCache := runMainWithJLogger("metarules.module.RegenerateTileOrientationCache").evaluated


libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test"

libraryDependencies += "tv.cntt" %% "scaposer" % "1.11.1"


// the following are transitive dependencies of metarules

libraryDependencies += "com.github.memo33" %% "scalaenum" % "0.1.4" from "https://github.com/memo33/scalaenum/releases/download/v0.1.4/scalaenum_2.11-0.1.4.jar"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"

libraryDependencies += "org.parboiled" %% "parboiled-scala" % "1.1.6"

libraryDependencies += "com.propensive" %% "rapture-io" % "0.9.1"

libraryDependencies += "com.propensive" %% "rapture-core" % "0.9.0"

libraryDependencies += "com.github.memo33" %% "scala-unsigned" % "0.1.3" from "https://github.com/memo33/scala-unsigned/releases/download/v0.1.3/scala-unsigned_2.11-0.1.3.jar"

libraryDependencies += "com.github.memo33" % "jsquish" % "2.0.1" from "https://github.com/memo33/jsquish/releases/download/v2.0.1/jsquish-2.0.1.jar"

libraryDependencies += "ps.tricerato" %% "pureimage" % "0.1.1" from "https://github.com/memo33/scdbpf/releases/download/v0.1.7/pureimage_2.11-0.1.1.jar"

libraryDependencies += "com.github.memo33" %% "scdbpf" % "0.1.10" from "https://github.com/memo33/scdbpf/releases/download/v0.1.10/scdbpf_2.11.jar"

libraryDependencies += "com.github.memo33" %% "metarules" % "0.3.2" from "https://github.com/memo33/metarules/releases/download/v0.3.2/metarules_2.11.jar"
