name := "nam-controller"

organization := "com.sc4nam"

version := "49-SNAPSHOT"

scalaVersion := "2.13.12"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  // "-opt-warnings:at-inline-failed-summary",
  // "-opt:l:inline", "-opt-inline-from:<sources>",
  "-encoding", "UTF-8",
  "-release:8")

console / initialCommands := """
import io.github.memo33.metarules.meta._, com.sc4nam.module, module.syntax._
import Implicits._, Network._, Flags._, RotFlip._, Rule.{CopyTile => %}, group.SymGroup._
lazy val resolve = module.Main.resolveSafely
implicit lazy val context: RuleTransducer.Context = RuleTransducer.Context(resolve, module.RegenerateTileOrientationCache.loadCache(), module.MirrorVariants.preprocessor)
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
    options = args)
}

// Compile / mainClass := Some("metarules.module.CompileAllMetarules")  // execute with `sbt run`
run := runMainWithJLogger("com.sc4nam.module.CompileAllMetarules").evaluated

runMain := runMainWithJLogger(null).evaluated

lazy val generateLocales = inputKey[scala.util.Try[Unit]]("Generates the locale .dat files from .po files")
generateLocales := runMainWithJLogger("com.sc4nam.localization.GenerateLocales").evaluated

lazy val regenerateTileOrientationCache = inputKey[scala.util.Try[Unit]]("Regenerates the cache used for translating metarules to RUL2")
regenerateTileOrientationCache := runMainWithJLogger("com.sc4nam.module.RegenerateTileOrientationCache").evaluated

lazy val conflictingOverridesCheck = inputKey[scala.util.Try[Unit]]("Checks all RUL2 code for conflicting overrides, optionally updates the inline `conflicting-override` tags")
conflictingOverridesCheck := runMainWithJLogger("com.sc4nam.scripts.ConflictingOverridesChecker").evaluated


libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

libraryDependencies += "tv.cntt" %% "scaposer" % "1.11.1"

libraryDependencies += "io.github.memo33" %% "scdbpf" % "0.2.0"

libraryDependencies += "io.github.memo33" %% "metarules" % "0.6.0"
