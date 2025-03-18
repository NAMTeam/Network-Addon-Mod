package com.sc4nam.module

import java.nio.file.{Files, Paths}
import io.github.memo33.metarules.meta.{RotFlip, EquivRule, Rule, IdTile}
import RotFlip._
import com.sc4nam.module._
import Rul2Model.{iterateRulFiles, parseRuleWithRestrictedDriveside, Driveside, Rhd, Lhd, RhdAndLhd, drivesideOfFile, rulesHaveSameOutput}
import SanityChecker.{fileEndsWithNewline, linePatternIncludingNewlines}

/** A generic interface for customizable checking of all RUL2 overrides in the
  * Controller.
  * The Controller files are either updated in place by adding or removing tags
  * as inline comments, or the failed checks are printed to the console.
  */
abstract class Rul2Checker {

  type Failure

  val allTags: Seq[String]

  val tagOf: Driveside => String

  def failureToString(rule: Rule[IdTile], line: String, failure: Failure): String

  def processRule(rule: Rule[IdTile], driveside: Driveside, line: String): Option[(Failure, Driveside)]

  def runChecks(updateMode: Boolean): Rul2Checker.Result = {
    var numFailures = 0
    var removed = 0
    var added = 0

    val directory = Paths.get("Controller/RUL2")
    LOGGER.info(s"""Loading all RUL2 code for RHD and LHD from "$directory" and running checks""")
    iterateRulFiles(directory).foreach { path =>
      val drivesideFile = drivesideOfFile(path)

      def findFailure(line: String): Option[(Rule[IdTile], Driveside, Failure)] = {
        for {
          (rule, driveside) <- parseRuleWithRestrictedDriveside(line, drivesideFile)
          (failure, driveside2) <- processRule(rule, driveside, line)
        } yield (rule, driveside2, failure)
      }

      var lineNumber = 0
      var badFile = false
      def logFailure(rule: Rule[IdTile], line: String, failure: Failure): Unit = {
        if (!badFile) {
          LOGGER.info(s"==> $path")
          badFile = true
        }
        LOGGER.warning(s"$lineNumber: ${failureToString(rule, line, failure)}")
      }

      if (!updateMode) {
        scala.util.Using.resource(new java.util.Scanner(path.toFile(), "UTF-8")) { scanner =>
          while(scanner.hasNextLine()) {
            val line = scanner.nextLine()
            lineNumber += 1
            findFailure(line) match {
              case None =>  // ignore
              case Some((rule, _, failure)) =>
                if (!allTags.exists(line.contains(_))) {
                  numFailures += 1
                  logFailure(rule, line, failure)
                }
            }
          }
        }
      } else {  // updateMode
        val tmpPath = path.resolveSibling(path.getFileName().toString() + ".tmp")
        val endsWithNewline = fileEndsWithNewline(path)  // attempt to preserve missing newlines at end of files to avoid noise
        scala.util.Using.resources(
          new java.util.Scanner(path.toFile(), "UTF-8").useDelimiter(linePatternIncludingNewlines),
          new java.io.PrintWriter(tmpPath.toFile(), "UTF-8")
        ) { (lineScanner, printer) =>
          while (lineScanner.hasNext()) {
            val line = lineScanner.next()
            findFailure(line) match {
              case Some((_, driveside, _)) =>
                numFailures += 1
                if (!allTags.exists(line.contains(_))) {
                  added += 1
                }
                printer.println(Rul2Checker.replaceTags(line, add = Some(tagOf(driveside)), remove = allTags))
              case None =>
                if (allTags.exists(line.contains(_))) {  // no conflict, so remove tag
                  removed += 1
                  printer.println(Rul2Checker.replaceTags(line, add = None, remove = allTags))
                } else {
                  printer.print(line)  // preserving original linebreaks
                }
            }
          }
        }
        Files.move(tmpPath, path, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      }
    }
    new Rul2Checker.Result(numFailures = numFailures, removed = removed, added = added)
  }
}

object Rul2Checker {
  class Result(val numFailures: Int, val removed: Int, val added: Int)

  def replaceTags(line: String, add: Option[String], remove: Seq[String]): String = {
    val line0 =
      remove.foldLeft(line.stripLineEnd) { (line, rem) =>
        line.replaceFirst(s" ?\\b$rem\\b", "").replaceFirst(";\\s*$", "")
      }
    if (add.isDefined) s"$line0; ${add.get}" else line0
  }
}
