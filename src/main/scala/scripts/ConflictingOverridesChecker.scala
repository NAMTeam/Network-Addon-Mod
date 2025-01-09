package com.sc4nam.scripts

import java.nio.file.{Files, Paths}
import io.github.memo33.metarules.meta.{RotFlip, EquivRule, Rule, IdTile}
import RotFlip._
import com.sc4nam.module._
import Rul2Model.{iterateRulFiles, parseRuleWithRestrictedDriveside, Driveside, Rhd, Lhd, RhdAndLhd, drivesideOfFile}
import SanityChecker.{fileEndsWithNewline, linePatternIncludingNewlines}

/** Checks for conflicting/duplicate RUL2 code. There are two modes of operation:
  *
  * {{{
  * SBT_OPTS="-Xmx2G" sbt -no-color conflictingOverridesCheck
  * }}}
  * to check all RUL2 code for new conflicting overrides (that are not tagged yet).
  * The new conflicts are printed to stdout. Exit code will be non-zero if new
  * conflicts are found.
  *
  * {{{
  * SBT_OPTS="-Xmx2G" sbt "conflictingOverridesCheck --update"
  * }}}
  * to update the files in "Controller/RUL2" in place by tagging the lines with
  * "conflicting-override" where conflicts are found.
  * The tag is removed from lines that are not conflicting anymore.
  * The first rule in a pair of conflicts is never tagged, as it is the one that
  * will have an effect in the game.
  * Make sure to commit your changes before running this command.
  */
object ConflictingOverridesChecker {

  val tagBoth = "conflicting-override"
  val tagRhd = "conflicting-override_rhd"  // underscore (instead of hyphen) for use with regex word boundaries
  val tagLhd = "conflicting-override_lhd"
  val tagOf = Map[Driveside, String](RhdAndLhd -> tagBoth, Rhd -> tagRhd, Lhd -> tagLhd)
  val allTags = Seq(tagBoth, tagRhd, tagLhd)

  /** Scans the Controller/RUL2 folder for conflicting duplicate RUL2 code.
    *
    * Example:
    *
    * A,B=C,D
    * A,B=E,F
    *
    * In this case, the second line conflicts with the first one.
    */
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      val (numConflicts, _, _) = checkConflictingRul2(updateMode = false)
      val msg = s"Found $numConflicts new conflicting RUL2 overrides."
      if (numConflicts > 0) {
        LOGGER.severe(s"$msg Please avoid introducing new conflicts. Instead verify whether these overrides really have the intended effect and consider rewriting or removing them.")
        System.exit(1)
      } else {
        LOGGER.info(msg)
      }
    } else if (args.sameElements(Seq("--update"))) {
      val (numConflicts, removed, added) = checkConflictingRul2(updateMode = true)
      val msg = s"Found $numConflicts conflicting RUL2 overrides (removed $removed, added $added)."
      if (numConflicts > 0) {
        LOGGER.warning(msg)
      } else {
        LOGGER.info(msg)
      }
    } else {
      LOGGER.severe("wrong arguments")
      System.exit(2)
    }
  }

  def checkConflictingRul2(updateMode: Boolean): (Int, Int, Int) = {
    var numConflicts = 0
    var removed = 0
    var added = 0
    val rulesRhd = collection.mutable.Map.empty[EquivRule, Rule[IdTile]]
    val rulesLhd = collection.mutable.Map.empty[EquivRule, Rule[IdTile]]
    val rulesShared = collection.mutable.Map.empty[EquivRule, Rule[IdTile]]
    val lookupRuleRhd: PartialFunction[EquivRule, Rule[IdTile]] = rulesShared.orElse(rulesRhd)  // the two maps should be disjoint
    val lookupRuleLhd: PartialFunction[EquivRule, Rule[IdTile]] = rulesShared.orElse(rulesLhd)  // the two maps should be disjoint

    val directory = Paths.get("Controller/RUL2")
    LOGGER.info(s"""Loading all RUL2 code for RHD and LHD from "$directory" and checking for conflicts""")
    iterateRulFiles(directory).foreach { path =>
      val drivesideFile = drivesideOfFile(path)

      // simultaneously builds the RUL2 cache and looks for conflicts
      def findConflict(line: String): Option[(Rule[IdTile], Rule[IdTile], Driveside)] = {
        parseRuleWithRestrictedDriveside(line, drivesideFile) match {
          // only the first matching rule is loaded by the game, so we store only the first one read
          case Some((rule, Rhd)) =>
            val key = new EquivRule(rule)
            lookupRuleRhd.unapply(key) match {
              case Some(rule2) => if (rulesHaveSameOutput(rule, rule2)) None else Some((rule, rule2, RhdAndLhd))
              case None => rulesRhd.addOne(key, rule); None
            }
          case Some((rule, Lhd)) =>
            val key = new EquivRule(rule)
            lookupRuleLhd.unapply(key) match {
              case Some(rule2) => if (rulesHaveSameOutput(rule, rule2)) None else Some((rule, rule2, RhdAndLhd))
              case None => rulesLhd.addOne(key, rule); None
            }
          case Some((rule, RhdAndLhd)) =>
            val key = new EquivRule(rule)
            rulesShared.get(key) match {
              case Some(rule2) => if (rulesHaveSameOutput(rule, rule2)) None else Some((rule, rule2, RhdAndLhd))
              case None =>
                if (!rulesRhd.contains(key) && !rulesLhd.contains(key)) {
                  rulesShared.addOne(key, rule); None
                } else {
                  (rulesRhd.get(key).filterNot(rulesHaveSameOutput(rule, _)), rulesLhd.get(key).filterNot(rulesHaveSameOutput(rule, _))) match {
                    case (None, None) => rulesShared.addOne(key, rule); None  // no conflict
                    case (Some(rule2), None) => Some((rule, rule2, Rhd))  // conflict only with RHD rules
                    case (None, Some(rule3)) => Some((rule, rule3, Lhd))  // conflict only with LHD rules
                    case (Some(rule2), Some(rule3)) => Some((rule, rule2, RhdAndLhd)) // conflict with both
                  }
                }
            }
          case None => None
        }
      }

      var lineNumber = 0
      var badFile = false
      def logConflict(x: Rule[IdTile], y: Rule[IdTile]): Unit = {
        if (!badFile) {
          LOGGER.info(s"==> $path")
          badFile = true
        }
        LOGGER.warning(s"$lineNumber: ${x(0)},${x(1)}=${x(2)},${x(3)} conflicts with ${y(0)},${y(1)}=${y(2)},${y(3)}")
      }

      if (!updateMode) {
        scala.util.Using.resource(new java.util.Scanner(path.toFile(), "UTF-8")) { scanner =>
          while(scanner.hasNextLine()) {
            val line = scanner.nextLine()
            lineNumber += 1
            findConflict(line) match {
              case Some((rule, rule2, _)) =>
                if (!line.contains(tagBoth)) {
                  numConflicts += 1
                  logConflict(rule, rule2)
                }
              case None =>  // ignore
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
            findConflict(line) match {
              case Some((_, _, driveside)) =>
                numConflicts += 1
                if (!line.contains(tagBoth)) {
                  added += 1
                }
                printer.println(replaceTags(line, add = Some(tagOf(driveside)), remove = allTags))
              case None =>
                if (line.contains(tagBoth)) {  // no conflict, so remove tag
                  removed += 1
                  printer.println(replaceTags(line, add = None, remove = allTags))
                } else {
                  printer.print(line)  // preserving original linebreaks
                }
            }
          }
        }
        Files.move(tmpPath, path, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      }
    }
    (numConflicts, removed, added)
  }

  def replaceTags(line: String, add: Option[String], remove: Seq[String]): String = {
    val line0 =
      remove.foldLeft(line.stripLineEnd) { (line, rem) =>
        line.replaceFirst(s" ?\\b$rem\\b", "").replaceFirst(";\\s*$", "")
      }
    if (add.isDefined) s"$line0; ${add.get}" else line0
  }

  /** Check if two rules with equivalent LHS actually lead to the same output on
    * the RHS (and thus are not at conflict with each other).
    */
  def rulesHaveSameOutput(x: Rule[IdTile], y: Rule[IdTile]): Boolean = (
    x(0) == y(0)        && x(1) == y(1)        && x(2) == y(2)        && x(3) == y(3)        ||
    x(0) == y(0) * R2F1 && x(1) == y(1) * R2F1 && x(2) == y(2) * R2F1 && x(3) == y(3) * R2F1 ||
    x(0) == y(1) * R2F0 && x(1) == y(0) * R2F0 && x(2) == y(3) * R2F0 && x(3) == y(2) * R2F0 ||
    x(0) == y(1) * R0F1 && x(1) == y(0) * R0F1 && x(2) == y(3) * R0F1 && x(3) == y(2) * R0F1 ||
    (x(2).id == 0 || x(3).id == 0) && (y(2).id == 0 || y(3).id == 0)
  )

}
