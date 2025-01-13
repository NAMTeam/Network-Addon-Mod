package com.sc4nam.scripts

import java.nio.file.{Files, Paths}
import io.github.memo33.metarules.meta.{RotFlip, EquivRule, Rule, IdTile}
import RotFlip._
import com.sc4nam.module._
import Rul2Model.{iterateRulFiles, parseRuleWithRestrictedDriveside, Driveside, Rhd, Lhd, RhdAndLhd, drivesideOfFile, rulesHaveSameOutput}
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
  *
  * In order to also flag (non-conflicting) duplicates, use one of the following commands:
  * {{{
  * SBT_OPTS="-Xmx2G" sbt "conflictingOverridesCheck --include-duplicates"
  * SBT_OPTS="-Xmx2G" sbt "conflictingOverridesCheck --include-duplicates --update"
  * }}}
  */
object ConflictingOverridesChecker {

  val tagBoth = "conflicting-override"
  val tagRhd = "conflicting-override_rhd"  // underscore (instead of hyphen) for use with regex word boundaries
  val tagLhd = "conflicting-override_lhd"
  val tagDuplicate = "duplicate_override"
  val tagOf = Map[Driveside, String](RhdAndLhd -> tagBoth, Rhd -> tagRhd, Lhd -> tagLhd)
  val allTags = Seq(tagBoth, tagRhd, tagLhd, tagDuplicate)

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
    val updateMode = args.contains("--update")
    val duplicateMode = args.contains("--include-duplicates")
    if (!updateMode) {
      val (numConflicts, _, _, numDuplicates) = checkConflictingRul2(updateMode = updateMode, duplicateMode = duplicateMode)
      val msg = s"Found $numConflicts new conflicting RUL2 overrides." + (if (duplicateMode) s" Found $numDuplicates duplicate RUL2 overrides." else "")
      if (numConflicts > 0) {
        LOGGER.severe(s"$msg Please avoid introducing new conflicts. Instead verify whether these overrides really have the intended effect and consider rewriting or removing them.")
        System.exit(1)
      } else {
        LOGGER.info(msg)
      }
    } else {
      val (numConflicts, removed, added, numDuplicates) = checkConflictingRul2(updateMode = updateMode, duplicateMode = duplicateMode)
      val msg = s"Found $numConflicts conflicting RUL2 overrides (removed $removed, added $added)." + (if (duplicateMode) s" Found $numDuplicates duplicate RUL2 overrides." else "")
      if (numConflicts > 0) {
        LOGGER.warning(msg)
      } else {
        LOGGER.info(msg)
      }
    }
  }

  def checkConflictingRul2(updateMode: Boolean, duplicateMode: Boolean): (Int, Int, Int, Int) = {
    var numConflicts = 0
    var numDuplicates = 0
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

      // Simultaneously builds the RUL2 cache and looks for conflicts.
      // Returns whether rule is duplicate (true) or conflicting (false).
      def findConflict(line: String): Option[(Rule[IdTile], Rule[IdTile], Driveside, Boolean)] = {
        parseRuleWithRestrictedDriveside(line, drivesideFile) match {
          // only the first matching rule is loaded by the game, so we store only the first one read
          case Some((rule, Rhd)) =>
            val key = new EquivRule(rule)
            lookupRuleRhd.unapply(key) match {
              case Some(rule2) => Some((rule, rule2, RhdAndLhd, rulesHaveSameOutput(rule, rule2)))
              case None => rulesRhd.addOne(key, rule); None
            }
          case Some((rule, Lhd)) =>
            val key = new EquivRule(rule)
            lookupRuleLhd.unapply(key) match {
              case Some(rule2) => Some((rule, rule2, RhdAndLhd, rulesHaveSameOutput(rule, rule2)))
              case None => rulesLhd.addOne(key, rule); None
            }
          case Some((rule, RhdAndLhd)) =>
            val key = new EquivRule(rule)
            rulesShared.get(key) match {
              case Some(rule2) => Some((rule, rule2, RhdAndLhd, rulesHaveSameOutput(rule, rule2)))
              case None =>
                (rulesRhd.get(key).map(r => r -> rulesHaveSameOutput(rule, r)), rulesLhd.get(key).map(r => r -> rulesHaveSameOutput(rule, r))) match {
                  case (None, None) => rulesShared.addOne(key, rule); None  // no conflict
                  case (Some((rule1, true)), None) => rulesShared.addOne(key, rule); Some((rule, rule1, Rhd, true))  // no conflict, only duplicate
                  case (Some((rule1, false)), None) => Some((rule, rule1, Rhd, false))  // conflict only with RHD rules
                  case (None, Some((rule2, true))) => rulesShared.addOne(key, rule); Some((rule, rule2, Lhd, true))  // no conflict, only duplicate
                  case (None, Some((rule2, false))) => Some((rule, rule2, Lhd, false))  // conflict only with LHD rules
                  case (Some((rule1, dup1)), Some((rule2, dup2))) => Some((rule, rule1, RhdAndLhd, dup1 && dup2)) // conflict with both
                }
            }
          case None => None
        }
      }

      var lineNumber = 0
      var badFile = false
      def logConflict(x: Rule[IdTile], y: Rule[IdTile], isDuplicate: Boolean): Unit = {
        if (!badFile) {
          LOGGER.info(s"==> $path")
          badFile = true
        }
        if (isDuplicate) {
          LOGGER.warning(s"$lineNumber: ${x(0)},${x(1)}=${x(2)},${x(3)} is duplicate of ${y(0)},${y(1)}=${y(2)},${y(3)}")
        } else {
          LOGGER.warning(s"$lineNumber: ${x(0)},${x(1)}=${x(2)},${x(3)} conflicts with ${y(0)},${y(1)}=${y(2)},${y(3)}")
        }
      }

      if (!updateMode) {
        scala.util.Using.resource(new java.util.Scanner(path.toFile(), "UTF-8")) { scanner =>
          while(scanner.hasNextLine()) {
            val line = scanner.nextLine()
            lineNumber += 1
            findConflict(line) match {
              case Some((rule, rule2, _, isDuplicate)) if duplicateMode || !isDuplicate =>
                if (!line.contains(tagBoth)) {
                  if (isDuplicate) numDuplicates += 1 else numConflicts += 1
                  logConflict(rule, rule2, isDuplicate)
                }
              case _ =>  // ignore
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
              case Some((_, _, driveside, isDuplicate)) if duplicateMode || !isDuplicate =>
                if (!isDuplicate) {
                  numConflicts += 1
                  if (!line.contains(tagBoth)) {
                    added += 1
                  }
                  printer.println(replaceTags(line, add = Some(tagOf(driveside)), remove = allTags))
                } else {  // no conflict, only duplicate
                  numDuplicates += 1
                  printer.println(replaceTags(line, add = Some(tagDuplicate), remove = allTags))
                }
              case _ =>
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
    (numConflicts, removed, added, numDuplicates)
  }

  def replaceTags(line: String, add: Option[String], remove: Seq[String]): String = {
    val line0 =
      remove.foldLeft(line.stripLineEnd) { (line, rem) =>
        line.replaceFirst(s" ?\\b$rem\\b", "").replaceFirst(";\\s*$", "")
      }
    if (add.isDefined) s"$line0; ${add.get}" else line0
  }

}
