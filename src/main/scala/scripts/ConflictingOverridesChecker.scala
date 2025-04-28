package com.sc4nam.scripts

import io.github.memo33.metarules.meta.{RotFlip, EquivRule, Rule, IdTile}
import RotFlip._
import com.sc4nam.module._
import Rul2Model.{Driveside, Rhd, Lhd, RhdAndLhd, rulesHaveSameOutput}

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
object ConflictingOverridesChecker extends Rul2Checker {

  type Failure = Rule[IdTile]

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
    LOGGER.info("Searching for conflicting overrides in RUL2 code")
    if (args.isEmpty) {
      val result = runChecks(updateMode = false)
      val msg = s"Found ${result.numFailures} new conflicting RUL2 overrides."
      if (result.numFailures > 0) {
        LOGGER.severe(s"$msg Please avoid introducing new conflicts. Instead verify whether these overrides really have the intended effect and consider rewriting or removing them.")
        System.exit(1)
      } else {
        LOGGER.info(msg)
      }
    } else if (args.sameElements(Seq("--update"))) {
      val result = runChecks(updateMode = true)
      val msg = s"Found ${result.numFailures} conflicting RUL2 overrides (removed ${result.removed}, added ${result.added})."
      if (result.numFailures > 0) {
        LOGGER.warning(msg)
      } else {
        LOGGER.info(msg)
      }
    } else {
      LOGGER.severe("wrong arguments")
      System.exit(2)
    }
  }

  def failureToString(x: Rule[IdTile], line: String, y: Failure): String =
    s"${x(0)},${x(1)}=${x(2)},${x(3)} conflicts with ${y(0)},${y(1)}=${y(2)},${y(3)}"

  val rulesRhd = collection.mutable.Map.empty[EquivRule, Rule[IdTile]]
  val rulesLhd = collection.mutable.Map.empty[EquivRule, Rule[IdTile]]
  val rulesShared = collection.mutable.Map.empty[EquivRule, Rule[IdTile]]
  val lookupRuleRhd: PartialFunction[EquivRule, Rule[IdTile]] = rulesShared.orElse(rulesRhd)  // the two maps should be disjoint
  val lookupRuleLhd: PartialFunction[EquivRule, Rule[IdTile]] = rulesShared.orElse(rulesLhd)  // the two maps should be disjoint

  // simultaneously builds the RUL2 cache and looks for conflicts
  def processRule(rule: Rule[IdTile], driveside: Driveside, line: String): Option[(Failure, Driveside)] = driveside match {
    case Rhd =>
      val key = new EquivRule(rule)
      lookupRuleRhd.unapply(key) match {
        case Some(rule2) => if (rulesHaveSameOutput(rule, rule2)) None else Some((rule2, RhdAndLhd))
        case None => rulesRhd.addOne(key, rule); None
      }
    case Lhd =>
      val key = new EquivRule(rule)
      lookupRuleLhd.unapply(key) match {
        case Some(rule2) => if (rulesHaveSameOutput(rule, rule2)) None else Some((rule2, RhdAndLhd))
        case None => rulesLhd.addOne(key, rule); None
      }
    case RhdAndLhd =>
      val key = new EquivRule(rule)
      rulesShared.get(key) match {
        case Some(rule2) => if (rulesHaveSameOutput(rule, rule2)) None else Some((rule2, RhdAndLhd))
        case None =>
          if (!rulesRhd.contains(key) && !rulesLhd.contains(key)) {
            rulesShared.addOne(key, rule); None
          } else {
            (rulesRhd.get(key).filterNot(rulesHaveSameOutput(rule, _)), rulesLhd.get(key).filterNot(rulesHaveSameOutput(rule, _))) match {
              case (None, None) => rulesShared.addOne(key, rule); None  // no conflict
              case (Some(rule2), None) => Some((rule2, Rhd))  // conflict only with RHD rules
              case (None, Some(rule3)) => Some((rule3, Lhd))  // conflict only with LHD rules
              case (Some(rule2), Some(rule3)) => Some((rule2, RhdAndLhd)) // conflict with both
            }
          }
      }
  }

}
