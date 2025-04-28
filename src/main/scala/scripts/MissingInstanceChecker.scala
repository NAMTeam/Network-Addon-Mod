package com.sc4nam.scripts

import io.github.memo33.scdbpf, scdbpf.strategy.throwExceptions
import io.github.memo33.metarules.meta.{RotFlip, EquivRule, Rule, IdTile}
import com.sc4nam.module._
import Rul2Model.{Driveside, Rhd, Lhd, RhdAndLhd}

object MissingInstanceChecker extends Rul2Checker {

  type Failure = Unit

  val allTags = Seq("missing_paths")

  val tagOf: Driveside => String = _ => allTags.head

  def failureToString(rule: Rule[IdTile], line: String, failure: Failure): String = s"missing paths: $line"

  val pathIds = collection.mutable.Set.empty[Int]

  /** Scans the RUL2 Controller files for IIDs that lack corresponding SC4PATHS
    * files.
    *
    * The SimCity_1.dat file and the folder containing all NAM dat files are
    * needed as input in order to check existence of SC4PATHS files.
    *
    * The identified rules are tagged in place as "missing_paths" as inline comment.
    *
    * Run with
    * {{{
    * sbt 'runMain com.sc4nam.scripts.MissingInstanceChecker "<path-to-SimCity_1.dat>" "<path-to-all-NAM-dat-files>'
    * }}}
    */
  def main(args: Array[String]): Unit = {
    if (args.length-1 != 2) {
      System.err.println(s"Usage: Pass two arguments (instead of ${args.length-1}):")
      System.err.println("  1. path to SimCity_1.dat")
      System.err.println("  2. path to folder containing all NAM dat files")
    } else {
      val sc1datFile = new java.io.File(args(1))
      val namInstallationDir = new java.io.File(args(2))

      LOGGER.info(s"Scanning for IIDs of all SC4Path files in $namInstallationDir")
      pathIds ++=
        (scdbpf.Experimental.dbpfFileTreeIterator(namInstallationDir) ++ Iterator(sc1datFile))
          .flatMap(scdbpf.DbpfFile.read(_).entries)
          .filter(_.tgi.matches(scdbpf.Tgi.Sc4Path))
          .map(_.tgi.iid)
      LOGGER.info(s"Found ${pathIds.size} SC4Path files")

      LOGGER.info("Searching for rules with missing SC4Path IIDs in RUL2 code")
      runChecks(updateMode = true)
    }
  }

  def processRule(rule: Rule[IdTile], driveside: Driveside, line: String): Option[(Failure, Driveside)] = {
    if (rule.forall(tile => tile.id == 0 || pathIds.contains(tile.id))) {
      None  // success
    } else {
      Some(((), driveside))  // failure
    }
  }
}
