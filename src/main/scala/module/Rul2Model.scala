package com.sc4nam.module

import java.nio.file.{Files, Paths, Path}
import io.github.memo33.metarules.meta.{RotFlip, EquivRule, Rule, IdTile}
import RotFlip._
import syntax.IdTile

object Rul2Model {

  sealed trait Driveside
  case object Rhd extends Driveside
  case object Lhd extends Driveside
  case object RhdAndLhd extends Driveside

  def isRulFile(path: Path) = {
    val s = path.getFileName().toString()
    s.endsWith(".txt") || s.endsWith(".rul")
  }

  /** Iterate all RUL files in a directory tree, following the order of the NAMControllerCompiler,
    * see https://github.com/memo33/NAMControllerCompiler/blob/4c375beaacf2d69aee96d923d364e47128b09fd3/src/controller/tasks/CollectRULsTask.java#L60
    */
  def iterateRulFiles(directory: Path): Iterator[Path] = {
    import scala.jdk.StreamConverters._
    val children: Seq[Path] = Files.list(directory).toScala(Seq).sorted
    children.iterator.flatMap { child =>
      if (Files.isDirectory(child))
        iterateRulFiles(child)
      else if (isRulFile(child))
        Iterator(child)
      else
        Iterator.empty
    }
  }

  def drivesideOfFile(path: Path): Driveside = {
    val name = path.getFileName().toString().toLowerCase(java.util.Locale.ENGLISH)
    if (name.contains("rhd.")) Rhd
    else if (name.contains("lhd.")) Lhd
    else RhdAndLhd
  }

  def parseRule(line: String): Option[Rule[IdTile]] = {
    val chunk = line.split(";|\\[", 2)(0).trim
    if (chunk.isEmpty) {
      None
    } else try {
      val ts = chunk.split(",|=").grouped(3).toSeq.map(tup =>
        IdTile(java.lang.Long.decode(tup(0)).toInt, RotFlip(tup(1).toInt, tup(2).toInt)))
      Some(Rule(ts(0), ts(1), ts(2), ts(3)))
    } catch {
      case _: IllegalArgumentException =>  // syntax errors in RUL2 code
        // throw new IllegalArgumentException(line)
        None
    }
  }

  val lhdPrefix = ";###LHD###"
  val rhdPrefix = ";###RHD###"

  def parseRuleWithRestrictedDriveside(line: String, driveside: Driveside): Option[(Rule[IdTile], Driveside)] = {
    val line1 = line.trim()
    if (line1.startsWith(lhdPrefix)) {
      if (driveside == Rhd) None else parseRule(line1.substring(lhdPrefix.length)).map(_ -> Lhd)
    } else if (line1.startsWith(rhdPrefix)) {
      if (driveside == Lhd) None else parseRule(line1.substring(rhdPrefix.length)).map(_ -> Rhd)
    } else {
      parseRule(line1).map(_ -> driveside)
    }
  }

  def applyRule(rule: Rule[IdTile], t0: IdTile, t1: IdTile): Option[(IdTile, IdTile)] = {
    if (t0 == rule(0) && t1 == rule(1)) Some((rule(2), rule(3)))
    else if (t0 == rule(0) * R2F1 && t1 == rule(1) * R2F1) Some((rule(2) * R2F1, rule(3) * R2F1))
    else if (t0 == rule(1) * R0F1 && t1 == rule(0) * R0F1) Some((rule(3) * R0F1, rule(2) * R0F1))
    else if (t0 == rule(1) * R2F0 && t1 == rule(0) * R2F0) Some((rule(3) * R2F0, rule(2) * R2F0))
    else None
  }

  def load(directory: Path): Rul2Model = {
    val rulesRhd = collection.mutable.Map.empty[EquivRule, Rule[IdTile]]
    val rulesLhd = collection.mutable.Map.empty[EquivRule, Rule[IdTile]]
    val rulesShared = collection.mutable.Map.empty[EquivRule, Rule[IdTile]]
    val lookupRuleRhd: PartialFunction[EquivRule, Rule[IdTile]] = rulesShared.orElse(rulesRhd)  // the two maps should be disjoint
    val lookupRuleLhd: PartialFunction[EquivRule, Rule[IdTile]] = rulesShared.orElse(rulesLhd)  // the two maps should be disjoint

    LOGGER.info(s"""Loading all RUL2 code for RHD and LHD from "$directory"""")
    iterateRulFiles(directory).foreach { path =>
      val drivesideFile = drivesideOfFile(path)
      scala.util.Using.resource(new java.util.Scanner(path.toFile(), "UTF-8")) { scanner =>
        while(scanner.hasNextLine()) {
          parseRuleWithRestrictedDriveside(scanner.nextLine(), drivesideFile) match {
            // only the first matching rule is loaded by the game, so we store only the first one read
            case Some((rule, Rhd)) =>
              val key = new EquivRule(rule)
              if (!rulesShared.contains(key) && !rulesRhd.contains(key))
                rulesRhd.addOne(key, rule)
            case Some((rule, Lhd)) =>
              val key = new EquivRule(rule)
              if (!rulesShared.contains(key) && !rulesLhd.contains(key))
                rulesLhd.addOne(key, rule)
            case Some((rule, RhdAndLhd)) =>
              val key = new EquivRule(rule)
              if (!rulesShared.contains(key)) {
                if (!rulesRhd.contains(key) && !rulesLhd.contains(key))
                  rulesShared.addOne(key, rule)
                else
                  require(
                    rulesRhd.contains(key) == rulesLhd.contains(key),
                    s"There's an unexpected rule conflict that differs between RHD and LHD: $rule"
                  )
              }
            case None => // ignore
          }
        }
      }
    }

    new Rul2Model(lookupRuleRhd, lookupRuleLhd)
  }

  def evaluateRulesOnce(lookupRule: PartialFunction[EquivRule, Rule[IdTile]], t0: IdTile, t1: IdTile): Option[(IdTile, IdTile)] = {
    val key = new EquivRule(Rule(t0, t1, t0, t1))
    lookupRule.unapply(key).flatMap(rule => applyRule(rule, t0, t1))
  }

}

/** Holds all RUL2 code in memory. Instantiate this with `Rul2Model.load`. */
class Rul2Model private (
  val lookupRuleRhd: PartialFunction[EquivRule, Rule[IdTile]],
  val lookupRuleLhd: PartialFunction[EquivRule, Rule[IdTile]],
)
