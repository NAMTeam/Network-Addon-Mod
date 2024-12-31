package com.sc4nam.scripts

import java.nio.file.{Files, Paths, Path}
import com.sc4nam.module._
import io.github.memo33.metarules.meta.{RotFlip, Rule, EquivRule, IdTile}
import RotFlip._
import syntax.IdTile
import SanityChecker.{fileEndsWithNewline, linePatternIncludingNewlines}
import Rul2Model.{iterateRulFiles, parseRuleWithRestrictedDriveside, Rhd, Lhd, RhdAndLhd, drivesideOfFile, applyRule}

/** Run with `SBT_OPTS="-Xmx2G" sbt "runMain com.sc4nam.scripts.RedundantAdjacenciesChecker"`.
  * Note that this increases the heap size for more memory.
  * Takes about 4 minutes.
  */
object RedundantAdjacenciesChecker {

  /** Scans the Controller/RUL2/ folder for adjancy RUL2 code that is
    * redundant with the DLL RUL2 engine.
    *
    * The respective lines are commented out and tagged as "; redundant-adjacency".
    *
    * Make sure all your changes to files are committed to git beforehand,
    * as this modifies files in place.
    *
    * Afterwards, you need to manually look through the changes and remove the
    * redundant code.
    */
  def main(args: Array[String]): Unit = {
    val rul2 = Rul2Model.load(Paths.get("Controller/RUL2"))
    checkRedundantAdjacencies(rul2)
  }

  def checkRedundantAdjacencies(rul2: Rul2Model): Unit = {
    LOGGER.info("Searching for redundant adjacencies in RUL2 code")
    iterateRulFiles(Paths.get("Controller/RUL2")).foreach { path =>
        val drivesideFile = drivesideOfFile(path)
        val tmpPath = path.resolveSibling(path.getFileName().toString() + ".tmp")
        val endsWithNewline = fileEndsWithNewline(path)  // attempt to preserve missing newlines at end of files to avoid noise
        scala.util.Using.resources(
          new java.util.Scanner(path.toFile(), "UTF-8").useDelimiter(linePatternIncludingNewlines),
          new java.io.PrintWriter(tmpPath.toFile(), "UTF-8")
        ) { (lineScanner, printer) =>
          while (lineScanner.hasNext()) {
            val line = lineScanner.next()

            val redundant =
              parseRuleWithRestrictedDriveside(line, drivesideFile) match {
                case Some((rule, Rhd)) => isRedundantAdjacency(rule, rul2.lookupRuleRhd)
                case Some((rule, Lhd)) => isRedundantAdjacency(rule, rul2.lookupRuleLhd)
                case Some((rule, RhdAndLhd)) =>
                  val b = isRedundantAdjacency(rule, rul2.lookupRuleRhd)
                  require(
                    b == isRedundantAdjacency(rule, rul2.lookupRuleLhd),
                    s"Redundancies should be the same for RHD and LHD: $rule"  // hopefully this will always be the case
                  )
                  b
                case None => false  // comments are not redundant
              }

            if (redundant) {
              printer.println(s";${line.stripLineEnd}; redundant-adjacency")  // comments out the line
            } else {
              printer.print(line)  // preserving original linebreaks
            }
          }
        }
        Files.move(tmpPath, path, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    }
  }

  val orthogonalSurrogateTiles = Seq(
    IdTile(0x00004B00, R1F0),  // Road
    IdTile(0x57000000, R1F0),  // Dirtroad
    IdTile(0x05004B00, R1F0),  // Street
    IdTile(0x5D540000, R1F0),  // Rail
    IdTile(0x08031500, R1F0),  // Lightrail
    IdTile(0x09004B00, R1F0),  // Onewayroad  (TODO or 0x5f940300?)
    IdTile(0x04006100, R3F0),  // Avenue
    IdTile(0x0D031500, R1F0),  // Monorail
  ).flatMap(t => Seq(t, t * R2F0))

  val diagonalSurrogateTiles = Seq(
    (IdTile(0x00000A00, R1F0), IdTile(0x00000A00, R3F0)),  // Road
    (IdTile(0x57000200, R1F0), IdTile(0x57000200, R3F0)),  // Dirtroad
    (IdTile(0x5F500200, R1F0), IdTile(0x5F500200, R3F0)),  // Street
    (IdTile(0x5D540100, R1F0), IdTile(0x5D540100, R3F0)),  // Rail
    (IdTile(0x08001A00, R1F0), IdTile(0x08001A00, R3F0)),  // Lightrail
    (IdTile(0x09000A00, R1F0), IdTile(0x09000A00, R3F0)),  // Onewayroad  (TODO or 0x5f94....?)
    (IdTile(0x04000200, R2F0), IdTile(0x04003800, R0F0)),  // Avenue~SW | Avenue~SharedDiagLeft
    (IdTile(0x04000200, R2F0), IdTile(0x04003800, R2F0)),  // Avenue~SW | Avenue~SharedDiagLeft (here we probably need this extra rotation to remove corresponding RUL2 code)
    (IdTile(0x04003800, R0F0), IdTile(0x04000200, R0F0)),  // Avenue~SharedDiagLeft | Avenue~SW
    (IdTile(0x04003800, R2F0), IdTile(0x04000200, R0F0)),  // Avenue~SharedDiagLeft | Avenue~SW (here we probably need this extra rotation to remove corresponding RUL2 code)
    (IdTile(0x0D001A00, R1F0), IdTile(0x0D001A00, R3F0)),  // Monorail
  ).flatMap { case (a,b) => Seq(true, false).map { southBound =>
    val rot = if (southBound) R0F0 else R1F0
    (a * rot, b * rot, southBound)
  }}

  /** Checks if adjacency overrides a -> b and b -> c exist (in that order and
    * direction) (where b is surrogate tile) and matches expected result
    * aExpected, cExpected.
    */
  def connectingOrthOverridesExist(lookupRule: PartialFunction[EquivRule, Rule[IdTile]], a: IdTile, b: IdTile, c: IdTile, aExpected: IdTile, cExpected: IdTile): Boolean = {
    Rul2Model.evaluateRulesOnce(lookupRule, a, b) match {
      case Some((a1, b1)) if a1 == a && a1.id != b1.id =>
        Rul2Model.evaluateRulesOnce(lookupRule, b1, c) match {
          case Some((b2, c2)) if b2 == b1 && b2.id != c2.id && c2 != c =>
            // found two overrides connecting a to c, so check if result of their application is as expected
            a1 == aExpected && c2 == cExpected
          case _ => false
        }
      case _ => false
    }
  }

  /** Checks if adjacency overrides a -> b and b*rot -> c*rot and c -> d exist
    * and matches expected result aExpected, dExpected.
    * Here, b and c are diagonal surrogate tiles rotated for southbound or
    * northbound direction.
    */
  def connectingDiagOverridesExist(lookupRule: PartialFunction[EquivRule, Rule[IdTile]], a: IdTile, b: IdTile, c: IdTile, d: IdTile, southBound: Boolean, aExpected: IdTile, dExpected: IdTile): Boolean = {
    Rul2Model.evaluateRulesOnce(lookupRule, a, b) match {
      case Some((a1, b1)) if a1 == a && a1.id != b1.id =>
        val rot = if (southBound) R3F0 else R1F0
        Rul2Model.evaluateRulesOnce(lookupRule, b1 * rot, c * rot) match {
          case Some((b2rot, c2rot)) =>
            val b2 = b2rot * (R0F0 / rot)
            val c2 = c2rot * (R0F0 / rot)
            if (b2 == b1 && c2 != c) {
              Rul2Model.evaluateRulesOnce(lookupRule, c2, d) match {
                case Some((c3, d3)) if c3 == c2 && d3.id != c3.id && d3.id != b2.id && d3 != d =>
                  a1 == aExpected && d3 == dExpected
                case _ => false
              }
            } else {
              false
            }
          case _ => false
        }
      case _ => false
    }
  }

  def isRedundantAdjacency(rule: Rule[IdTile], lookupRule: PartialFunction[EquivRule, Rule[IdTile]]): Boolean = {
    val a = rule(0)
    def checkOrth() = {
      val c = rule(1)
      orthogonalSurrogateTiles.exists { b =>
        if (b.id == a.id || b.id == c.id) {  // this would depend on the same rule
          false
        } else {
          (connectingOrthOverridesExist(lookupRule, a, b, c, rule(2), rule(3))  // a -> b -> c
            || connectingOrthOverridesExist(lookupRule, c * R2F0, b * R2F0, a * R2F0, rule(3) * R2F0, rule(2) * R2F0))  // c -> b -> a
        }
      }
    }
    def checkDiag() = {
      val d = rule(1)
      diagonalSurrogateTiles.exists { case (b, c, southBound) =>
        if (c.id == a.id || b.id == d.id) {  // this would depend on the same rule
          false
        } else {
          (connectingDiagOverridesExist(lookupRule, a, b, c, d, southBound, rule(2), rule(3))  // a -> b -> c -> d
            || connectingDiagOverridesExist(lookupRule, d * R2F0, c * R2F0, b * R2F0, a * R2F0, southBound, rule(3) * R2F0, rule(2) * R2F0))  // d -> c -> b -> a
        }
      }
    }
    checkOrth() || checkDiag()
  }

}
