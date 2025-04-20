package com.sc4nam.scripts

import io.github.memo33.metarules.meta.{RotFlip, EquivRule, Rule, IdTile, Flags, Flag}
import com.sc4nam.module._
import syntax._, Network._, RotFlip._
import Rul2Model.Driveside

/** Scans RUL2 to check for bad orientations of tiles as well as the individual
  * segments that make up the tile. The check is only performed for IDs that are
  * defined in the metarule ID resolvers, which is needed to obtain the
  * individual segments.
  *
  * Run with
  * {{{
  * sbt "runMain com.sc4nam.scripts.SegmentOrientationChecker"
  * }}}
  * to check all RUL2 code for new segment orientation conflicts (that are not tagged yet).
  * The new conflicts are printed to stdout. Exit code will be non-zero if new
  * conflicts are found.
  *
  * Or run
  * {{{
  * sbt "runMain com.sc4nam.scripts.SegmentOrientationChecker --update"
  * }}}
  * to update the files in "Controller/RUL2" in place by tagging the lines with
  * "bad_segment_orientations" where conflicts are found.
  */
object SegmentOrientationChecker extends Rul2Checker {

  type Failure = String

  val allTags = Seq("bad_segment_orientations")

  val tagOf: Driveside => String = _ => allTags.head

  lazy val reverseResolver = ReverseResolver.create()

  def failureToString(rule: Rule[IdTile], line: String, failure: Failure): String =
    s"bad segment orientations: $line; $failure"

  def main(args: Array[String]): Unit = {
    LOGGER.info("Scanning RUL2 code for incorrect orientations based on metarule ID definitions")
    if (args.isEmpty) {
      val result = runChecks(updateMode = false)
      val msg = s"Found ${result.numFailures} new bad segment orientations in RUL2 overrides."
      if (result.numFailures > 0) {
        LOGGER.severe(s"$msg Please avoid introducing new segment orientation conflicts." +
          " Instead verify that the RUL2 overrides are oriented correctly." +
          " (If the overrides look correct, possible other sources of this error are:" +
          " orientation mismatch between paths and models/textures, incorrect ID definition in metarule ID resolver, or a false positive in the SegmentOrientationChecker script.)")
        System.exit(1)
      } else {
        LOGGER.info(msg)
      }
    } else if (args.sameElements(Seq("--update"))) {
      val result = runChecks(updateMode = true)
      val msg = s"Found ${result.numFailures} bad segment orientations (removed ${result.removed}, added ${result.added})."
      if (result.numFailures > 0) {
        LOGGER.warning(msg)
      } else {
        LOGGER.info(msg)
      }
    } else {
      LOGGER.severe(s"wrong arguments: ${args.mkString(", ")}")
      System.exit(2)
    }


  }

  def processRule(rule: Rule[IdTile], driveside: Driveside, line: String): Option[(Failure, Driveside)] = {
    val aa = reverseResolver.lift(rule(0))
    val bb = reverseResolver.lift(rule(1))
    val cc = reverseResolver.lift(rule(2))
    val dd = reverseResolver.lift(rule(3))

    val failureOpt =
      Option.when(bb.isDefined && dd.isDefined) {
        checkConversionTiles(bb.get, dd.get, rule(1), rule(3))
      }.flatten.orElse {
        Option.when(aa.isDefined && cc.isDefined) {
          checkConversionTiles(
            aa.get.map(_ * R2F0).asInstanceOf[::[Tile]],
            cc.get.map(_ * R2F0).asInstanceOf[::[Tile]],
            rule(0) * R2F0,
            rule(2) * R2F0,
          )
        }.flatten.orElse {
          Option.when(cc.isDefined && dd.isDefined) {
            checkOutputTiles(cc.get, dd.get, rule)
          }.flatten
        }
      }

    failureOpt.map(_ -> driveside)
  }

  /** Finds bad segment orientations by looking at tiles 1 and 3 or 2 and 4 of a rule.
    */
  def checkConversionTiles(bb: ::[Tile], dd: ::[Tile], idTileIn: IdTile, idTileOut: IdTile): Option[Failure] = {
    if (bb.exists(t1 => dd.exists(t2 => isBaseOrientationDifferent(t1, t2) || isUnexpectedRerotation(t1, t2, idTileIn, idTileOut)))) {
      Some("base orientation wrong")
    } else if (bb.exists(t1 => dd.exists(t2 => hasSegmentReversal(t1, t2)))) {
      Some("unexpected segment reversal")
    } else {
      None
    }
  }

  private def baseSegment(seg: Segment): Segment =
    seg.network.base match {
      case Some(n) => Segment(n, Flags(seg.flags, if (n.isSymm) Flag.Bi else Flag.InOut))
      case None => throw IllegalArgumentException(s"base segment can only be constructed for override networks: $seg")
    }

  /** Checks whether tile t2 is a single-segment tile overriding tile t1, but
    * messing up the rotation in an unexpected way.
    */
  def isBaseOrientationDifferent(t1: Tile, t2: Tile): Boolean = {
    if (t1.segs.size == 1 && t2.segs.size == 1 && t1 != t2) {
      val s1 = t1.segs.head
      val s2 = t2.segs.head
      if (s2.network.base.contains(s1.network)) {  // this is an override from base to override-network
        val seg1expected = baseSegment(s2)
        (RotFlip.values -- t1.symmetries).exists(rf => seg1expected * rf == s1)  // segment s1 uses a wrong rotation, as it is not one of the symmetries of the tile
      } else if (s2.network == s1.network) {
        (RotFlip.values -- t1.symmetries).exists(rf => s2 * rf == s1)  // segment s2 uses a wrong rotation, as it is not one of the symmetries of the tile
      } else false
    } else false
  }

  /** If ID stays the same, the tiles must be equal up to symmetries.
    */
  def isUnexpectedRerotation(t1: Tile, t2: Tile, idTile1: IdTile, idTile2: IdTile): Boolean = {
    if (idTile1.id == idTile2.id && idTile1.rf != idTile2.rf) {
      val rf = (R0F0 / idTile1.rf) * idTile2.rf
      !t1.symmetries.contains(rf)
    } else false
  }

  def hasSegmentReversal(t1: Tile, t2: Tile): Boolean = {
    t2.segs.exists { s2 =>
      // plain reversal of a crossing segment
      val s2Reversed: Segment = s2.copy(flags = s2.flags.reverseFlags)
      if (s2Reversed != s2) {
        t1.segs.exists(_ == s2Reversed)
      } else false
    } || t2.segs.exists { s2 =>
      // reversal of the main overriding segment
      if (s2.network.base.isDefined) {
        val s2Base = baseSegment(s2)
        val s2BaseReversed = s2Base.copy(flags = s2Base.flags.reverseFlags)
        if (s2BaseReversed != s2Base) {
          t1.segs.exists(_ == s2BaseReversed)
        } else false
      } else false
    }
  }

  /** Finds bad segment orientations by looking at tiles 3 and 4 of a rule.
    */
  def checkOutputTiles(cc: ::[Tile], dd: ::[Tile], rule: Rule[IdTile]): Option[Failure] = {
    if (cc.forall(t1 => dd.forall(t2 => areSegmentsBadlyConnected(t1, t2, rule)))
      && !badConnectionsFalsePositives.contains(rule)
    ) {
      Some(s"badly connected output segments")
    } else {
      None
    }
  }

  /** A simplified flag that distinguishes between curve directions and orthogonal directions. */
  def basicFlag(flag: Int): Int = (flag.abs % 10) match {
    case 1 => 1
    case 3 => 3
    case _ => 2
  }

  private val dxdDirtroadTiles = Set(IdTile(0x5700aa00, R0F0), IdTile(0x5700aa00, R2F0))
  private val badConnectionsFalsePositives = Set.empty[Rule[IdTile]]  // currently none

  def areSegmentsBadlyConnected(tile1: Tile, tile2: Tile, rule: Rule[IdTile]): Boolean = {
    val connectingSegs1 = tile1.segs.filter(_.flags(2) != 0)
    val connectingSegs2 = tile2.segs.filter(_.flags(0) != 0)
    val commonNetworks = connectingSegs1.map(_.network).intersect(connectingSegs2.map(_.network))
    if (commonNetworks.isEmpty) {
      connectingSegs1.nonEmpty && connectingSegs2.nonEmpty
    } else {

      def wellConnected(connectingSegs1: Set[Segment], connectingSegs2: Set[Segment], east: Int, west: Int): Boolean =
        connectingSegs2.forall { s2 =>
          if (!commonNetworks.contains(s2.network)) {
            val is3LevelCrossingFalsePositive =  // specifically detect O×D adjacent to D×D to avoid a false positive
              (s2.network.base.contains(Dirtroad)
                && connectingSegs2.size == 2
                && connectingSegs1.size == 1
                && s2.flags(1) == 0
                && s2.flags(3) == 0
                && basicFlag(s2.flags(west)) == 2
                && dxdDirtroadTiles.contains(rule(1))
              )
            if (is3LevelCrossingFalsePositive) {
              true
            } else {
              s2.network.base.isEmpty  // if no matching segment exists, it should be a base network (e.g. for some intermediate overrides)
            }
          } else {
            connectingSegs1.exists { s1 =>
              (s1.network == s2.network
                && (if (s2.network.isSymm) true else s1.flags(east).sign != s2.flags(west).sign)  // network direction must match
                // && basicFlag(s1.flags(east)) == basicFlag(s2.flags(west))  // curve direction must match (TODO produces some false positives)
              )
            }
          }
        }

      !wellConnected(connectingSegs1, connectingSegs2, 2, 0) || !wellConnected(connectingSegs2, connectingSegs1, 0, 2)
    }
  }

}
