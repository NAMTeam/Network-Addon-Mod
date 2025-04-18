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
        checkConversionTiles(bb.get, dd.get)
      }.flatten.orElse {
        Option.when(aa.isDefined && cc.isDefined) {
          checkConversionTiles(aa.get.map(_ * R2F0).asInstanceOf[::[Tile]], cc.get.map(_ * R2F0).asInstanceOf[::[Tile]])
        }.flatten.orElse {
          None // TODO
          // Option.when(cc.isDefined && dd.isDefined) {
          //   checkOutputTiles(cc.get, dd.get)
          // }.flatten
        }
      }

    failureOpt.map(_ -> driveside)
  }

  /** Finds bad segment orientations by looking at tiles 1 and 3 or 2 and 4 of a rule.
    */
  def checkConversionTiles(bb: ::[Tile], dd: ::[Tile]): Option[Failure] = {
    if (bb.exists(t1 => dd.exists(t2 => isBaseOrientationDifferent(t1, t2)))) {
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
      } else false
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
  def checkOutputTiles(cc: ::[Tile], dd: ::[Tile]): Option[Failure] = {
    // if (cc.nonEmpty && dd.nonEmpty && !cc.exists(t1 => dd.exists(t2 => areSegmentsConnecting(t1, t2)))) {
    //   Some(s"output flags do not connect properly: ${cc.head} | ${dd.head}")
    // } else {
    //   None
    // }
    None
  }

  // def areSegmentsConnecting(tile1: Tile, tile2: Tile): Boolean = {
  //   val segs1 = tile1.segs.filter(_.flags(2) != 0)
  //   val segs2 = tile2.segs.filter(_.flags(0) != 0)
  //   if (segs1.size != segs2.size) {
  //     false
  //   } else {
  //     // masking out other edges
  //     (segs1.toSeq.map(s => ((s.network, s.flags.manifest.reverse(s.flags(2))), s.flags.manifest)).sortBy(_._1)
  //     == segs2.toSeq.map(s => ((s.network, s.flags(0)), s.flags.manifest)).sortBy(_._1))
  //   }
  // }


}
