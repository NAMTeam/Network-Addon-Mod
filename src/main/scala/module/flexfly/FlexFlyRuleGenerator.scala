package com.sc4nam.module
package flexfly

import io.github.memo33.metarules.meta._, syntax._, Network._, RotFlip._, Flags._, Implicits._
import FlexFlyTiles._, Adjacencies._, NetworkProperties._

object FlexFlyRuleGenerator {

  val orientations = Seq[IntFlags => IntFlags](identity _, reverseIntFlags _)
  private[flexfly] val deactivated = Rhw10c + L1Rhw10c + L2Rhw10c

  /** the directions of a network for which the north edge is a shoulder (possibly empty) */
  def directionsWithShoulderNorth(n: Network) = {
    val b = List.newBuilder[IntFlags]
    if (hasRightShoulder(n)) b += EW
    if (hasLeftShoulder(n)) b += WE
    b.result()
  }
}

class FlexFlyRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Stability {
  import FlexFlyRuleGenerator._

  def start(): Unit = {
    for (orient <- orientations) {
      // orient is responsible for distinguishing between A1 and A2 curve:
      // we only write code for A1 curve, orient reverses all the flags for us
      for (main <- RhwNetworks rangeFrom Mis rangeTo L4Rhw4) {
        // first establish tiles 2 and 4 of base curve which are not anchors
        Rules += main~orient(T1) | Dirtroad~(0,0,0,0) | % | main~orient(T2)
        Rules += main~orient(T3) | Dirtroad~(0,0,0,0) | % | main~orient(T4)
        Rules += main~orient(T3) * R1F0 | Dirtroad~(0,0,0,0) | % | main~orient(T2) * R1F0
        // connect tile 0 to orthogonal network and tile 6 to diagonal
        Rules += (Dirtroad ~> main)~orient(EW) | main~orient(T0)
        Rules += main~orient(T6) * R3F0 | (Dirtroad ~> main)~orient(NW)
        createRules()

        for (minor <- RhwNetworks if minor.height != main.height && !deactivated(minor); base <- minor.base) {
          val minDirs = directionsWithShoulderNorth(minor)
          // crossings of anchor tiles 0, 1, 3 and 6
          for ((t, rot) <- Seq(T0 -> R1F0, T0 -> R3F0, T1 -> R1F0, T1 -> R3F0, T3 -> R3F0, T6 -> R1F0)) {
            Rules += main~orient(t) * rot | minor~WE~EW | main~orient(t) * rot & minor~WE~EW | %   // t < orth
            Rules += main~orient(t) * rot & minor~WE~EW | (base ~> minor)~WE~EW   // t > orth
          }
          for ((t, rot) <- Seq(T3 -> R0F1, T6 -> R0F0); dir <- minDirs) { // same as above but restricted to physically possible directions
            Rules += main~orient(t) * rot | minor~dir | main~orient(t) * rot & minor~dir | %   // t < orth
            Rules += main~orient(t) * rot & minor~dir | base~dir | % | minor~dir   // t > orth
          }
          // of non-achor tiles 2 (usual direction) and 4 (alternative direction)
          for (((t, rot), physicalDirs) <- Seq(T2 -> R1F0 -> Seq(EW, WE), T4 -> R0F0 -> minDirs); dir <- physicalDirs) {
            val minSeg = minor~dir;            val baseSeg = base~dir
            val t3Seg = main~orient(T3) * rot; val tSeg = main~orient(t) * rot
            Rules += t3Seg & minSeg | baseSeg       | %              | tSeg & minSeg   // T3 > t
            Rules += t3Seg & minSeg | minSeg        | %              | tSeg & minSeg   // T3 > t (stability)
            Rules += t3Seg          | minSeg        | t3Seg & minSeg | tSeg & minSeg   // T3 < orth
            Rules += t3Seg          | tSeg & minSeg | t3Seg & minSeg | %               // T3 < t
            Rules += tSeg & minSeg  | baseSeg       | %              | minSeg          // t > orth
          }
          // moreover, we need to make sure the override of minor carries over
          // between tiles 3 and 6
          for (dir <- minDirs) {
            Rules += main~orient(T3) & minor~dir | main~orient(T6) | % | main~orient(T6) & minor~dir   // T3 > T6
            Rules += main~orient(T3) | main~orient(T6) & minor~dir | main~orient(T3) & minor~dir | %   // T3 < T6
          }

          // Now we still need to connect the end tiles to orth or diag network
          // if crossings are present.
          // First we consider cases in which only one of the two tiles has crossing
          for (minDir <- minDirs) {
            Rules += (Dirtroad ~> main)~orient(EW)                       | main~orient(T0) & minor~minDir * R3F0
            Rules += (Dirtroad ~> main)~orient(EW) & minor~minDir * R1F0 | main~orient(T0)
            Rules += main~orient(T6) * R3F0                              | (Dirtroad ~> main)~orient(NW) & minor~minDir * R3F0
            if (!isTripleTile(minor)) { // otherwise physically impossible
              Rules += main~orient(T6) * R3F0      & minor~minDir * R1F0 | (Dirtroad ~> main)~orient(NW)
            }
          }

          // additional crossing of minor and tile 6 in different direction
          Rules += main~orient(T6) * R3F0 & minor~WE~EW | (Dirtroad ~> main)~orient(NW) & (base ~> minor)~WE~EW   // T6 > OxD
          Rules += main~orient(T6) * R3F0 & minor~WE~EW | (Dirtroad ~> main)~orient(NW) & minor~WE~EW             // stability
          Rules += main~orient(T6) * R3F0 & minor~WE~EW | main~orient(NW)               & (base ~> minor)~WE~EW   // stability
          Rules += main~orient(T6) * R3F0 | main~orient(NW) & minor~WE~EW | main~orient(T6) * R3F0 & minor~WE~EW | %   // T6 < OxD

          // Now we consider cases involving two adjacent crossing networks.
          // Due to DLL-based adjacencies, we only need to consider the *inner* adjacencies of multitile networks, if any.
          for ((other, directions) <- Adjacencies.multitileNetworks.getOrElse(minor, Seq.empty)
               if other.isRhw && other != Dirtroad && other.height == minor.height && !deactivated(other)) {
            val (minDir, otherDir) = directions match {
              case NSNS => (NS, NS)
              case NSSN => (NS, SN)
              case SNNS => (SN, NS)
              case SNSN => (SN, SN)
            }
            Rules += (Dirtroad ~> main)~orient(EW) & minor~minDir | main~orient(T0) & other~otherDir
            if (otherDir == NS && hasLeftShoulder(other) || otherDir == SN && hasRightShoulder(other)) { // skip impossible crossings
              Rules += (Dirtroad ~> main)~orient(SE) & minor~minDir | main~orient(T6) * R1F0 & other~otherDir
            }
          }
        }

        // FlexFly × FlexFly
        for {
          minor <- RhwNetworks rangeFrom Mis rangeTo L4Rhw4
          if main.height <= 3 && minor.height < main.height
          o2 <- orientations
          if orient == o2 && orient == orientations(0)  // currently inside×inside curves only
        } /*do*/ {
          // 4-tile gap
          Rules ++= stabilize( main~orient(T1) | minor~o2(T1) * R0F1 | main~orient(T1) & minor~o2(T2) * R0F1 | main~orient(T2) & minor~o2(T1) * R0F1 )
          // 2-tile gap
          Rules ++= stabilize( main~orient(T3) | minor~o2(T3) * R0F1 | main~orient(T3) & minor~o2(T4) * R0F1 | main~orient(T4) & minor~o2(T3) * R0F1 )
          // 0-tile gap
          Rules ++= stabilize( main~orient(T3) * R1F0 | minor~o2(T3) * R1F1 | main~orient(T3) * R1F0 & minor~o2(T2) * R1F1 | main~orient(T2) * R1F0 & minor~o2(T3) * R1F1 )

          // crossing of a third network with two adjacent FlexFlys
          // (These are not strictly needed if starters are used, so we try to cut down the large number of adjacencies to a useful subset)
          // (disabled, as these are redundant with DLL-based adjacencies)
          // for {
          //   third <- RhwNetworks
          //   if third.height != main.height && third.height != minor.height && !deactivated(third)
          //   if third.height <= 2 && orient == o2 && orient == orientations(0)  // limits adjacencies to inside curves
          //   base <- third.base
          //   dir <- directionsWithShoulderNorth(third)
          // } /*do*/ {
          //   Rules += minor~o2(T3) * R0F1 & third~dir | main~orient(T3) * R0F0 | % | main~orient(T3) * R0F0 & third~dir  // 4-tile gap
          //   Rules += minor~o2(T3) * R3F0 & third~dir | main~orient(T3) * R3F1 | % | main~orient(T3) * R3F1 & third~dir  // 2-tile gap
          //   Rules += minor~o2(T1) * R3F0 & third~dir | main~orient(T1) * R3F1 | % | main~orient(T1) * R3F1 & third~dir  // 0-tile gap
          //   Rules += minor~o2(T3) * R0F1 | main~orient(T3) * R0F0 & third~dir | minor~o2(T3) * R0F1 & third~dir | %  // 4-tile gap
          //   Rules += minor~o2(T3) * R3F0 | main~orient(T3) * R3F1 & third~dir | minor~o2(T3) * R3F0 & third~dir | %  // 2-tile gap
          //   Rules += minor~o2(T1) * R3F0 | main~orient(T1) * R3F1 & third~dir | minor~o2(T1) * R3F0 & third~dir | %  // 0-tile gap
          // }
        }
        createRules()
      }
    }
  }

}
