package com.sc4nam.module

import io.github.memo33.metarules.meta._
import syntax._, Network._, Flags._, RotFlip._, Implicits._
import NetworkProperties._

object CrossingGenerator {

  def crossingNetworksOf(main: Network): Iterator[Network] = {
    for {
      minor <- Network.values.iterator
      if minor != Subway && !isHrw(minor) && (main.isRhw || minor.isRhw ||
         (main.isNwm && (minor.isRhw || minor.isNwm || minor.base.isEmpty)) ||
         (main.isNwm && isSingleTile(main) && main.height == 0 && (minor == L1Dtr || minor == L2Dtr)) //||
         //(main.isViaduct || minor.isViaduct)
         ) && intersectionAllowed(main, minor)
    } yield minor
  }
}

trait CrossingGenerator extends Adjacencies { this: RuleGenerator =>

  /** Creates generic rules for +-intersections and overpasses (O×O, O×D, D×O, D×D). */
  def createCrossingRules(main: Network, minor: Network): Unit = {
    for (base <- main.base) {
      // entry (override from straight tile to first crossing tile)
      if (intersectionAllowed(base, minor)) { // skips e.g. preexisting L0Rhw2 x L0Rhw6c in second tile
        def entryCode(orient: Segment => Segment) = withSharedDiagonals {
          Rules += main~WE    | (base ~> main)~WE & orient(minor~NS)      // OxO
          Rules += main~WE~EW | (base ~> main)~WE~EW & orient(minor~ES)   // OxD
          Rules += main~SE~ES | (base ~> main)~WN~NW & orient(minor~NS)   // DxO
          Rules += main~SE~ES | (base ~> main)~WN~NW & orient(minor~NE)   // DxD
          if (main.isNwm && !minor.isRhw) withResolvableRulesOnly {  // T intersections are not defined for all network combinations
            Rules += main~WE~EW | (base ~> main)~WE~EW & orient(minor~CS)   // OxO T 1
            Rules += main~WE~EW | (base ~> main)~WE~EW & orient(minor~NC)   // OxO T 2
          }
          // Shared diagonals on minor are not relevant here since the shared diagonal is an inner tile (i.e. without an edge).
        }
        if (hasRightShoulder(minor)) entryCode(identity)
        if (hasLeftShoulder(minor)) entryCode(_.reverse)
        createRules() // duplicate rules will be removed
      }
      // exit (override from last crossing tile to straight tile)
      {
        def exitCode(orient: Segment => Segment) = withSharedDiagonals {
          Rules += main~WE & orient(minor~SN)    | (base ~> main)~WE      // OxO
          Rules += main~WE~EW & orient(minor~WN) | (base ~> main)~WE~EW   // OxD
          Rules += main~SE~ES & orient(minor~SN) | (base ~> main)~WN~NW   // DxO
          Rules += main~SE~ES & orient(minor~SW) | (base ~> main)~WN~NW   // DxD
          // Shared diagonals on minor are not relevant here since the shared diagonal is an inner tile (i.e. without an edge).
        }
        if (hasRightShoulder(minor)) exitCode(identity)
        if (hasLeftShoulder(minor)) exitCode(_.reverse)
        createRules()
      }
      // Inside diagonal crossings (Diagonal crossings consist of two or more tiles, so the following rules ensure
      // that the override carries over between those inner-intersection tiles)
      withSharedDiagonals {
        if (intersectionAllowed(base, minor)) {
          Rules += main~WE~EW & minor~NE | (base ~> main)~WE~EW & minor~WS   // OxD
          Rules += main~WE~EW & minor~EN | (base ~> main)~WE~EW & minor~SW
          Rules += main~SE~ES & minor~WE | (base ~> main)~WN~NW & minor~WE   // DxO
          Rules += main~SE~ES & minor~EW | (base ~> main)~WN~NW & minor~EW
          Rules += main~SE~ES & minor~NE | (base ~> main)~WN~NW & minor~WS   // DxD
          Rules += main~SE~ES & minor~EN | (base ~> main)~WN~NW & minor~SW
        }
        // stability
        if (minor >= main) for (minBase <- minor.base) {  // If minor < main, then the following rules have already been added as part of the overrides of minor.
          Rules += main~WE~EW & minor~NE | (base ~> main)~WE~EW & (minBase ~> minor)~WS   // OxD
          Rules += main~WE~EW & minor~EN | (base ~> main)~WE~EW & (minBase ~> minor)~SW
          Rules += main~SE~ES & minor~WE | (base ~> main)~WN~NW & (minBase ~> minor)~WE   // DxO
          Rules += main~SE~ES & minor~EW | (base ~> main)~WN~NW & (minBase ~> minor)~EW
          Rules += main~SE~ES & minor~NE | (base ~> main)~WN~NW & (minBase ~> minor)~WS   // DxD
          Rules += main~SE~ES & minor~EN | (base ~> main)~WN~NW & (minBase ~> minor)~SW
          if (intersectionAllowed(base, minor) && intersectionAllowed(main, minBase)) {
            Rules += main~WE~EW & (minBase ~> minor)~NE | (base ~> main)~WE~EW & minor~WS   // OxD
            Rules += main~WE~EW & (minBase ~> minor)~EN | (base ~> main)~WE~EW & minor~SW
            Rules += main~SE~ES & (minBase ~> minor)~WE | (base ~> main)~WN~NW & minor~WE   // DxO
            Rules += main~SE~ES & (minBase ~> minor)~EW | (base ~> main)~WN~NW & minor~EW
            Rules += main~SE~ES & (minBase ~> minor)~NE | (base ~> main)~WN~NW & minor~WS   // DxD
            Rules += main~SE~ES & (minBase ~> minor)~EN | (base ~> main)~WN~NW & minor~SW
          }
        }
      }
      // inside multi-tile intersection
      createAdjacentIntersections(main, base, minor)
      createRules()
    }
  }
}
