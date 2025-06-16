package com.sc4nam.module

import io.github.memo33.metarules.meta._
import syntax._, Network._, Flags._, RotFlip._, Implicits._
import NetworkProperties._

object CrossingGenerator {

  def crossingNetworksOf(main: Network): Iterator[Network] = {
    for {
      minor <- Network.values.iterator
      if minor != Subway && !isHrw(minor) && (main.isRhw || minor.isRhw ||
         (main.isNwm && (minor.isRhw || minor.isNwm || minor.isViaduct || minor.base.isEmpty)) ||
         (main.isNwm && isSingleTile(main) && main.height == 0 && (minor == L1Dtr || minor == L2Dtr)) ||
         (main.isViaduct && (minor.isNwm || minor.isRhw || minor.base.isEmpty) && minor != Groundhighway)
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
            Rules += main~WE~EW | (base ~> main)~WC~CW & orient(minor~NS)   // OxO T End
            Rules += main~WE~EW | (base ~> main)~WE~EW & orient(minor~NEC)   // OxD T 1
            Rules += main~WE~EW | (base ~> main)~WE~EW & orient(minor~NWC)   // OxD T 2
            if (minor != Avenue) {
              Rules += main~WE~EW | (base ~> main)~WC~CW & orient(minor~SW)   // OxD T End 1
              Rules += main~WC~CW & minor~WS~SW | (base ~> main)~WC~CW & orient(minor~EN)   // OxD T End 2
              Rules += main~WE~EW & minor~WS~SW | base~WC~CW & minor~EN~NE | main~WC~CW & minor~WS~SW | main~WC~CW & minor~EN~NE   // OxD T End 2 Alt
            }
            if (minor == Avenue) { 
              Rules += main~WE~EW | (base ~> main)~WC~CW & orient(minor~ES)   // OxD T End-Short 1
              Rules += main~WC~CW & minor~ES | (base ~> main)~WC~CW & minor~SharedDiagRight   // OxD T End-Short 2
              Rules += main~WE~EW & minor~ES | (base ~> main)~WC~CW & minor~SharedDiagRight   // OxD T End-Short 2 Alt
            }
         }
          if (main.isNwm && !minor.isRhw && minor.isNwmDual) withResolvableRulesOnly {  // T intersections are not defined for all network combinations
            Rules += main~WE | (base ~> main)~WE & orient(minor~CS)   // OxO T 1
            Rules += main~WE | (base ~> main)~WE & orient(minor~NC)   // OxO T 2
            Rules += main~WE & minor~NC | (base ~> main)~WE & minor~CN   // OxO T Tile 1-2
            Rules += main~EW & minor~CS | (base ~> main)~EW & minor~SC   // OxO T Tile 2-1
            Rules += main~EW & minor~NS | (base ~> main)~CW & minor~SN // OxO T End 1/Long T
            Rules += main~WE & minor~NS | (base ~> main)~WC & minor~SN // OxO T End 2/Long T
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
          if (main.isNwm && !minor.isRhw) withResolvableRulesOnly {  // T intersections are not defined for all network combinations
            Rules += main~WE~EW & minor~CS | (base ~> main)~WE~EW 		// OxO T 1
            Rules += main~WE~EW & minor~NC | (base ~> main)~WE~EW   	// OxO T 2
            Rules += main~WE~EW & minor~NEC | (base ~> main)~WE~EW 		// OxD T 1
            Rules += main~WE~EW & minor~NWC | (base ~> main)~WE~EW   	// OxD T 2
          }
          // if (main.isNwm && !minor.isRhw && minor.isNwmDual) withResolvableRulesOnly {  // T intersections are not defined for all network combinations
            // Rules += main~WE | (base ~> main)~WE & orient(minor~CS)   // OxO T 1
            // Rules += main~WE | (base ~> main)~WE & orient(minor~NC)   // OxO T 2
            // Rules += main~WE & minor~NC | (base ~> main)~WE & minor~CN   // OxO T Tile 1-2
            // Rules += main~EW & minor~CS | (base ~> main)~EW & minor~SC   // OxO T Tile 2-1
          // }
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
