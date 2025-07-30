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
          if (main.isNwm) withResolvableRulesOnly {  // T intersections are not defined for all network combinations
            Rules += main~WE~EW | (base ~> main)~WE~EW & orient(minor~CS)   // OxO T 1
            Rules += main~WE~EW | (base ~> main)~WE~EW & orient(minor~NC)   // OxO T 2
            Rules += main~WE~EW | (base ~> main)~WC~CW & orient(minor~NS)   // OxO T End
            Rules += main~WE~EW | (base ~> main)~WE~EW & orient(minor~NEC)   // OxD T 1
            Rules += main~WE~EW | (base ~> main)~WE~EW & orient(minor~NWC)   // OxD T 2
            Rules += main~NE~EN | (base ~> main)~WS~SW & orient(minor~NC) // DxO T 1
            Rules += main~NE~EN | (base ~> main)~WS~SW & orient(minor~SC) // DxO T 2 
            Rules += main~ES~SE & minor~WC~CW | (base ~> main)~NW~WN & orient(minor~WC) // DxO T 1-2
            Rules += main~ES~SE & minor~CE~EC | (base ~> main)~NW~WN & orient(minor~CE) // DxO T 2-1
            Rules += main~ES~SE & minor~WE~EW | main~NW~WN | main~ES~SE & minor~WC~CW | main~NW~WN & orient(minor~WC) // DxO T 1-2 Stability
            Rules += main~ES~SE & minor~WE~EW | base~NW~WN | main~ES~SE & minor~WC~CW | main~NW~WN & orient(minor~WC) // DxO T 1-2 Stability
            Rules += main~ES~SE & minor~CE~EC | base~NW~WN & minor~WE~EW | main~ES~SE & minor~CE~EC | main~NW~WN & minor~CE~EC // DxO T 2-1 Alt
            Rules += main~NE~EN | (base ~> main)~WS~SW & orient(minor~ESC) // DxD T 1
            Rules += main~NE~EN | (base ~> main)~WS~SW & orient(minor~SEC) // DxD T 2 
            Rules += main~SE~ES & minor~NEC~CEN | (base ~> main)~WN~NW & orient(minor~WSC) // DxD T 1-2
            Rules += main~NE~EN & minor~ESC~CSE | (base ~> main)~WS~NS & orient(minor~CWN) // DxD T 2-1
            Rules += main~SE~ES & minor~NE~EN | base~WN~NW & minor~WSC~CSW | main~SE~ES & minor~NEC~CEN | main~WN~NW & minor~WSC~CSW // DxD T 2-1 Alt
            Rules += main~SE~ES & minor~NEC~CEN | base~WN~NW | main~SE~ES & minor~NEC~CEN | main~WN~NW & minor~WSC~CSW // DxD T 1-2 Alt
            Rules += main~SE~ES & minor~NEC~CEN | main~WN~NW | main~SE~ES & minor~NEC~CEN | main~WN~NW & minor~WSC~CSW // DxD T 1-2 Alt

            if (minor != Avenue) {
              Rules += main~WE~EW | (base ~> main)~WC~CW & orient(minor~SE)   // OxD T End 1
              Rules += main~WC~CW & minor~ES~SE | (base ~> main)~WC~CW & orient(minor~NW)   // OxD T End 2
              Rules += main~WE~EW & minor~ES~SE | base~WC~CW & minor~WN~NW | main~WC~CW & minor~ES~SE | main~WC~CW & minor~WN~NW   // OxD T End 2 Alt
              Rules += main~WE~EW & minor~ES~SE | minor~WN~NW | main~WC~CW & minor~ES~SE | main~WC~CW & minor~WN~NW   // OxD T End 3 Alt
              Rules += main~WC~CW & minor~ES~SE | minor~WN~NW | main~WC~CW & minor~ES~SE | main~WC~CW & minor~WN~NW   // OxD T End 4 Alt
              // Rules += main~EW~WE & minor~ES~SE | minor~WN~NW | main~CW~WC & minor~ES~SE | main~CW~WC & minor~WN~NW   // OxD T End 5 Alt
              // Rules += main~CW~WC & minor~ES~SE | minor~WN~NW | main~CW~WC & minor~ES~SE | main~CW~WC & minor~WN~NW   // OxD T End 6 Alt
              Rules += main~(0,0,11,3) | base~WNC~CNW & minor~NS~SN | main~(0,0,11,3) | main~WNC~CNW & minor~NS~SN   // DxO T End 1
              Rules += main~SW~WS | base~WNC~CNW & minor~NS~SN | main~(0,0,11,3) | main~WNC~CNW & minor~NS~SN   // DxO T End 1 Alt
              Rules += main~SE~ES | (base ~> main)~WNC~CNW & orient(minor~EN)   // DxD T End 1
              Rules += main~CEN~NEC & minor~SE~ES | (base ~> main)~CSW~WSC & orient(minor~WN)   // DxD T End 2
              Rules += main~EN~NE & minor~SE~ES | base~CSW~WSC & minor~WN~NW | main~CEN~NEC & minor~SE~ES | main~CSW~WSC & minor~WN~NW   // DxD T End 2 Alt
              Rules += main~EN~NE & minor~SE~ES | main~SW~WS | main~CEN~NEC & minor~SE~ES | main~CSW~WSC & minor~SW~WS   // DxD T End 2 Alt 2
              Rules += main~CEN~NEC & minor~SE~ES | base~SW~WS | main~CEN~NEC & minor~SE~ES | main~CSW~WSC & minor~SW~WS   // DxD T End 2 Alt 3
            }
            if (minor == Avenue) { 
              Rules += main~WE~EW | (base ~> main)~WE~EW & minor~(0,0,3,0)   // OxD T 1
              Rules += main~WE~EW | (base ~> main)~WE~EW & minor~(1,0,0,0)   // OxD T 3
              Rules += main~WE~EW & minor~(0,0,3,0) | (base ~> main)~WE~EW & minor~(0,0,3,1)   // OxD T 1-2
              Rules += main~WE~EW & minor~(1,0,0,0) | (base ~> main)~WE~EW & minor~(3,1,0,0)   // OxD T 3-2
              Rules += main~WE~EW & minor~(0,0,3,1) | (base ~> main)~WE~EW & minor~(0,0,1,0)   // OxD T 2-3
              Rules += main~WE~EW & minor~(3,1,0,0) | (base ~> main)~WE~EW & minor~(3,0,0,0)   // OxD T 2-1
              Rules += main~SE~ES | (base ~> main)~WN~NW & minor~CS   // DxO T-Thru 1
              Rules += main~SE~ES | (base ~> main)~WN~NW & minor~NC   // DxO T-Thru 4
              Rules += main~NE~EN & minor~CW | (base ~> main)~WS~SW & minor~CW   // DxO T-Thru 1-2
              Rules += main~NE~EN & minor~CE | (base ~> main)~WS~SW & minor~CE   // DxO T-Thru 2-1
              Rules += main~SE~ES & minor~CS | (base ~> main)~WN~NW & minor~SC   // DxO T-Thru 2-3
              Rules += main~SE~ES & minor~NC | (base ~> main)~WN~NW & minor~CN   // DxO T-Thru 3-2
              Rules += main~NE~EN & minor~WC | (base ~> main)~WS~SW & minor~WC   // DxO T-Thru 3-4
              Rules += main~NE~EN & minor~WC | base~WS~SW | main~NE~EN & minor~WC | main~WS~SW & minor~WC   // DxO T-Thru 3-4 Stability 1
              Rules += main~NE~EN & minor~WC | main~WS~SW | main~NE~EN & minor~WC | main~WS~SW & minor~WC   // DxO T-Thru 3-4 Stability 2
              Rules += main~NE~EN & minor~EC | (base ~> main)~WS~SW & minor~EC   // DxO T-Thru 4-3
              Rules += main~NE~EN & minor~EC | base~WS~SW | main~NE~EN & minor~EC | main~WS~SW & minor~EC   // DxO T-Thru 4-3 Stability 1
              Rules += main~NE~EN & minor~EC | main~WS~SW | main~NE~EN & minor~EC | main~WS~SW & minor~EC   // DxO T-Thru 4-3 Stability 2
              Rules += main~NE~EN | base~WS~SW & minor~EC | main~NE~EN & minor~EC | main~WS~SW & minor~EC   // DxO T-Thru 4-3 Stability 3
              Rules += main~NE~EN | base~WS~SW & minor~EC | main~NE~EN & minor~EC | main~WS~SW & minor~EC   // DxO T-Thru 4-3 Stability 4
              if (main != Owr3 && main != Tla3) { 
                //Tla3 and Owr3 handled in manual supplement for time being
                Rules += main~SE~ES | (base ~> main)~WN~NW & minor~CNE   // DxD T-Thru 1a1
                // Rules += main~NE~EN | (base ~> main)~WS~SW & minor~ESC   // DxD T-Thru 1a2
                Rules += main~NE~EN & minor~CES | (base ~> main)~WS~SW & minor~(1,0,0,3)   // DxD T-Thru 1a1-1b1
                // Rules += main~SE~ES & minor~NEC | (base ~> main)~WN~NW & minor~(3,1,0,0)   // DxD T-Thru 1a2-1b2
                Rules += main~NE~EN & minor~ES | base~WS~SW & minor~(1,0,0,3) | main~NE~EN & minor~CES | main~WS~SW & minor~(1,0,0,3)   // DxD T-Thru 1a-1b Stability
                // Rules += main~SE~ES & minor~(0,0,3,1) | (base ~> main)~WN~NW & minor~SWC   // DxD T-Thru 1b-1a
                Rules += main~SE~ES & minor~(0,0,3,1) | base~WN~NW & minor~SW | % | main~WN~NW & minor~SWC  // DxD T-Thru 1b-1a Stability 1
                Rules += main~NE~EN & minor~(0,3,1,0) | base~WS~SW & minor~WN | % | main~WS~SW & minor~CWN  // DxD T-Thru 1b-1a Stability 2
                Rules += Tla3~SE~ES & minor~(0,0,3,1) | Road~WN~NW & minor~SW | % | Tla3~WN & minor~SWC  // DxD T-Thru 1b-1a Stability 1
                Rules += Tla3~NE~EN & minor~(0,3,1,0) | Road~WS~SW & minor~WN | % | Tla3~SW & minor~CWN  // DxD T-Thru 1b-1a Stability 2
                Rules += main~SE~ES | (base ~> main)~WN~NW & minor~NEC   // DxD T-Thru 2a
                // Rules += main~NE~EN | (base ~> main)~WS~SW & minor~CES   // DxD T-Thru 2a Alt
                Rules += main~SE~ES & minor~(3,1,0,0) | (base ~> main)~WN~NW & minor~CSW   // DxD T-Thru 2b-2a
                Rules += main~NE~EN & minor~(1,0,0,3) | (base ~> main)~WS~SW & minor~WNC   // DxD T-Thru 2b-2a Alt
                Rules += main~SE~ES & minor~(3,1,0,0) | base~WN~NW | % | main~WN~NW & minor~CSW   // DxD T-Thru 2b-2a
                Rules += main~NE~EN & minor~(1,0,0,3) | base~WS~SW | % | main~WS~SW & minor~WNC   // DxD T-Thru 2b-2a Alt
                Rules += main~SE~ES & minor~(3,1,0,0) | main~WN~NW | % | main~WN~NW & minor~CSW   // DxD T-Thru 2b-2a
                Rules += main~NE~EN & minor~(1,0,0,3) | main~WS~SW | % | main~WS~SW & minor~WNC   // DxD T-Thru 2b-2a Alt

                Rules += main~SE~ES & minor~CNE | (base ~> main)~WN~NW & minor~(0,0,3,1)   // DxD T-Thru 2a-2b
                Rules += main~SE~ES & minor~NE | base~WN~NW & minor~(0,0,3,1) | main~SE~ES & minor~CNE | main~WN~NW & minor~(0,0,3,1)   // DxD T-Thru 2a-2b Stability
                Rules += main~NE~EN & minor~ESC | (base ~> main)~WS~SW & minor~(0,3,1,0)   // DxD T-Thru 2a-2b Alt
                // Rules += main~NE~EN & minor~ES | base~WS~SW & minor~(0,3,1,0) | main~NE~EN & minor~ESC | main~WS~SW & minor~(0,3,1,0)   // DxD T-Thru 2a-2b Alt Stability
              }
              Rules += main~WE~EW | (base ~> main)~WC~CW & orient(minor~ES)   // OxD T End-Short 1
              Rules += main~WC~CW & minor~ES | (base ~> main)~WC~CW & minor~SharedDiagRight   // OxD T End-Short 2
              Rules += main~WE~EW & minor~ES | (base ~> main)~WC~CW & minor~SharedDiagRight   // OxD T End-Short 2 Alt
              Rules += main~WE~EW & minor~SharedDiagRight | base~WC~CW & minor~WN | main~WC~CW & minor~SharedDiagRight | main~WC~CW & minor~WN   // OxD T End-Long
              Rules += main~(0,0,11,3) | base~WNC~CNW & minor~NS~SN | main~(0,0,11,3) | main~WNC~CNW & minor~NS~SN   // DxO T End-Short 1
              Rules += main~SW~WS | base~WNC~CNW & minor~NS~SN | main~(0,0,11,3) | main~WNC~CNW & minor~NS~SN   // DxO T End-Short 1 Alt
              Rules += main~SE~ES & minor~NS | (base ~> main)~WNC~CNW & minor~SN   // DxO T End-Long 1
              Rules += main~SEC~CES & minor~EW | base~WNC~CNW & minor~EW | main~SEC~CES & minor~EW | main~WNC~CNW & minor~EW   // DxO T End-Long 2
              Rules += main~SE~ES & minor~EW | base~WNC~CNW & minor~EW | main~SEC~CES & minor~EW | main~WNC~CNW & minor~EW   // DxO T End-Long 2 Alt
              Rules += main~SE~ES | (base ~> main)~WNC~CNW & orient(minor~EN)   // DxD T End-Short 1
              Rules += main~SE~ES & minor~SharedDiagLeft | base~WNC~CNW & minor~SW | main~SEC~CES & minor~SharedDiagLeft | main~WNC~CNW & minor~SW   // DxD T End-Long
            }
         }
          if (main.isNwm && minor.isNwmDual) withResolvableRulesOnly {  // T intersections are not defined for all network combinations
            Rules += main~WE | (base ~> main)~WE & orient(minor~CS)   // OxO T 1
            Rules += main~WE | (base ~> main)~WE & orient(minor~NC)   // OxO T 2
            Rules += main~WE & minor~NC | (base ~> main)~WE & minor~CN   // OxO T Tile 1-2
            Rules += main~EW & minor~CS | (base ~> main)~EW & minor~SC   // OxO T Tile 2-1
            Rules += main~EW & minor~NS | (base ~> main)~CW & minor~SN // OxO T End 1/Long T
            Rules += main~WE & minor~NS | (base ~> main)~WC & minor~SN // OxO T End 2/Long T
          }

          if (main.isNwm && minor.isNwmTriple) withResolvableRulesOnly {  // T intersections are not defined for all network combinations
            Rules += main~WE | (base ~> main)~WE & orient(minor~CS)   // OxO T 1
            Rules += main~WE | (base ~> main)~WE & orient(minor~NC)   // OxO T 2
            // Rules += main~WE & minor~NC | (base ~> main)~WE & minor~CN   // OxO T Tile 1-2
            // Rules += main~EW & minor~CS | (base ~> main)~EW & minor~SC   // OxO T Tile 2-1
            Rules += main~EW & Tla7m~NS | (base ~> main)~CW & minor~SN // OxO T End 1/Long T
            Rules += main~WE & Tla7m~NS | (base ~> main)~WC & minor~SN // OxO T End 2/Long T
            Rules += main~EW & Ave6m~NS | (base ~> main)~CW & minor~SN // OxO T End 1/Long T
            Rules += main~WE & Ave6m~NS | (base ~> main)~WC & minor~SN // OxO T End 2/Long T
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
          if (main.isNwm) withResolvableRulesOnly {  // T intersections are not defined for all network combinations
            Rules += main~WE~EW & minor~CS~SC | (base ~> main)~WE~EW 		// OxO T 1
            Rules += main~WE~EW & minor~NC~CN | (base ~> main)~WE~EW   	// OxO T 2
            Rules += main~WE~EW & minor~NEC~CEN | (base ~> main)~WE~EW 		// OxD T 1
            Rules += main~WE~EW & minor~NWC~CWN | (base ~> main)~WE~EW   	// OxD T 2
            Rules += main~WE~EW & Avenue~(3,0,0,0) | (base ~> main)~WE~EW 		// OxD T Ave 1
            Rules += main~WE~EW & Avenue~(1,0,0,0) | (base ~> main)~WE~EW        // OxD T Ave 2
            Rules += main~NE~EN & minor~CS~SC | (base ~> main)~WS~SW 		// DxO T 1
            Rules += main~NE~EN & minor~NC~CN | (base ~> main)~WS~SW 		// DxO T 2
            Rules += main~SE~ES & minor~WSC~CSW | (base ~> main)~WN~NW 		// DxD T 1
            Rules += main~SE~ES & minor~CWS~SWC | (base ~> main)~WN~NW 		// DxD T 2
            Rules += main~ES~SE & minor~WSC~CSW | (base ~> main)~NW~WN 		// DxD T 1b
            Rules += main~ES~SE & minor~CWS~SWC | (base ~> main)~NW~WN 		// DxD T 2b
          }
          // if (main.isNwm && minor.isNwmDual) withResolvableRulesOnly {  // T intersections are not defined for all network combinations
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
