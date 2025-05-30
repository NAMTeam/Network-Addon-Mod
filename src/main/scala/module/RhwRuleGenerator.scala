package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._
import Network._, Flags._, Flag._, RotFlip._, Implicits._, group.SymGroup._
import NetworkProperties._


object RhwRuleGenerator {

  implicit class HeightLevel(val level: Int) extends AnyVal {
    def ~ (n: Network): Network = if (n == Dirtroad) {
      if (level > 0 && level <= 2) {
        Network(L1Rhw2.id + level - 1)
      } else {
        require(level == 0)
        Dirtroad
      }
    } else if (level == 0 && (n == L1Rhw2 || n == L2Rhw2)) {
      Dirtroad
    } else {
      require(n.height == 0)
      val m = Network(n.id + (level - n.height))
      assert(m.height == level)
      m
    }
  }

  def crossingNetworksOf(main: Network): Iterator[Network] = {
    for {
      minor <- Network.values.iterator
      if minor != Subway && !isHrw(minor) && (main.isRhw || minor.isRhw ||
         (main.isNwm && (minor.isRhw || minor.isNwm || minor.base.isEmpty)) ||
         (main.isNwm && isSingleTile(main) && main.height == 0 && (minor == L1Dtr || minor == L2Dtr))
         ) && intersectionAllowed(main, minor)
    } yield minor
  }
}

class RhwRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Curve45Generator with Adjacencies {
  import RhwRuleGenerator._

  def createMultiTileStarters(): Unit = {
    val g = 0 // ground level
    val heights = 0 to 2
    for (h <- heights if h != g) {
      // C networks
      val cMultis = Iterable(Rhw6c, Rhw8c, Rhw10c)
      for (m <- cMultis) {
        Rules += h~Rhw6cm~SN | g~m~SN | % | h~m~SN
        Rules += h~m~WE~EW | g~m~WE~EW | % | h~m~WE~EW  // stability against starter-induced auto-L0 issues
        Rules += h~m~WE~EW | g~m~WC~CW | % | h~m~WC~CW  // stability
      }
      // S networks
      val sMultis = Iterable(Rhw8s, Rhw10s, Rhw12s)
      for (m <- sMultis) {
        Rules += h~Rhw8sm~SN | g~m~SN | % | h~m~SN
        Rules += h~m~WE~EW | g~m~WE~EW | % | h~m~WE~EW  // stability against starter-induced auto-L0 issues
        Rules += h~m~WE~EW | g~m~WC~CW | % | h~m~WC~CW  // stability
      }
      createRules()
    }
  }

  def start(): Unit = {
    createMultiTileStarters()

    for (main <- OverrideNetworks; base <- main.base; if main.isRhw || main.isNwm) {  // TODO filtering
      if (main.isRhw || main.isNwm) {
        Rules += main~WE    | (base ~> main)~WE      // ortho
        Rules += main~WE    | (base ~> main)~WC      // ortho stub
        withSharedDiagonals { Rules =>
          Rules += main~SE~ES | (base ~> main)~WN~NW   // diagonal
        }
        createRules() // flush the buffer from time to time
      }
      // curves
      createCurve45Rules(main)
      createCurve90Rules(main)

      // TODO filtering
      for (minor <- crossingNetworksOf(main)) {
        // entry (override from straight tile to first crossing tile)
        if (intersectionAllowed(base, minor)) { // skips e.g. preexisting L0Rhw2 x L0Rhw6c in second tile
          def entryCode(orient: Segment => Segment) = withSharedDiagonals { Rules =>
            Rules += main~WE    | (base ~> main)~WE & orient(minor~NS)      // OxO
            Rules += main~WE~EW | (base ~> main)~WE~EW & orient(minor~ES)   // OxD
            Rules += main~SE~ES | (base ~> main)~WN~NW & orient(minor~NS)   // DxO
            Rules += main~SE~ES | (base ~> main)~WN~NW & orient(minor~NE)   // DxD
            // Shared diagonals on minor are not relevant here since the shared diagonal is an inner tile (i.e. without an edge).
          }
          if (hasRightShoulder(minor)) entryCode(identity)
          if (hasLeftShoulder(minor)) entryCode(_.reverse)
          createRules() // duplicate rules will be removed
        }
        // exit (override from last crossing tile to straight tile)
        {
          def exitCode(orient: Segment => Segment) = withSharedDiagonals { Rules =>
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
        withSharedDiagonals { Rules =>
          if (intersectionAllowed(base, minor)) {
            Rules += main~WE~EW & minor~NE | (base ~> main)~WE~EW & minor~WS   // OxD
            Rules += main~WE~EW & minor~EN | (base ~> main)~WE~EW & minor~SW
            Rules += main~SE~ES & minor~WE | (base ~> main)~WN~NW & minor~WE   // DxO
            Rules += main~SE~ES & minor~EW | (base ~> main)~WN~NW & minor~EW
            Rules += main~SE~ES & minor~NE | (base ~> main)~WN~NW & minor~WS   // DxD
            Rules += main~SE~ES & minor~EN | (base ~> main)~WN~NW & minor~SW
            createRules()
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
}
