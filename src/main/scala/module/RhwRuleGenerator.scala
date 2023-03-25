package metarules.module

import metarules.meta._
import Network._, Flags._, Flag._, RotFlip._, Implicits._, Group.SymGroup._
import scala.collection.mutable.Buffer
import NetworkProperties._


object RhwRuleGenerator {

  implicit class HeightLevel(val level: Int) extends AnyVal {
    def ~ (n: Network): Network = {
      require(n.height == 0)
      val m = Network(n.id + (level - n.height))
      assert(m.height == level)
      m
    }
  }
}

class RhwRuleGenerator(val resolver: IdResolver) extends RuleGenerator with Curve45Generator with Adjacencies {
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

  def createOnslopeTransition(): Unit = {
    val rhw2SlopeL1 = IdTile(0x57700000,1,0, (Dirtroad~EC).symmetries)  // direction East (upper) to West (lower)
    val rhw2SlopeL2 = IdTile(0x57700100,1,0, (Dirtroad~EC).symmetries)  // direction East (upper) to West (lower)

    for (main <- RhwNetworks - Dirtroad - Rhw10c if main.height == 0) {
      val maxHeight = if ((Mis + Rhw4 + Rhw6s).contains(main)) 4 else 2
      val rangeId = (main.rhwRangeId.get & 0xFFFFF) + ((main.rhwRangeId.get >>> 4) & 0xF000)  // e.g. 0x88080 for Rhw6cm
      for {
        (levelDiff, rhw2Slope) <- Seq((1, rhw2SlopeL1), (2, rhw2SlopeL2))  // L1 vs L2 onslopes
        height <- 0 to (maxHeight-levelDiff)
      } /*do*/ {
        val onslope = IdTile(0x57700000 + rangeId + 0x100*(levelDiff-1) + 0x10*height, R1F0, (main~EC).symmetries)  // direction East (upper) to West (lower)
        val lower = height~main
        val upper = (height+levelDiff)~main
        Rules += lower~EW | rhw2Slope | % | onslope   // lower > OST
        Rules += onslope | (Dirtroad ~> upper)~EW     // OST > upper
        Rules += rhw2Slope | upper~EW | onslope | %   // OST < upper
        Rules += (Dirtroad ~> lower)~EW | onslope     // lower < OST

        // adjacencies
        for (minor <- RhwNetworks ++ (BaseNetworks - Subway) ++ NwmNetworks) {  // crossing network
          if (intersectionAllowed(upper, minor)) {
            if (hasRightShoulder(minor)) {
              Rules += onslope | (Dirtroad ~> upper)~EW & minor~NS    // OST > upper crossing minor
              Rules += rhw2Slope | upper~EW & minor~NS | onslope | %  // OST < upper crossing minor
            }
            if (hasLeftShoulder(minor)) {
              Rules += onslope | (Dirtroad ~> upper)~EW & minor~SN    // OST > upper crossing minor
              Rules += rhw2Slope | upper~EW & minor~SN | onslope | %  // OST < upper crossing minor
            }
          }
          if (intersectionAllowed(lower, minor)) {
            if (hasRightShoulder(minor)) {
              Rules += (Dirtroad ~> lower)~EW & minor~SN | onslope    // lower crossing minor < OST
              Rules += lower~EW & minor~SN | rhw2Slope | % | onslope  // lower crossing minor > OST
            }
            if (hasLeftShoulder(minor)) {
              Rules += (Dirtroad ~> lower)~EW & minor~NS | onslope    // lower crossing minor < OST
              Rules += lower~EW & minor~NS | rhw2Slope | % | onslope  // lower crossing minor > OST
            }
          }
        }

        // OST adjacent to 45 degree curves
        if (isSingleTile(main)) {  // curves adjacent to OSTs only seem useful for tight setups with single-tilers
          if (hasSharpCurveBase(upper, inside=false) && hasSharpCurve(upper, inside=false)) {
            Rules += onslope | (Dirtroad ~> upper)~(+2,0,-13,0)    // OST > R0 upper
            Rules += rhw2Slope | upper~(+2,0,-13,0) | onslope | %  // OST < R0 upper
            Rules += lower~(+11,0,-2,0) | rhw2Slope | % | onslope  // R0 lower > OST
            Rules += (Dirtroad ~> lower)~(+11,0,-2,0) | onslope    // R0 lower < OST
          }
          if (hasSharpCurveBase(upper, inside=true) && hasSharpCurve(upper, inside=true)) {
            Rules += onslope | (Dirtroad ~> upper)~(+2,0,-11,0)    // OST > R0 upper
            Rules += rhw2Slope | upper~(+2,0,-11,0) | onslope | %  // OST < R0 upper
            Rules += lower~(+13,0,-2,0) | rhw2Slope | % | onslope  // R0 lower > OST
            Rules += (Dirtroad ~> lower)~(+13,0,-2,0) | onslope    // R0 lower < OST
          }
          if (hasR1CurveBase(upper) && hasR1Curve(upper, inside=false)) {
            Rules += onslope | (Dirtroad ~> upper)~(+2,0,-123,0)    // OST > R1 upper
            Rules += rhw2Slope | upper~(+2,0,-123,0) | onslope | %  // OST < R1 upper
            Rules += lower~(+121,0,-2,0) | rhw2Slope | % | onslope  // R1 lower > OST
            Rules += (Dirtroad ~> lower)~(+121,0,-2,0) | onslope    // R1 lower < OST
          }
          if (hasR1CurveBase(upper) && hasR1Curve(upper, inside=true)) {
            Rules += onslope | (Dirtroad ~> upper)~(+2,0,-121,0)    // OST > R1 upper
            Rules += rhw2Slope | upper~(+2,0,-121,0) | onslope | %  // OST < R1 upper
            Rules += lower~(+123,0,-2,0) | rhw2Slope | % | onslope  // R1 lower > OST
            Rules += (Dirtroad ~> lower)~(+123,0,-2,0) | onslope    // R1 lower < OST
          }
        }

        // OST adjacent to OST
        if (upper.height <= maxHeight - 1) {
          // +L1
          val onslopeUpperL1 = IdTile((onslope.id & 0xFFFFF0FF) + 0x10 * levelDiff, R1F0, onslope.symmetries)
          Rules += onslope | rhw2SlopeL1 | % | onslopeUpperL1  // lower > upper
          Rules += rhw2Slope | onslopeUpperL1 | onslope | %    // lower < upper
        }
        if (upper.height <= maxHeight - 2) {
          // +L2
          val onslopeUpperL2 = IdTile((onslope.id & 0xFFFFF0FF) + 0x100 + 0x10 * levelDiff, R1F0, onslope.symmetries)
          Rules += onslope | rhw2SlopeL2 | % | onslopeUpperL2  // lower > upper
          Rules += rhw2Slope | onslopeUpperL2 | onslope | %    // lower < upper
        }

      }
    }

    createRules()
  }

  def start(): Unit = {
    createMultiTileStarters()
    createOnslopeTransition()

    for (main <- OverrideNetworks; base <- main.base; if main.isRhw || main.isNwm) {  // TODO filtering
      if (main.isRhw || main.isNwm) {
        Rules += main~WE    | (base ~> main)~WE      // ortho
        Rules += main~WE    | (base ~> main)~WC      // ortho stub
        if (main.typ != AvenueLike) {
          Rules += main~SE~ES | (base ~> main)~WN~NW   // diagonal
        } else {
          Rules += main~SharedDiagRight~ES | (base ~> main)~WN~SharedDiagRight // shared diagonal
        }
        createRules() // flush the buffer from time to time
      }
      // 45 degree curves
      createCurve45Rules(main)

      // TODO filtering
      for (minor <- Network.values if minor != Subway && !isHrw(minor) && (main.isRhw || minor.isRhw ||
           (main.isNwm && (minor.isRhw || minor.isNwm || minor.base.isEmpty))
           ) && intersectionAllowed(main, minor)) {
        // entry (override from straight tile to first crossing tile)
        if (intersectionAllowed(base, minor)) { // skips e.g. preexisting L0Rhw2 x L0Rhw6c in second tile
          def entryCode(orient: Segment => Segment) = {
            Rules += main~WE    | (base ~> main)~WE & orient(minor~NS)      // OxO
            Rules += main~WE~EW | (base ~> main)~WE~EW & orient(minor~ES)   // OxD
            if (main.typ != AvenueLike) {
              Rules += main~SE~ES | (base ~> main)~WN~NW & orient(minor~NS)   // DxO
              Rules += main~SE~ES | (base ~> main)~WN~NW & orient(minor~NE)   // DxD
            } else {
              Rules += main~SharedDiagRight~ES | (base ~> main)~WN~SharedDiagRight & orient(minor~NS)   // DxO
              Rules += main~SharedDiagRight~ES | (base ~> main)~WN~SharedDiagRight & orient(minor~NE)   // DxD
            }
            // Shared diagonals on minor are not relevant here since the shared diagonal is an inner tile (i.e. without an edge).
          }
          if (hasRightShoulder(minor)) entryCode(identity)
          if (hasLeftShoulder(minor)) entryCode(_.reverse)
          createRules() // duplicate rules will be removed
        }
        // exit (override from last crossing tile to straight tile)
        {
          def exitCode(orient: Segment => Segment) = {
            Rules += main~WE & orient(minor~SN)    | (base ~> main)~WE      // OxO
            Rules += main~WE~EW & orient(minor~WN) | (base ~> main)~WE~EW   // OxD
            if (main.typ != AvenueLike) {
              Rules += main~SE~ES & orient(minor~SN) | (base ~> main)~WN~NW   // DxO
              Rules += main~SE~ES & orient(minor~SW) | (base ~> main)~WN~NW   // DxD
            } else {
              Rules += main~SharedDiagRight~ES & orient(minor~SN) | (base ~> main)~WN~SharedDiagRight   // DxO
              Rules += main~SharedDiagRight~ES & orient(minor~SW) | (base ~> main)~WN~SharedDiagRight   // DxD
            }
            // Shared diagonals on minor are not relevant here since the shared diagonal is an inner tile (i.e. without an edge).
          }
          if (hasRightShoulder(minor)) exitCode(identity)
          if (hasLeftShoulder(minor)) exitCode(_.reverse)
          createRules()
        }
        // Inside diagonal crossings (Diagonal crossings consist of two or more tiles, so the following rules ensure
        // that the override carries over between those inner-intersection tiles)
        {
          // The following automatically places shared diagonals instead of diagonals in the appropriate spots.
          val (se1, nw1) = if (main.typ  != AvenueLike) (SE, NW) else (SharedDiagRight, SharedDiagRight)
          val (ws2, en2) = if (minor.typ != AvenueLike) (WS, EN) else (SharedDiagLeft, SharedDiagLeft)
          if (intersectionAllowed(base, minor)) {
            Rules += main~WE~EW  & minor~NE  | (base ~> main)~WE~EW  & minor~ws2   // OxD
            Rules += main~WE~EW  & minor~en2 | (base ~> main)~WE~EW  & minor~SW
            Rules += main~se1~ES & minor~WE  | (base ~> main)~WN~nw1 & minor~WE    // DxO
            Rules += main~se1~ES & minor~EW  | (base ~> main)~WN~nw1 & minor~EW
            Rules += main~se1~ES & minor~NE  | (base ~> main)~WN~nw1 & minor~ws2   // DxD
            Rules += main~se1~ES & minor~en2 | (base ~> main)~WN~nw1 & minor~SW
            createRules()
          }
          // stability
          if (minor >= main) for (minBase <- minor.base) {  // If minor < main, then the following rules have already been added as part of the overrides of minor.
            Rules += main~WE~EW  & minor~NE  | (base ~> main)~WE~EW  & (minBase ~> minor)~ws2   // OxD
            Rules += main~WE~EW  & minor~en2 | (base ~> main)~WE~EW  & (minBase ~> minor)~SW
            Rules += main~se1~ES & minor~WE  | (base ~> main)~WN~nw1 & (minBase ~> minor)~WE    // DxO
            Rules += main~se1~ES & minor~EW  | (base ~> main)~WN~nw1 & (minBase ~> minor)~EW
            Rules += main~se1~ES & minor~NE  | (base ~> main)~WN~nw1 & (minBase ~> minor)~ws2   // DxD
            Rules += main~se1~ES & minor~en2 | (base ~> main)~WN~nw1 & (minBase ~> minor)~SW
            if (intersectionAllowed(base, minor) && intersectionAllowed(main, minBase)) {
              Rules += main~WE~EW  & (minBase ~> minor)~NE  | (base ~> main)~WE~EW  & minor~ws2   // OxD
              Rules += main~WE~EW  & (minBase ~> minor)~en2 | (base ~> main)~WE~EW  & minor~SW
              Rules += main~se1~ES & (minBase ~> minor)~WE  | (base ~> main)~WN~nw1 & minor~WE    // DxO
              Rules += main~se1~ES & (minBase ~> minor)~EW  | (base ~> main)~WN~nw1 & minor~EW
              Rules += main~se1~ES & (minBase ~> minor)~NE  | (base ~> main)~WN~nw1 & minor~ws2   // DxD
              Rules += main~se1~ES & (minBase ~> minor)~en2 | (base ~> main)~WN~nw1 & minor~SW
            }
          }
        }
        // inside multi-tile intersection and adjacent intersections
        createAdjacentIntersections(main, base, minor)
        createRules()
      }
    }
  }
}
