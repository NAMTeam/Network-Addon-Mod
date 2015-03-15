package metarules.module

import metarules.meta._
import Network._, Flags._, Flag._, RotFlip._, Implicits._
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

  private def rhwIntersectionAllowed(rhw: Network, any: Network): Boolean = {
    if (!rhw.isRhw) {
      assert(any.isRhw)
      rhwIntersectionAllowed(any, rhw)
    } else {
      if (any.rhwPieceId.isEmpty) false
      else if (rhw.height != any.height) true
      else if (rhw.height != 0) false
      else if (any > rhw && any.isRhw) rhwIntersectionAllowed(any, rhw)
      else {
        rhw == Dirtroad && any == Dirtroad ||
        rhw == Rhw3 && any == Dirtroad ||
        rhw == Mis && (any == Dirtroad || any == Rhw3) ||
        rhw == Rhw4 && any == Dirtroad ||
        rhw <= Rhw4 && any < Dirtroad
      }
    }
  }

  def intersectionAllowed(a: Network, b: Network): Boolean = {
    if (a.isRhw || b.isRhw) {
      rhwIntersectionAllowed(a, b)
    } else {
//      assert(a.isNwm || b.isNwm) // not correct, as bases such as Road or OWR are not detected as NWM networks
      if (a == Groundhighway || b == Groundhighway)
        a.height != b.height
      else
        true // TODO
    }
  }

//  private def isDoubleSymmetrical(n: Network) = n.typ == AvenueLike || (n >= Tla5 && n <= Rd6)
//  private def isSNetwork(n: Network) = n >= Rhw8s && n <= L2Rhw12s
//  private def isCNetwork(n: Network) = n >= Rhw6c && n <= L2Rhw10c || n == Ave6 || n == Ave8

  def createMultiTileStarters(): Unit = {
    val g = 0 // ground level
    val heights = 0 to 2
    for (h <- heights) {
      // C networks
      val singles = Iterable(Rhw4, Mis, Rhw6s)
      val multis = Iterable(Rhw6c, Rhw8c, Rhw10c)
      for ((s, m) <- singles zip multis) {
        Rules += h~Rhw6cm~SN | g~s~SN | % | h~m~SN
        // stability
        Rules += h~m~WE | g~s~WE | % | h~m~WE
      }
      // S networks
      val sMultis = Iterable(Rhw8s, Rhw10s, Rhw12s)
      for (m <- sMultis) {
        Rules += h~Rhw4~SN | g~m~SN | h~Rhw8sm~SN | h~m~SN
        Rules += h~Rhw4~SN | h~m~SN | h~Rhw8sm~SN | %
        if (h != g) Rules += h~Rhw8sm~SN | g~m~SN | % | h~m~SN
        // stability
        Rules += h~Rhw8sm~WE | h~Rhw4~WE | % | h~Rhw8sm~WE
        if (h != g) {
          Rules += h~Rhw8s~WE | g~m~WE | % | h~m~WE   // TODO this does not look right, needs correction
        }
      }
      createRules()
    }
  }

  def start(): Unit = {
    createMultiTileStarters()

    for (main <- OverrideNetworks; base <- main.base) {
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
      for (minor <- Network.values if minor != Subway && (main.isRhw || minor.isRhw ||
           (main.isNwm && (minor.isRhw || minor.isNwm || minor.base.isEmpty))
           ) && intersectionAllowed(main, minor)) {
        // entry
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
            // TODO what if minor has shared diagonal
          }
          if (hasRightShoulder(minor)) entryCode(identity)
          if (hasLeftShoulder(minor)) entryCode(_.reverse)
          createRules() // duplicate rules will be removed
        }
        // exit
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
            // TODO what if minor has shared diagonal
          }
          if (hasRightShoulder(minor)) exitCode(identity)
          if (hasLeftShoulder(minor)) exitCode(_.reverse)
          createRules()
        }
        // diagonal inner intersection
        {
          if (intersectionAllowed(base, minor)) {
            if (minor.typ != AvenueLike) { // shared-tile diagonals work different, see below
              Rules += main~WE~EW & minor~ES | (base ~> main)~WE~EW & minor~NW   // OxD
              Rules += main~WE~EW & minor~SE | (base ~> main)~WE~EW & minor~WN
            }
            if (main.typ != AvenueLike) {
              Rules += main~SE~ES & minor~WE | (base ~> main)~WN~NW & minor~WE   // DxO
              Rules += main~SE~ES & minor~EW | (base ~> main)~WN~NW & minor~EW
              if (minor.typ != AvenueLike) {
                Rules += main~SE~ES & minor~NE | (base ~> main)~WN~NW & minor~WS   // DxD
                Rules += main~SE~ES & minor~EN | (base ~> main)~WN~NW & minor~SW
              } // else TODO
            } else {
              // TODO check if DxO is needed
              if (minor.typ != AvenueLike) {
                Rules += main~SharedDiagRight~ES & minor~NE | (base ~> main)~WN~SharedDiagRight & minor~WS   // DxD
                Rules += main~SharedDiagRight~ES & minor~EN | (base ~> main)~WN~SharedDiagRight & minor~SW
              }
            }
            createRules()
          }
          if (main.typ != AvenueLike && minor.typ != AvenueLike) for (minBase <- minor.base) {
            Rules += main~WE~EW & minor~ES | (base ~> main)~WE~EW & (minBase ~> minor)~NW   // OxD and DxO
            Rules += main~WE~EW & minor~SE | (base ~> main)~WE~EW & (minBase ~> minor)~WN
            Rules += main~SE~ES & minor~NE | (base ~> main)~WN~NW & (minBase ~> minor)~WS   // DxD
            Rules += main~SE~ES & minor~EN | (base ~> main)~WN~NW & (minBase ~> minor)~SW
            if (intersectionAllowed(base, minor) && intersectionAllowed(main, minBase)) {
              Rules += main~WE~EW & (minBase ~> minor)~ES | (base ~> main)~WE~EW & minor~NW   // OxD and DxO
              Rules += main~WE~EW & (minBase ~> minor)~SE | (base ~> main)~WE~EW & minor~WN
              Rules += main~SE~ES & (minBase ~> minor)~NE | (base ~> main)~WN~NW & minor~WS   // DxD
              Rules += main~SE~ES & (minBase ~> minor)~EN | (base ~> main)~WN~NW & minor~SW
            }
          }
        }
        // inside multi-tile intersection and adjacent intersections
        createAdjacentIntersections(main, base, minor)
      }
    }
  }
}
