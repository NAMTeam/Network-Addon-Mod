package metarules.module

import metarules.meta._, Network._, Flags._, Implicits._
import scala.collection.mutable
import NetworkProperties._

object Adjacencies {

  val NSNS = 0
  val NSSN = 1
  val SNNS = 2
  val SNSN = 3

  /** Covers the multi-tile "inner" adjacencies */
  private val multitileNetworks: Map[Network, Seq[(Network, Int)]] = {
    val m = mutable.Map.empty[Network, Seq[(Network, Int)]]
    import RhwRuleGenerator.HeightLevel
    // Rhw multi-tile networks
    for (h <- 0 to 2) {
      val cMed = Seq((h~Rhw6cm, NSNS))
      val cShoulders = Seq(h~Rhw6c, h~Rhw8c, h~Rhw10c)
      for (shoulder <- cShoulders) {
        m(shoulder) = cMed
      }
      val sMed = Seq((h~Rhw8sm, NSNS))
      val sShoulders = Seq(h~Rhw8s, h~Rhw10s, h~Rhw12s)
      for (shoulder <- sShoulders) {
        m(shoulder) = sMed
      }
      m(h~Rhw6cm) = cShoulders map (n => n -> SNSN)
      m(h~Rhw8sm) = sShoulders map (n => n -> SNSN)
    }
    // Nwm multi-tile networks
    m(Ave6) = Seq(Ave6m -> NSNS, Tla7m -> NSNS)
    m(Ave8) = m(Ave6)
    m(Ave6m) = Seq(Ave6 -> SNSN, Ave8 -> SNSN)
    m(Tla7m) = m(Ave6m)
    m(Tla5) = Seq(Tla5 -> NSSN)
    m(Rd6) = Seq(Rd6 -> NSSN)
    m(Owr5) = Seq(Owr4 -> NSSN)
    // avelike networks are covered elsewhere because of the peculiar way shared diagonals work
    m.toMap
  }

  /** used as a cache */
  private[this] val adjacentNetworksMap = mutable.Map.empty[Network, Seq[(Network, Int)]]

  private[this] def isRhw3(n: Network) = n == Rhw3 || n == L1Rhw3 || n == L2Rhw3

  /** Lists the networks that are supported adjacent to `n` including their
    * directions (NSNS, NSSN, SNSN).
    * TODO make sure that nothing is included twice, unnecessarily.
    */
  /*private*/ def adjacentNetworks(n: Network): TraversableOnce[(Network, Int)] = adjacentNetworksMap.getOrElseUpdate(n, {
    val multAdjs = multitileNetworks.getOrElse(n, Seq.empty[(Network, Int)])
    if (n.isRhw && !isRhw3(n)) {
      val one = if (hasLeftShoulder(n)) {
        RhwNetworks filter { m =>
          !isRhw3(m) && hasRightShoulder(m) && (isSingleTile(n) || isSingleTile(m) || n.height == m.height)
        } map (m => m -> NSNS)
      } else Seq.empty
      val two = if (hasRightShoulder(n)) {
        RhwNetworks filter { m =>
          !isRhw3(m) && hasLeftShoulder(m) && (isSingleTile(n) || isSingleTile(m) || n.height == m.height)
        } map (m => m -> SNSN)
      } else Seq.empty
      val three = if (hasLeftShoulder(n) && n.typ != Symmetrical) {
        RhwNetworks filter { m =>
          !isRhw3(m) && hasLeftShoulder(m) && m.typ != Symmetrical && (isSingleTile(n) || isSingleTile(m) || n == m)
        } map (m => m -> NSSN)
      } else Seq.empty
      multAdjs ++ one ++ two ++ three
    } else {
      multAdjs
    }
  })

}

trait Adjacencies { this: RhwRuleGenerator =>
  import Adjacencies._

  /** Covers all cases of parallel adjacent +/X-intersections, i.e. OxO, OxD,
    * DxO, DxD, save for 'inner' diagonal intersections.
    */
  def createAdjacentIntersections(main: Network, base: Network, minor: Network): Unit = {
    assert(intersectionAllowed(main, minor))
    val (se, nw) = if (main.typ != AvenueLike) (SE, NW) else (SharedDiagRight, SharedDiagRight) // that way, code below works whether main is avelike or not
    // TODO case of avelike main needs to be tested, e.g. RD4

    for ((adjacent, dirs) <- adjacentNetworks(minor)) {
      val (ns1, nw1, ws1) = if (dirs == NSNS || dirs == NSSN) (NS, NW, WS) else (SN, WN, SW)
      val (ns2, es2, ne2) = if (dirs == NSNS || dirs == SNNS) (NS, ES, NE) else (SN, SE, EN)

      def addRules(adj: Network) = {
        if (intersectionAllowed(base, adj) && intersectionAllowed(main, adj)) {
          Rules += main~WE & minor~ns1    | (base ~> main)~WE & adj~ns2      // OxO
          Rules += main~se~ES & minor~ns1 | (base ~> main)~WN~nw & adj~ns2   // DxO
          if (minor.typ != AvenueLike || dirs == SNNS) {
            Rules += main~WE~EW & minor~nw1 | (base ~> main)~WE~EW & adj~es2   // OxD
            Rules += main~se~ES & minor~ws1 | (base ~> main)~WN~nw & adj~ne2   // DxD
          } else { assert((adj == minor || minor.base.isDefined && minor.base.get == adj) && dirs == NSSN, s"adj $adj minor $minor minbase ${minor.base} dirs $dirs")
            Rules += main~WE~EW & minor~ES              | (base ~> main)~WE~EW & adj~SharedDiagRight   // OxD
            Rules += main~WE~EW & minor~SharedDiagRight | (base ~> main)~WE~EW & adj~WN                // OxD
            Rules += main~se~ES & minor~NE              | (base ~> main)~WN~nw & adj~SharedDiagLeft    // DxD
            Rules += main~se~ES & minor~SharedDiagLeft  | (base ~> main)~WN~nw & adj~SW                // DxD
          }
        }
      }
      addRules(adjacent)
      for (adjBase <- adjacent.base) {
        addRules(adjBase) // TODO add base only once!
      }
    }
    createRules()
  }
}
