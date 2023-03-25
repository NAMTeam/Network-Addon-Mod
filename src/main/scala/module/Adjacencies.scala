package metarules.module

import metarules.meta._, Network._, Flags._, Implicits._
import scala.collection.mutable
import NetworkProperties._

/** This file covers adjacency situations involving three networks:
  * - the main line network running in west-east direction,
  * - the first crossing network running in north-south direction,
  * - the second crossing network running in north-south direction (adjacent to the other crossing network).
  */
object Adjacencies {

  val NSNS = 0  // Overall directions of the two adjacent crossing networks, i.e. both run from north to south.
  val NSSN = 1  // Left crossing network from north to south, right crossing network from south to north, etc.
  val SNNS = 2
  val SNSN = 3

  /** Covers the multi-tile "inner" adjacencies (such as Rhw6cm adjacent to Rhw6c). */
  val multitileNetworks: Map[Network, Seq[(Network, Int)]] = {
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
    m(Owr5) = Seq(Owr5 -> NSSN)
    m(Rd4) = Seq(Rd4 -> NSSN)
    m(Owr4) = Seq(Owr4 -> NSSN)
    // Base multi-tile networks
    m(Avenue) = Seq(Avenue -> NSSN)
    m(Highway) = Seq(Highway -> NSSN)
    m(Groundhighway) = Seq(Groundhighway -> NSSN)
    // viaducts
    m(L1Avenue) = Seq(L1Avenue -> NSSN)
    m(L2Avenue) = Seq(L2Avenue -> NSSN)
    m.toMap
  }

  /** used as a cache */
  private[this] val adjacentNetworksMap = mutable.Map.empty[Network, Seq[(Network, Int)]]

  private[this] def isRhw3(n: Network) = n == Rhw3 || n == L1Rhw3 || n == L2Rhw3

  /** Lists the networks that are supported adjacent to `n` including their
    * directions (NSNS, NSSN, SNSN).
    * TODO make sure that nothing is included twice, unnecessarily.
    */
  def adjacentNetworks(n: Network): TraversableOnce[(Network, Int)] = adjacentNetworksMap.getOrElseUpdate(n, {
    val multAdjs = multitileNetworks.getOrElse(n, Seq.empty[(Network, Int)])
    if (n.isRhw && !isRhw3(n)) {
      // Supported adjacencies between RHW networks. Add any lacking adjacency support here.
      val one = if (hasLeftShoulder(n)) {
        (RhwNetworks.filter { m =>
          !isRhw3(m) && hasRightShoulder(m) && (isSingleTile(n) || isSingleTile(m) || n.height == m.height)
        } ++
        (if (n.height <= 2) Seq(Lightrail) else Seq.empty)  // Lightrail between two RHW networks
        ).map(m => m -> NSNS)
      } else Seq.empty
      val two = if (hasRightShoulder(n)) {
        (RhwNetworks.filter { m =>
          !isRhw3(m) && hasLeftShoulder(m) && (isSingleTile(n) || isSingleTile(m) || n.height == m.height)
        } ++
        (if (n.height <= 2) Seq(Lightrail, Rail, /*Str,*/ Glr2) else Seq.empty)  // rail-type networks parallel to RHW (on the outside)
        ).map(m => m -> SNSN)
      } else Seq.empty
      val three = if (hasLeftShoulder(n) && n.typ != Symmetrical) {
        RhwNetworks filter { m =>
          !isRhw3(m) && hasLeftShoulder(m) && m.typ != Symmetrical && (isSingleTile(n) || isSingleTile(m) || n == m)
        } map (m => m -> NSSN)
      } else Seq.empty
      multAdjs ++ one ++ two ++ three
    } else if (n.isNwm || n == Road || n == Onewayroad || n == Avenue) {
      // Supported adjacencies between NWM or road-type networks.
      val parallelOwrs = if (n == Owr3) Seq(n -> NSSN)
        else if (n == Owr1) Seq(n -> NSSN, n -> SNNS)  // since Owr1 is asymmetrical, both directions are needed
        else if (n == Owr4 || n == Owr5) Seq(n -> SNNS)
        else Seq.empty
      val railTypes = Seq(Rail, /*Str,*/ Lightrail /*, Glr1, Glr2, Glr3, Glr4*/)
      var adjacentRail = if (hasRightShoulder(n)) railTypes.map(m => m -> SNNS)
        else if (hasLeftShoulder(n)) railTypes.map(m => m -> NSSN)
        else Seq.empty
      multAdjs ++ parallelOwrs ++ adjacentRail
    } else {
      // TODO All other adjacencies (e.g. viaducts).
      multAdjs
    }
  })

}

trait Adjacencies { this: RuleGenerator =>
  import Adjacencies._

  def diagonalCrossingsRequireHalfdragging(base: Network, minor1: Network, minor2: Network): Boolean = {
    val base1 = minor1.base.getOrElse(minor1)
    val base2 = minor2.base.getOrElse(minor2)
    if (base == Road || base == Onewayroad || base == Dirtroad || base == Street) {
      // Diagonal crossings of these networks have INRUL footprints that are
      // larger than just the crossing itself, but include the diagonal approach
      // tiles. Therefore adjacent crossings are only possible using
      // half-dragging techniques, unless either all three base networks are the
      // same or both crossing networks differ from the main base network.
      (base1 != base2) && ((base == base1) || (base == base2))
    } else false
  }

  /** Covers all cases of parallel adjacent +/X-intersections involving three networks, i.e.
    * - OxO | OxO,
    * - OxD | OxD,
    * - DxO | DxO,
    * - DxD | DxD.
    */
  def createAdjacentIntersections(main: Network, base: Network, minor: Network): Unit = {  // minor is the first (left) crossing network
    assert(intersectionAllowed(main, minor))
    val (se, nw) = if (main.typ != AvenueLike) (SE, NW) else (SharedDiagRight, SharedDiagRight) // that way, code below works whether main is avelike or not
    // TODO case of avelike main needs to be tested, e.g. RD4

    val seen = collection.mutable.Set.empty[(Network, Int)]

    for ((adjacent, dirs) <- adjacentNetworks(minor)) {
      val (ns1, nw1, ws1) = if (dirs == NSNS || dirs == NSSN) (NS, NW, WS) else (SN, WN, SW)
      val (ns2, es2, ne2) = if (dirs == NSNS || dirs == SNNS) (NS, ES, NE) else (SN, SE, EN)

      def addRules(adj: Network) = if (!seen((adj, dirs))) {
        seen.add((adj, dirs))  // in particular, in order to avoid adding adjBase multiple times
        if (intersectionAllowed(base, adj) && intersectionAllowed(main, adj)) {
          Rules += main~WE & minor~ns1    | (base ~> main)~WE & adj~ns2      // OxO
          if (!diagonalCrossingsRequireHalfdragging(base, minor, adj)) {
            Rules += main~se~ES & minor~ns1 | (base ~> main)~WN~nw & adj~ns2   // DxO
            // for avelike networks, this puts shoulder between the adjacent networks, so these rules do not use shared diagonals
            if ((minor.typ != AvenueLike || nw1 == WN && ws1 == SW) && (adj.typ != AvenueLike || es2 == ES && ne2 == NE)) {
              Rules += main~WE~EW & minor~nw1 | (base ~> main)~WE~EW & adj~es2   // OxD
              Rules += main~se~ES & minor~ws1 | (base ~> main)~WN~nw & adj~ne2   // DxD
            }
          }
        }
      }
      addRules(adjacent)
      for (adjBase <- adjacent.base) {
        addRules(adjBase)
      }
    }
    createRules()
  }
}
