package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, RotFlip._, Flags._
import Implicits.segmentToTile
import com.sc4nam.module.{NetworkProperties => NP}
import NwmResolver.nwmRangeId

object RhwResolver {

  val rhwRangeId = Map(
    Dirtroad -> 0x57000000, L1Rhw2   -> 0x57100000, L2Rhw2   -> 0x57200000,
    Rhw3     -> 0x57010000, L1Rhw3   -> 0x57110000, L2Rhw3   -> 0x57210000,
    Mis      -> 0x57020000, L1Mis    -> 0x57120000, L2Mis    -> 0x57220000, L3Mis    -> 0x57320000, L4Mis    -> 0x57420000,
    Rhw4     -> 0x57030000, L1Rhw4   -> 0x57130000, L2Rhw4   -> 0x57230000, L3Rhw4   -> 0x57330000, L4Rhw4   -> 0x57430000,
    Rhw6s    -> 0x57040000, L1Rhw6s  -> 0x57140000, L2Rhw6s  -> 0x57240000, L3Rhw6s  -> 0x57340000, L4Rhw6s  -> 0x57440000,
    Rhw8sm   -> 0x57050080, L1Rhw8sm -> 0x57150080, L2Rhw8sm -> 0x57250080,
    Rhw8s    -> 0x57050000, L1Rhw8s  -> 0x57150000, L2Rhw8s  -> 0x57250000,
    Rhw10s   -> 0x57060000, L1Rhw10s -> 0x57160000, L2Rhw10s -> 0x57260000,
    Rhw12s   -> 0x57070000, L1Rhw12s -> 0x57170000, L2Rhw12s -> 0x57270000,
    Rhw6cm   -> 0x57080080, L1Rhw6cm -> 0x57180080, L2Rhw6cm -> 0x57280080,
    Rhw6c    -> 0x57080000, L1Rhw6c  -> 0x57180000, L2Rhw6c  -> 0x57280000,
    Rhw8c    -> 0x57090000, L1Rhw8c  -> 0x57190000, L2Rhw8c  -> 0x57290000,
    Rhw10c   -> 0x570A0000, L1Rhw10c -> 0x571A0000, L2Rhw10c -> 0x572A0000)

  val rhwPieceId = Map(
    Street        -> 0x1000,
    Road          -> 0x1100, L1Road        -> 0x1110, L2Road        -> 0x1120,
    Onewayroad    -> 0x1200, L1Onewayroad  -> 0x1210, L2Onewayroad  -> 0x1220,
    Avenue        -> 0x1300, L1Avenue      -> 0x1310, L2Avenue      -> 0x1320,
    Groundhighway -> 0x1400,                          Highway       -> 0x1420,
    Rail          -> 0x1500, Str           -> 0x1505,
    //Ttr         -> 0x1600, //Qtr         -> 0x1605,
    Glr1          -> 0x1700, Glr3          -> 0x1705, Lightrail     -> 0x1720,
    Glr2          -> 0x1800, Glr4          -> 0x1805,
    Hsr           -> 0x1905, L2Hsr         -> 0x1925, Monorail      -> 0x1920,

    Dirtroad -> 0x1A00, L1Rhw2   -> 0x1A10, L2Rhw2   -> 0x1A20,
    Rhw3     -> 0x1B00, L1Rhw3   -> 0x1B10, L2Rhw3   -> 0x1B20,
    Mis      -> 0x1C00, L1Mis    -> 0x1C10, L2Mis    -> 0x1C20, L3Mis    -> 0x1C30, L4Mis    -> 0x1C40,
    Rhw4     -> 0x1D00, L1Rhw4   -> 0x1D10, L2Rhw4   -> 0x1D20, L3Rhw4   -> 0x1D30, L4Rhw4   -> 0x1D40,
    Rhw6s    -> 0x1E00, L1Rhw6s  -> 0x1E10, L2Rhw6s  -> 0x1E20, L3Rhw6s  -> 0x1E30, L4Rhw6s  -> 0x1E40,
    Rhw8sm   -> 0x1F00, L1Rhw8sm -> 0x1F10, L2Rhw8sm -> 0x1F20,
    Rhw8s    -> 0x2000, L1Rhw8s  -> 0x2010, L2Rhw8s  -> 0x2020,
    Rhw10s   -> 0x2100, L1Rhw10s -> 0x2110, L2Rhw10s -> 0x2120,
    Rhw12s   -> 0x2200, L1Rhw12s -> 0x2210, L2Rhw12s -> 0x2220,
    Rhw6cm   -> 0x2300, L1Rhw6cm -> 0x2310, L2Rhw6cm -> 0x2320,
    Rhw6c    -> 0x2400, L1Rhw6c  -> 0x2410, L2Rhw6c  -> 0x2420,
    Rhw8c    -> 0x2500, L1Rhw8c  -> 0x2510, L2Rhw8c  -> 0x2520,
    Rhw10c   -> 0x2600, L1Rhw10c -> 0x2610, L2Rhw10c -> 0x2620,

    Tla3     -> 0x2700, Ave2     -> 0x2800, Ard3     -> 0x2900,
    Owr1     -> 0x2A00, Owr3     -> 0x2B00, Nrd4     -> 0x2C00,
    Tla5     -> 0x2D00, Owr4     -> 0x2E00, Owr5     -> 0x2F00,
    Rd4      -> 0x3000, Rd6      -> 0x3100, Ave6     -> 0x3200,
    Tla7m    -> 0x3300, Ave8     -> 0x3400, Ave6m    -> 0x3500,
    // skipped some tram-dual networks
    Owr4m    -> 0x3B00,
  )

  def rhwHtRangeId(n: Network): Int = {  // for OST and HT
    require(n.height == 0)
    0x57700000 + (rhwRangeId(n) & 0xFFFFF) + ((rhwRangeId(n) >>> 4) & 0xF000)  // e.g. 0x57788080 for Rhw6cm
  }

  /** A network is greater, if it has higher priority, thus dominates the other
    * and determines the main ID range, like 0x5713#### for instance. The other
    * network determines the piece ID.
    */
  def greater(a: Network, b: Network): Boolean = {
    if (a.isRhw != b.isRhw) {
      a.isRhw
    } else if (Viaducts.contains(a) != Viaducts.contains(b)) {
      Viaducts.contains(a)
    } else if (a.isNwm != b.isNwm) {
      a.isNwm
    } else if (a.height != b.height) {
      a.height > b.height
    } else if (a != b) {
      a > b // both RHW or both Viaducts or both NWM with same height
    } else {
      assert(a == b)
      false
    }
  }

}

class RhwResolver extends IdResolver {
  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)
  def apply(tile: Tile): IdTile = tileMap(tile)

  val tileMap = {
    val builder = new ResolverBuilder(
      // To simplify adding shared diagonals, we automatically add them for avenue-like networks going in the wrong direction.
      remap = (tile: Tile) => NP.transformSharedDiagonals(tile),
    )
    import builder.add

    add(0x57000f00, Dirtroad~(0,0,0,0))
    for (n <- RhwNetworks) {
      val id = RhwResolver.rhwRangeId(n)
      add(id + 0x0000, n~NS)  // orth
      add(id + 0x0100, n~CS)  // orth stub

      if (NP.isSingleTile(n)) {
        add(id + 0x0200, n~ES)  // diag 1
        add(id + 0x0300, n~SWC)  // diag stub 1
        add(id + 0x0400, n~(0,-2,0,+11))  // 45 curve 1
        add(id + 0x0500, n~(0,0,-1,+13))  // 45 curve 1
        if (!n.isSymm) {
          add(id + 0x0900, n~SE)  // diag 2
          add(id + 0x0a00, n~CWS)  // diag stub 2
          add(id + 0x0b00, n~(0,+2,0,-11))  // 45 curve 2
          add(id + 0x0c00, n~(0,0,+1,-13))  // 45 curve 2
        }
      } else {  // multi-tile RHW networks
        add(id + (if (!NP.isRhwShoulder(n)) 0x0200 else 0x0300), n~ES) // diag 1
        add(id + (if (!NP.isRhwShoulder(n)) 0x0400 else 0x0500), n~SWC) // diag stub 1
        if (!n.isSymm) {
          add(id + (if (!NP.isRhwShoulder(n)) 0x0300 else 0x0200), n~SE) // diag 2
          // add(id + 0x0300, n~SharedDiagRight) // (avelike)
          add(id + (if (!NP.isRhwShoulder(n)) 0x0500 else 0x0400), n~CWS) // diag stub 2
        }
        // multi-tile RHW curve assembly
        val rev = NP.isRhwShoulderMedian(n)
        val orientM: IntFlags => IntFlags = if (rev) reverseIntFlags else identity
        if (NP.hasMiniCurve(n, inside = !rev))
          add(id + 0x0600, n~orientM(0,+2,0,-11))
        if (NP.hasMiniCurve(n, inside = rev))
          add(id + 0x0700, n~orientM(0,-2,0,+11))
        if (NP.hasExtendedCurve(n, inside = !rev)) {
          add(id + 0x0600, n~orientM(0,+111,0,-11))
          add(id + 0x0a00, n~orientM(0,+2,0,-111))
        }
        if (NP.hasExtendedCurve(n, inside = rev) && !n.isSymm) {
          add(id + 0x0700, n~orientM(0,-111,0,+11))
          add(id + 0x0b00, n~orientM(0,-2,0,+111))
        }
        if (NP.hasMiniCurve(n, inside = rev) || NP.hasExtendedCurve(n, inside = rev)) {
          add(id + 0x0800, n~orientM(0,0,+111,-13))
          add(id + 0x0c00, n~orientM(0,0,-111,+3))
        }
        if (NP.hasMiniCurve(n, inside = !rev) || NP.hasExtendedCurve(n, inside = !rev) && !n.isSymm) {
          add(id + 0x0900, n~orientM(0,0,-111,+13))
          add(id + 0x0d00, n~orientM(0,0,+111,-3))
        }
        // add(id + 0x0800, n~(+1,-3,+1,-13)) // (avelike)
        // add(id + 0x0900, n~(0,0,-1,+13)) // (avelike)
      }
    }

    // crossings
    for {
      n <- RhwNetworks.iterator
      n2 <- RhwResolver.rhwPieceId.keysIterator
      if !RhwResolver.greater(n2, n)
      if NP.intersectionAllowed(n, n2)
    } {
      val pid = RhwResolver.rhwPieceId(n2) + (if (n.height == 0 && n2 == Str) 4 else 0)  // Str has offset 0x09 instead of 0x05
      val id = RhwResolver.rhwRangeId(n) + pid
      val dir1 = if (NP.isSingleTile(n)) 0x80 else 0x40
      val dir2 = if (n.height == 0 && n2.height == 0) 0x09 else 0x05
      val (msk1a, msk1b) = if (NP.isRhwShoulder(n)) (0x00, 0xf0) else (0xf0, 0x00)  // reversal of direction
      val (msk2a, msk2b) = if (NP.isRhwShoulder(n2)) (0x00, 0x0f) else (0x0f, 0x00)  // reversal of direction
      val off8Diag = if (n2.height != 0 && Network.Viaducts.contains(n2)) 5 else 0  // use 5/A instead of 0/5 as 8th digit (presumably to avoid wealth texture conflict)
      def asymmOrShared(network: Network) = !network.isSymm && network != Owr4m  // Owr4m shared diagonals use Owr4 IDs instead
      def asymmOrOwr4(network: Network) = network.typ == Asymmetrical || network.isOwr4Like && network != Owr4m  // Owr4 has fewer symmetries than Avenue
      val orientA: IntFlags => IntFlags = if (n2 == Ard3) reverseIntFlags else identity

      // O×O
      add(n~NS & n2~orientA(EW), id + 0x0000)
      // O×D
      add(n~NS & n2~SW, id + 0x3000 + off8Diag + (dir2 & msk2b))
      add(n~NS & n2~WS, id + 0x3000 + off8Diag + (dir2 & msk2a), when = asymmOrShared(n2))
      add(n~SN & n2~SW, id + 0x3000 + off8Diag + (dir2 & msk2b | dir1), when = !n.isSymm)
      add(n~SN & n2~WS, id + 0x3000 + off8Diag + (dir2 & msk2a | dir1), when = !n.isSymm && asymmOrOwr4(n2))
      // D×O
      if (n != n2) {
        add(n~ES & n2~EW, id + 0x6000 + off8Diag + (dir1 & msk1b))
        add(n~ES & n2~WE, id + 0x6000 + off8Diag + (dir1 & msk1b | dir2), when = !n2.isSymm)
        add(n~SE & n2~EW, id + 0x6000 + off8Diag + (dir1 & msk1a), when = !n.isSymm)
        add(n~SE & n2~WE, id + 0x6000 + off8Diag + (dir1 & msk1a | dir2), when = (n.typ == Asymmetrical) && !n2.isSymm)
      }
      // D×D
      add(n~ES & n2~SW, id + 0x9000 + off8Diag + (dir1 & msk1b | dir2 & msk2b))
      add(n~ES & n2~WS, id + 0x9000 + off8Diag + (dir1 & msk1b | dir2 & msk2a), when = asymmOrShared(n2))
      add(n~SE & n2~SW, id + 0x9000 + off8Diag + (dir1 & msk1a | dir2 & msk2b), when = !n.isSymm)
      add(n~SE & n2~WS, id + 0x9000 + off8Diag + (dir1 & msk1a | dir2 & msk2a), when = !n.isSymm && asymmOrShared(n2))
    }

    // T intersections with viaducts
    // Rhw2
    add(0x57600110, L1Rhw2~NS & L1Road~EC)
    add(0x57600120, L2Rhw2~NS & L2Road~EC)
    add(0x57600210, L1Rhw2~NS & L1Onewayroad~EC)
    add(0x57600220, L2Rhw2~NS & L2Onewayroad~EC)
    add(0x57600310, L1Rhw2~NS & L1Avenue~EC)
    add(0x57600320, L2Rhw2~NS & L2Avenue~EC)
    add(0x57601110, L1Rhw2~CE & L1Road~NS)
    add(0x57601120, L2Rhw2~CE & L2Road~NS)
    add(0x57601210, L1Rhw2~CE & L1Onewayroad~NS)
    add(0x57601220, L2Rhw2~CE & L2Onewayroad~NS)
    add(0x57601310, L1Rhw2~CE & L1Avenue~SN)
    add(0x57601315, L1Rhw2~CE & L1Avenue~NS)
    add(0x57601320, L2Rhw2~CE & L2Avenue~SN)
    add(0x57601325, L2Rhw2~CE & L2Avenue~NS)
    // Rhw3 (incomplete)
    add(0x57610310, L1Rhw3~NS & L1Avenue~EC)
    add(0x57610320, L2Rhw3~NS & L2Avenue~EC)
    add(0x57610390, L1Rhw3~SN & L1Avenue~EC)
    add(0x576103a0, L2Rhw3~SN & L2Avenue~EC)
    add(0x57611310, L1Rhw3~NC & L1Avenue~EW)
    add(0x57611315, L1Rhw3~CE & L1Avenue~NS)
    add(0x57611320, L2Rhw3~NC & L2Avenue~EW)
    add(0x57611325, L2Rhw3~CE & L2Avenue~NS)
    // Mis
    add(0x57620110, L1Mis~NS & L1Road~EC)
    add(0x57620120, L2Mis~NS & L2Road~EC)
    add(0x57620190, L1Mis~SN & L1Road~EC)
    add(0x576201a0, L2Mis~SN & L2Road~EC)
    add(0x57620210, L1Mis~NS & L1Onewayroad~EC)
    add(0x57620220, L2Mis~NS & L2Onewayroad~EC)
    add(0x57620290, L1Mis~SN & L1Onewayroad~EC)
    add(0x576202a0, L2Mis~SN & L2Onewayroad~EC)
    // (Avenue ending at Mis is not possible due to lane math)
    add(0x57621110, L1Mis~EC & L1Road~NS)
    add(0x57621120, L2Mis~EC & L2Road~NS)
    add(0x57621210, L1Mis~EC & L1Onewayroad~NS)
    add(0x57621220, L2Mis~EC & L2Onewayroad~NS)
    add(0x57621310, L1Mis~EC & L1Avenue~SN)
    add(0x57621315, L1Mis~EC & L1Avenue~NS)
    add(0x57621320, L2Mis~EC & L2Avenue~SN)
    add(0x57621325, L2Mis~EC & L2Avenue~NS)
    // Rhw4
    add(0x57630110, L1Rhw4~NS & L1Road~EC)
    add(0x57630120, L2Rhw4~NS & L2Road~EC)
    add(0x57630190, L1Rhw4~SN & L1Road~EC)
    add(0x576301a0, L2Rhw4~SN & L2Road~EC)
    add(0x57630210, L1Rhw4~NS & L1Onewayroad~EC)
    add(0x57630220, L2Rhw4~NS & L2Onewayroad~EC)
    add(0x57630290, L1Rhw4~SN & L1Onewayroad~EC)
    add(0x576302a0, L2Rhw4~SN & L2Onewayroad~EC)
    add(0x57630310, L1Rhw4~NS & L1Avenue~EC)
    add(0x57630320, L2Rhw4~NS & L2Avenue~EC)
    add(0x57630390, L1Rhw4~SN & L1Avenue~EC)
    add(0x576303a0, L2Rhw4~SN & L2Avenue~EC)
    add(0x57631110, L1Rhw4~EC & L1Road~NS)
    add(0x57631120, L2Rhw4~EC & L2Road~NS)
    add(0x57631210, L1Rhw4~EC & L1Onewayroad~NS)
    add(0x57631220, L2Rhw4~EC & L2Onewayroad~NS)
    add(0x57631310, L1Rhw4~EC & L1Avenue~SN)
    add(0x57631315, L1Rhw4~EC & L1Avenue~NS)
    add(0x57631320, L2Rhw4~EC & L2Avenue~SN)
    add(0x57631325, L2Rhw4~EC & L2Avenue~NS)

    builder.result()
  }
}
