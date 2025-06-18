package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, RotFlip._, Flags._
import Implicits.segmentToTile
import com.sc4nam.module.{NetworkProperties => NP}

object ViaductResolver {

  val viaductRangeId = Map(
    L1Road       -> 0x5c000000,
    L1Onewayroad -> 0x5c010000,
    L1Avenue     -> 0x5c020000,
    L2Road       -> 0x5c030000,
    L2Onewayroad -> 0x5c040000,
    L2Avenue     -> 0x5c050000,
  )

  val viaductPieceId =
    Map.from[Network, Int](Iterable(
      0x1000 -> Street,
      0x1100 -> Road,          0x1110 -> L1Road,          0x1120 -> L2Road,
      0x1200 -> Onewayroad,    0x1210 -> L1Onewayroad,    0x1220 -> L2Onewayroad,
      0x1300 -> Avenue,        0x1310 -> L1Avenue,        0x1320 -> L2Avenue,
      0x1400 -> Groundhighway,                            0x1420 -> Highway,
      0x1500 -> Rail,          0x1505 -> Str,
      0x1700 -> Glr1,          0x1705 -> Glr3,            0x1720 -> Lightrail, // 0x1730 L4 Lightrail
      0x1800 -> Glr2,          0x1805 -> Glr4,
      0x1905 -> Hsr,           0x1925 -> L2Hsr,           0x1920 -> Monorail, // 0x1930 L4 Monorail

      0x1a00 -> Tla3,    0x1b00 -> Ave2,   0x1c00 -> Ard3,
      0x1d00 -> Owr1,    0x1e00 -> Owr3,   0x1f00 -> Nrd4,
      0x2000 -> Tla5,    0x2100 -> Owr4,   0x2b00 -> Owr4m,   0x2200 -> Owr5,
      0x2300 -> Rd4,     0x2400 -> Rd6,    0x2500 -> Ave6,
      0x2580 -> Tla7m,   0x2600 -> Ave8,   0x2680 -> Ave6m,
      // 0x2700 Tram-on-Street, 0x2800 Tram-in-Road, 0x2805 Tram-on-Road, 0x2a00 Tram-in-Avenue
    ).map(_.swap))
}

class ViaductResolver extends IdResolver {
  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)
  def apply(tile: Tile): IdTile = tileMap(tile)

  val tileMap = {
    val builder = new ResolverBuilder(
      // To simplify adding shared diagonals, we automatically add them for avenue-like networks going in the wrong direction.
      remap = (tile: Tile) => NP.transformSharedDiagonals(tile),
    )
    import builder.add

    // curves
    for (n <- Viaducts) {
      val id = ViaductResolver.viaductRangeId(n)

      if (NP.isSingleTile(n)) {
        add(id + 0x0400, n~(0,2,0,11))  // 45 curve
        add(id + 0x0500, n~(0,0,1,13))  // 45 curve
        add(id + 0x0600, n~(0,11,0,11))  // S curve
        add(id + 0x0700, n~(0,11,0,13))  // boomerang
        add(id + 0x0800, n~(0,0,2,2))  // 90 curve
      } else {
        add(id + 0x0300, n~SharedDiagLeft)
        add(id + 0x0500, n~(0,-2,0,+11)) // 45 curve
        add(id + 0x0600, n~(0,+2,0,-11)) // 45 curve
        add(id + 0x0700, n~(0,-11,+3,0)) // 45 curve
        add(id + 0x0800, n~(-3,+11,-3,+1)) // 45 curve
        add(id + 0x0a00, n~(+2,0,-113,0)) // 90 curve extended
        add(id + 0x0b00, n~(+2,0,0,-2)) // 90 curve outside
        add(id + 0x0c00, n~(-2,0,0,+2)) // 90 curve inside
      }
    }

    // crossings
    for {
      n <- Viaducts.iterator
      n2 <- ViaductResolver.viaductPieceId.keysIterator
      if !RhwResolver.greater(n2, n)
      if NP.intersectionAllowed(n, n2) || n.height == 2 && (n2 == Lightrail || n2 == Monorail)  // Lightrail and Monorail are base network crossings, but might not actually be needed for overrides
    } {
      val pid = ViaductResolver.viaductPieceId(n2)
      val id = ViaductResolver.viaductRangeId(n) + pid
      val (rev00, rev01, rev10, rev11) =  // for reversed directions of networks
        if (n.typ == AvenueLike) (0x00, 0x09, 0x80, 0x89)
        else (0x00, 0x05, 0x80, 0x85)
      val orientA: IntFlags => IntFlags = if (n2 == Ard3) reverseIntFlags else identity

      // O×O
      add(n~NS & n2~orientA(EW), id + 0x0000)

      // O×D
      if (!Viaducts.contains(n2)) {
        val ne = if (n2.typ == AvenueLike) SharedDiagLeft else NE
        val en = if (n2.typ == AvenueLike) SharedDiagLeft else EN
        val ws = if (n2.typ == AvenueLike) SharedDiagLeft else WS
        if (n.typ == AvenueLike) {
          if (!NP.isTripleTile(n2) || !n2.isSymm) {
            add(n~NS & n2~SW, id + 0x3000 + rev00)
            add(n~NS & n2~NE, id + 0x3000 + rev01, when = !n2.isSymm)
            add(n~NS & n2~en, id + 0x3000 + rev10, when = !n.isSymm)  // TODO Monorail swaps 0x80 and 0x00 -> model issue only
            add(n~NS & n2~WS, id + 0x3000 + rev11, when = !n.isSymm && (n2.typ == Asymmetrical))
          }
          if (NP.isTripleTile(n2) && n2.isSymm) {
            add(n~NS & n2~SW, id + 0x2F8E)
            add(n~NS & n2~en, id + 0x300E)  // TODO Monorail swaps 0x80 and 0x00 -> model issue only
          }
        } else {
          add(n~NS & n2~SW, id + 0x3000 + rev00)
          add(n~NS & n2~ws, id + 0x3000 + rev01, when = !n2.isSymm && (n2 != Owr4 && n2 != Owr4m))
        }
      }
      // D×O
      if (!Viaducts.contains(n2)) {
        if (n.typ == AvenueLike) {
          add(n~NE             & n2~NS, id + 0x6000 + rev00)
          add(n~NE             & n2~SN, id + 0x6000 + rev01, when = !n2.isSymm)
          add(n~SharedDiagLeft & n2~NS, id + 0x6000 + rev01, when = !n.isSymm && NP.isTripleTile(n2) && n2.isSymm)
          add(n~SharedDiagLeft & n2~NS, id + 0x6000 + rev10, when = !n.isSymm && !NP.isTripleTile(n2))
          add(n~SharedDiagLeft & n2~NS, id + 0x600E, when = !n.isSymm && (NP.isTripleTile(n2) && !n2.isSymm))
          add(n~SharedDiagLeft & n2~SN, id + 0x6000 + rev11, when = (n.typ == Asymmetrical) && !n2.isSymm && !NP.isTripleTile(n2))
        } else {
          add(n~ES & n2~EW, id + 0x6000 + rev00)
          add(n~ES & n2~WE, id + 0x6000 + rev01, when = !n2.isSymm)
        }
      }
      // D×D
      if (!Viaducts.contains(n2)) {
        val se = if (n2.typ == AvenueLike) SharedDiagRight else SE
        val es = if (n2.typ == AvenueLike) SharedDiagRight else ES
        if (n.typ == AvenueLike) {
          if (!NP.isTripleTile(n2)) {
            add(n~NE & n2~ES,              id + 0x9000 + rev00)
            add(n~NE & n2~se, id + 0x9000 + rev01, when = n2.typ == AvenueLike)
            add(n~NE & n2~se, id + 0x9000 + rev01, when = n2.typ != AvenueLike && !n2.isSymm)
            add(n~SharedDiagLeft & n2~se,  id + 0x9000 + rev10, when = !n.isSymm && (n2 != Owr4 && n2 != Owr4m))
            add(n~SharedDiagLeft & n2~WN,  id + 0x9000 + rev11, when = !n.isSymm && !n2.isSymm)
          } 
          if (!n2.isSymm && NP.isTripleTile(n2)) {
            add(n~NE & n2~ES,              id + 0x9000 + rev00)
            add(n~NE & n2~se, id + 0x9000 + rev01, when = n2.typ != AvenueLike && !n2.isSymm)
            add(n~SharedDiagLeft & n2~se,  id + 0x9000 + rev10, when = !n.isSymm)
            add(n~SharedDiagLeft & n2~WN,  id + 0x9000 + rev11, when = !n.isSymm && !n2.isSymm)
          } 
          if (n2.isSymm && NP.isTripleTile(n2)) {
            add(n~NE & n2~ES,              id + 0x8F8E, when = n2.isSymm && NP.isTripleTile(n2))
            add(n~SharedDiagLeft & n2~se,  id + 0x900E, when = !n.isSymm && n2.isSymm && NP.isTripleTile(n2))
          }
        } else {
          add(n~ES & n2~SW,             id + 0x9000 + rev00)
          add(n~ES & n2~WS,             id + 0x9000 + rev01, when = !n2.isSymm && n2.typ != AvenueLike)
          add(n~ES & n2~SharedDiagLeft, id + 0x9000 + rev10, when = n2.typ == AvenueLike)
        }
      }
    }

    // T intersections
    for (n <- Viaducts) {
      val id = ViaductResolver.viaductRangeId(n)
      if (n.height == 1) {
        add(id + 0x3110, n~SN & L1Road~CE, when = n.isSymm)
        add(id + 0x3110, n~SN & L1Road~CE, when = !n.isSymm)
        add(id + 0x3115, n~SN & L1Road~WC, when = !n.isSymm)
        add(id + 0x3210, n~SN & L1Onewayroad~CE, when = n.isSymm)
        add(id + 0x3210, n~SN & L1Onewayroad~CE, when = !n.isSymm)
        add(id + 0x3215, n~SN & L1Onewayroad~WC, when = !n.isSymm)
        add(id + 0x3310, n~WE & L1Avenue~NC, when = n.isSymm)
        add(id + 0x3315, n~WE & L1Avenue~NC, when = !n.isSymm)
      }
      if (n.height == 2) {
        add(id + 0x3120, n~SN & L2Road~CE, when = n.isSymm)
        add(id + 0x3120, n~SN & L2Road~CE, when = !n.isSymm)
        add(id + 0x3125, n~SN & L2Road~WC, when = !n.isSymm)
        add(id + 0x3220, n~SN & L2Onewayroad~CE, when = n.isSymm)
        add(id + 0x3220, n~SN & L2Onewayroad~CE, when = !n.isSymm)
        add(id + 0x3225, n~SN & L2Onewayroad~WC, when = !n.isSymm)
        add(id + 0x3320, n~WE & L2Avenue~NC, when = n.isSymm)
        add(id + 0x3325, n~WE & L2Avenue~NC, when = !n.isSymm)
      }
    }

    // Onslope transitions
    add(0x5c060000, L1Road~NC & Road~CS)
    add(0x5c060010, L2Road~NC & L1Road~CS)
    add(0x5c060100, L2Road~NC & Road~CS)
    add(0x5c070000, L1Onewayroad~NC & Onewayroad~CS)
    add(0x5c070010, L2Onewayroad~NC & L1Onewayroad~CS)
    add(0x5c070100, L2Onewayroad~NC & Onewayroad~CS)
    add(0x5c080000, L1Avenue~NC & Avenue~CS)
    add(0x5c080010, L2Avenue~NC & L1Avenue~CS)
    add(0x5c080100, L2Avenue~NC & Avenue~CS)


    add(0x5C005105, L1Road~NS & Owr4m~SE & Owr4~NW)  // Owr4/Owr4m
    add(0x5C015105, L1Onewayroad~NS & Owr4m~SE & Owr4~NW)  // Owr4/Owr4m
    add(0x5C02510E, L1Avenue~NS & Owr4m~SE & Owr4~NW)  // Owr4/Owr4m
    add(0x5C02518E, L1Avenue~NS & Owr4m~WS & Owr4~EN)  // Owr4/Owr4m
    add(0x5C035105, L2Road~NS & Owr4m~SE & Owr4~NW)  // Owr4/Owr4m
    add(0x5C045105, L2Onewayroad~NS & Owr4m~SE & Owr4~NW)  // Owr4/Owr4m
    add(0x5C05510E, L2Avenue~NS & Owr4m~SE & Owr4~NW)  // Owr4/Owr4m
    add(0x5C05518E, L2Avenue~NS & Owr4m~WS & Owr4~EN)  // Owr4/Owr4m
    add(0x5C00B105, L1Road~ES & Owr4m~WS & Owr4~EN)  // Owr4/Owr4m
    add(0x5C01B105, L1Onewayroad~ES & Owr4m~WS & Owr4~EN)  // Owr4/Owr4m
    add(0x5C02B109, L1Avenue~NE & Owr4m~SE & Owr4~NW)  // Owr4/Owr4m
    add(0x5C02B180, L1Avenue~SharedDiagLeft & Owr4m~SE & Owr4~NW)  // Owr4/Owr4m
    add(0x5C03B105, L2Road~ES & Owr4m~WS & Owr4~EN)  // Owr4/Owr4m
    add(0x5C04B105, L2Onewayroad~ES & Owr4m~WS & Owr4~EN)  // Owr4/Owr4m
    add(0x5C05B109, L2Avenue~NE & Owr4m~SE & Owr4~NW)  // Owr4/Owr4m
    add(0x5C05B180, L2Avenue~SharedDiagLeft & Owr4m~SE & Owr4~NW)  // Owr4/Owr4m

    builder.result()
  }
}
