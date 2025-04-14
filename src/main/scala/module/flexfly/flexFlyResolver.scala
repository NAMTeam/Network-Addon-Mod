package com.sc4nam.module
package flexfly

import io.github.memo33.metarules.meta._, syntax._, Network._, RotFlip._, Flags._, Implicits._

/* Flags of 5x5 FlexFly:
 *       ,---------,---------,---------,
 *       |  [T0]   |  [T1]   |  [T2]   |
 *       |+2   -213|+213 -223|+223     | over
 *       |         |         |         | hang
 *       '---------'---------;---------;---------,
 *                      over |  [T3]   |  [T4]   |
 *                      hang |+233 -243|+243     | over
 *                           |         |  -241   | hang
 *                           '---------;---------;---------,
 *                                     |  +241   |         |
 *                                     |  [T3']  |  [T2']  |
 *                                     |  -231   |  -221   |
 *                                     '---------;---------;
 *                                          over |  +221   |
 *                                          hang |  [T1']  |
 *                                               |  -211   |
 *                                               ;---------;
 *                                               |  +211   |
 *                                               |  [T0']  |
 *                                               |   -2    |
 *                                               '---------'
 * Flags of 45 degree FlexFly:
 *       ,---------,---------,---------,
 *       |  [T0]   |  [T1]   |  [T2]   |
 *       |+2   -213|+213 -223|+223     | over
 *       |         |         |         | hang
 *       '---------'---------;---------;---------,
 *                      over |  [T3]   |  [T6]   |
 *                      hang |+233 -243|+243     |
 *                           |         |   -1    |
 *                           '---------'---------'
 */
object FlexFlyTiles {

  val T0 = (+2,0,-213,0)
  val T1 = (+213,0,-223,0)
  val T2 = (+223,0,0,0)
  //val unused = (0,0,-233,0)
  val T3 = (+233,0,-243,0)
  val T4 = (+243,0,0,-241)
  val T5 = (0,+241,0,-231) // T5 is T3 transposed
  val T6 = (+243,0,0,-1) // 45 degree curve diagonal end tile
  val T7 = (+3,0,0,-241) // T7 is T6 transposed

}

import FlexFlyTiles._

class FlexFlyResolver extends IdResolver {
  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t) || flyFlyCrossings.isDefinedAt(t)
  def apply(tile: Tile): IdTile = tileMap.get(tile).getOrElse(flyFlyCrossings(tile))

  private val setNumber = Array(6, 2, 0xB, 1, 5)
  private def curveNumber(reversed: Boolean, network: Network) =
    if (network >= Rhw4 && network <= L4Rhw4) {
      if (reversed) 5 else 4
    } else { // Mis
      if (reversed) 0xD else 0xC
    }
  private val networkIdMap = (Map.newBuilder
    += Mis    -> 0x00 += L1Mis    -> 0x10 += L2Mis    -> 0x20 += L3Mis   -> 0x30 += L4Mis   -> 0x40
    += Rhw4   -> 0x02 += L1Rhw4   -> 0x12 += L2Rhw4   -> 0x22 += L3Rhw4  -> 0x32 += L4Rhw4  -> 0x42
    += Rhw6s  -> 0x04 += L1Rhw6s  -> 0x14 += L2Rhw6s  -> 0x24 += L3Rhw6s -> 0x34 += L4Rhw6s -> 0x44
    += Rhw3   -> 0x06 += L1Rhw3   -> 0x16 += L2Rhw3   -> 0x26
    += Rhw8sm -> 0x08 += L1Rhw8sm -> 0x18 += L2Rhw8sm -> 0x28
    += Rhw8s  -> 0x0A += L1Rhw8s  -> 0x1A += L2Rhw8s  -> 0x2A
    += Rhw10s -> 0x0C += L1Rhw10s -> 0x1C += L2Rhw10s -> 0x2C
    += Rhw12s -> 0x0E += L1Rhw12s -> 0x1E += L2Rhw12s -> 0x2E
    += Rhw6cm -> 0x50 += L1Rhw6cm -> 0x60 += L2Rhw6cm -> 0x70
    += Rhw6c  -> 0x51 += L1Rhw6c  -> 0x61 += L2Rhw6c  -> 0x71
    += Rhw8c  -> 0x53 += L1Rhw8c  -> 0x63 += L2Rhw8c  -> 0x73
    += Rhw10c -> 0x55 += L1Rhw10c -> 0x65 += L2Rhw10c -> 0x75
    ).result()
  private def networkId(network: Network, h: Int, minReversed: Boolean) = network match {
    case Dirtroad => h << 4 | 0
    case L1Rhw2   => h << 4 | 1
    case L2Rhw2   => h << 4 | 2
    case n =>
      val id = networkIdMap(network)
      if (minReversed) id + 1 else id
  }

  val tileMap = {
    val builder = new ResolverBuilder
    val pieceIds = Seq(T0 -> 0, T1 -> 1, T2 -> 2, T3 -> 3, T4 -> 4, T5 -> 5, T6 -> 6, T7 -> 7)
    for {
      main <- Network.values rangeFrom Mis rangeTo L4Rhw4
      (flags, pid) <- pieceIds
      h = main.height
      sn = setNumber(h)
      reversed <- Seq(false, true)
      cn = curveNumber(reversed, main)
      seg1 = main~(if (reversed) reverseIntFlags(flags) else flags)
    } {
      // base curve
      if (pid != 5 && pid != 7) {  // T5 and T7 are transpose of T3 and T6
        val id = 0x5CA << 20 | sn << 16 | cn << 12 | pid << 8 | h << 4 | h
        builder.add(seg1, id)
      }
      // crossings (FlexFly × Orth)
      for {
        minor <- RhwNetworks
        if minor.height != main.height
        minReversed <- Seq(false, true)
      } {
        val nn = networkId(minor, h, minReversed)
        val id = 0x5CA << 20 | sn << 16 | cn << 12 | pid << 8 | nn
        builder.add(seg1 & (if (minReversed) minor~SN else minor~NS), id, when = !(minor.isSymm && minReversed))
      }
    }
    builder.result()
  }

  val flyFlyCrossings = {
    val builder = new ResolverBuilder
    // FlexFly × FlexFly crossings
    val flyFlyPieceIds = Seq(
      (1, T1, R0F0, T2, R0F1),  // 4-tile gap
      (2, T2, R0F0, T1, R0F1),  // 4-tile gap
      (3, T3, R0F0, T4, R0F1),  // 2-tile gap
      (4, T4, R0F0, T3, R0F1),  // 2-tile gap
      (5, T5, R0F0, T2, R3F0),  // 0-tile gap
      (0, T2, R3F1, T5, R0F1))  // 0-tile gap
    for {
      (pid, flags0, rf0, flags1, rf1) <- flyFlyPieceIds
      main <- Seq(L1Mis, L2Mis, L3Mis, L1Rhw4, L2Rhw4, L3Rhw4)
      top = main~flags0 * rf0
      (minor, minOffset) <- Seq(Rhw4 -> 0, Mis -> 2, L1Rhw4 -> 4, L1Mis -> 6, L2Rhw4 -> 8, L2Mis -> 0xA)
      if minor.height < main.height
      bottom = minor~flags1 * rf1
      reversed <- Seq(false /*, true*/)  // currently inside×inside curves only
      minReversed <- Seq(false /*, true*/)  // currently inside×inside curves only
    } {
      val h0 = top.network.height
      val sn = setNumber(h0)
      val cn = curveNumber(reversed, top.network)
      require(bottom.network.height < 4)
      val nn = 0x80 + minOffset + (if (minReversed) 1 else 0)
      val id = 0x5CA << 20 | sn << 16 | cn << 12 | pid << 8 | nn
      builder.add((if (reversed) top.reverse else top) & (if (minReversed) bottom.reverse else bottom), id)
    }
    builder.result()
  }

}
