package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, RotFlip._, Flags._, group.SymGroup
import Implicits.segmentToTile
import NetworkProperties.{isSingleTile, isTripleTile, nonMirroredOnly, mirroredOnly, hasTurnPaths}
import com.sc4nam.module.{NetworkProperties => NP}

object NwmResolver {

  val nwmRangeId = Map(
    Tla3          -> 0x51000000,
    Ave2          -> 0x51010000,
    Ard3          -> 0x51020000,
    Owr1          -> 0x51030000,
    Owr3          -> 0x51040000,
    Nrd4          -> 0x51050000,

    Tla5          -> 0x51100000,
    Owr4          -> 0x51110000,
    Owr5          -> 0x51120000,
    Rd4           -> 0x51130000,
    Rd6           -> 0x51140000,

    Ave6          -> 0x51200000,
    Tla7m         -> 0x51200080,  // with overflow 0x51220000
    Ave8          -> 0x51210000,
    Ave6m         -> 0x51210080)  // with overflow 0x51220080

  val nwmRangeIdOverflow = Map(  // for diagonal intersections
    Tla7m         -> 0x51220000,
    Ave6m         -> 0x51220080).orElse(nwmRangeId)

  val nwmPieceId = Map(
    Street        -> 0x0000,
    Road          -> 0x0100,
    Onewayroad    -> 0x0200,
    Avenue        -> 0x0300,
    Highway       -> 0x0420,
    Groundhighway -> 0x0400,
    Rail          -> 0x0500,
    Lightrail     -> 0x0600,
    Monorail      -> 0x0700,
    Glr1          -> 0x0800,
    Glr2          -> 0x0900,
    L2Hsr         -> 0x0A00,  // previously 0x1700, moved here to avoid DxO/DxD collision
    Str           -> 0x0F00,

    Tla3          -> 0x1000,
    Ave2          -> 0x1100,
    Ard3          -> 0x1200,
    Owr1          -> 0x1300,
    Owr3          -> 0x1400,
    Nrd4          -> 0x1500,

    Tla5          -> 0x1800,
    Owr4          -> 0x1900,
    Owr5          -> 0x1A00,
    Rd4           -> 0x1B00,
    Rd6           -> 0x1C00,

    Ave6          -> 0x1D00,
    Tla7m         -> 0x1D0A,
    Ave8          -> 0x1E00,
    Ave6m         -> 0x1E0A)
    // currently not defined:
    // Glr3          -> 0x....,
    // Glr4          -> 0x....,
    // Hsr           -> 0x....,

  // orientation relative to RHW scheme
  lazy val orientationOffsetOxO: Map[Network.ValueSet, RotFlip] = {
    val map = collection.mutable.Map.empty[Network.ValueSet, RotFlip]
    val crossingNetworks = Network.ValueSet() ++ nwmPieceId.keysIterator
    map.getOrElseUpdate(Ard3 + Rail, R2F0)
    for (main <- NwmNetworks; minor <- crossingNetworks if !minor.isNwm || minor <= main) {
      if (main == Ard3) {
        map.getOrElseUpdate(main + minor, R3F0)
      } else if (isSingleTile(main) && minor == Rail) {
        map.getOrElseUpdate(main + minor, R0F0)
      } else if (!isSingleTile(main) && minor == Ard3) {
        map.getOrElseUpdate(main + minor, R1F0)
      } else {
        map.getOrElseUpdate(main + minor, R1F1)  // default
      }
    }
    map.toMap
  }
}
import NwmResolver._

class NwmResolver extends IdResolver {
  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)
  def apply(tile: Tile): IdTile = tileMap(tile)

  val tileMap = {
    val builder = new ResolverBuilder
    import builder.add

    for (n <- NwmNetworks) {
      val id = nwmRangeId(n)
      val orientA: IntFlags => IntFlags = if (n == Ard3) reverseIntFlags else identity
      add(id + 0x0000, n~orientA(NS))  // orth
      add(id + 0x0100, n~CS)  // orth stub

      if (NP.isSingleTile(n)) {
        add(id + 0x0200, n~orientA(ES))  // diag 1
        add(id + 0x0300, n~orientA(SWC))  // diag stub 1
        add(id + 0x0400, n~(0,-2,0,+11))  // 45 curve 1
        add(id + 0x0500, n~(0,0,-1,+13))  // 45 curve 1
        if (!n.isSymm) {
          add(id + 0x0900, n~orientA(SE))  // diag 2
          add(id + 0x0a00, n~orientA(CWS))  // diag stub 2
          add(id + 0x0b00, n~(0,+2,0,-11))  // 45 curve 2
          add(id + 0x0c00, n~(0,0,+1,-13))  // 45 curve 2
        }
      } else {
        // multi-tile networks (partially defined in MiscResolver)
      }
    }

    // Multi-tile NWM curve assembly
    // (listed explicitly to simplify maintaining compatibility with old IID scheme)
    // (TODO consider migrating sharp curves to RHW-spec mini curves or extended curves)
    for (n <- Seq(Tla5, Rd6, Owr5)) {
      add(nwmRangeId(n) + 0x0400, n~(0,-13,0,+2))  // sharp curve outside
      add(nwmRangeId(n) + 0x0500, n~(0,0,+1,-13))  // sharp curve inside
      add(nwmRangeId(n) + 0x0509, n~(0,+13,0,-2))  // sharp curve inside (TODO add orthogonal placeholder texture)
      add(nwmRangeId(n) + 0x0600, n~(0,0,-1,+13))  // sharp curve outside
    }
    for (n <- Seq(Rd4, Owr4)) {
      add(nwmRangeId(n) + 0x0500, n~(0,-2,0,+11))  // shared diagonal curve outside
      add(nwmRangeId(n) + 0x0600, n~(0,+2,0,-11))  // shared diagonal curve inside
      add(nwmRangeId(n) + 0x0700, n~(0,0,-1,+13))  // shared diagonal curve outside
      add(nwmRangeId(n) + 0x0800, n~(+1,-3,+1,-13))  // shared diagonal curve
    }
    for (n <- Seq(Ave6, Ave8)) {
      add(nwmRangeId(n) + 0x0400, n~(0,0,+1,-13))  // sharp curve inside
      add(nwmRangeId(n) + 0x0409, n~(0,+13,0,-2))  // sharp curve inside (TODO add orthogonal placeholder texture)
      add(nwmRangeId(n) + 0x0500, n~(0,-113,0,+2))  // extended curve outside
      add(nwmRangeId(n) + 0x0600, n~(0,-13,0,+113))  // extended curve outside
      add(nwmRangeId(n) + 0x0700, n~(0,0,-111,+13))  // extended curve outside
      add(nwmRangeId(n) + 0x0800, n~(+111,-3,0,0))  // extended curve outside
    }
    for (n <- Seq(Ave6m, Tla7m)) {
      add(nwmRangeId(n) + 0x0400, n~(0,-13,0,+2))  // mini curve
      add(nwmRangeId(n) + 0x0500, n~(0,0,-111,+13))  // mini curve
      add(nwmRangeId(n) + 0x0600, n~(+111,-3,0,0))  // mini curve
    }
    for (n <- NwmNetworks if NP.isSingleTile(n)) {
      add(nwmRangeId(n) + 0x0800, n~(0,0,-2,+2))  // 90 degree curve
      if (!n.isSymm) {
        add(nwmRangeId(n) + 0x0e00, n~(0,0,+2,-2))  // 90 degree curve
      }
    }
    for ((n, offset) <- Seq(Rd4 -> 0, Tla5 -> 0x0100)) {
      add(nwmRangeId(n) + 0x0900 + offset, n~(0,-113,0,+2))  // 90 degree curve extended
      add(nwmRangeId(n) + 0x0980 + offset, n~(0,0,-2,+2))  // 90 degree curve outside
      add(nwmRangeId(n) + 0x0a00 + offset, n~(0,0,+2,-2))  // 90 degree curve inside
    }

    // crossings
    for {
      n <- NwmNetworks.iterator
      n2 <- NwmResolver.nwmPieceId.keysIterator
    } {

      // adds offsets and mirroring variants for some TLA crossings
      def withProjections(tile: Tile, idTile: IdTile, when: Boolean = true): Seq[(Tile, IdTile | (IdTile, IdTile))] = {
        if (!when) Nil
        else if (!tile.segs.exists(_.network.isTla)) {
          Seq((tile, idTile))
        } else {
          val tile0 = tile * (R0F0 / idTile.rf)
          val seq = Seq.newBuilder[(Tile, IdTile | (IdTile, IdTile))]
          if (!tile0.symmetries.exists(_.flipped)) {
            val lhdOffset =
              if (NP.hasTurnPaths(n, n2)) 0x20000000  // 0x7... range instead of 0x5... range, e.g. O×D Tla3×Road
              else 0  // e.g. Tla3×Rail
            // define two different IDs (mirror variants) for each tile
            val idTile1 = idTile.copy(rf = R0F0, mappedRepr = NP.nonMirroredOnly)
            val idTile2 = idTile.copy(rf = R0F0, mappedRepr = NP.mirroredOnly, id = idTile.id + lhdOffset)
            seq += ((NP.projectTlaLeft(tile0), (idTile1, idTile2)))
            seq += ((NP.projectTlaRight(tile0), (idTile2, idTile1)))
          } else {
            // tile does not have mirror variant IDs, e.g. O×O Tla3×Road
            val idTile1 = idTile.copy(rf = R0F0, mappedRepr = NP.nonMirroredOnly)
            seq += ((NP.projectTlaLeft(tile0), idTile1))
            seq += ((NP.projectTlaRight(tile0), idTile1))
          }
          seq.result()
        }
      }

      val pid = NwmResolver.nwmPieceId(n2)
      val (rev00, rev01, rev10, rev11) = (0x00, 0x05, 0x80, 0x85)  // for reversed directions of networks
      val rfOxO = R0F0 / NwmResolver.orientationOffsetOxO(n + n2)
      val n2HasSharedDiag = n2.typ == AvenueLike || n2 == Onewayroad  // e.g. for Owr4
      val ws = if (n2HasSharedDiag) SharedDiagLeft else WS
      val se = if (n.typ == AvenueLike) SharedDiagRight else SE
      def off8(id: Int): Int =  // map 8th digit 5 to 9, A to E
        if (id % 0x10 != 0 && (n.height == 0 || n2.height == 0)) id + 0x4
        else id

      // O×O
      if (!RhwResolver.greater(n2, n)) {
        builder ++= withProjections(n~NS & n2~EW, IdTile(off8(NwmResolver.nwmRangeId(n) + pid + 0x1000), rfOxO))
      }
      // O×D
      val id = NwmResolver.nwmRangeIdOverflow(n) + pid
      if (!RhwResolver.greater(n2, n) || n2.isNwm) {
        builder ++= withProjections(n~NS & n2~SW, IdTile(off8(id + 0x5000 + rev00), R0F0))
        builder ++= withProjections(n~NS & n2~ws, IdTile(off8(id + 0x5000 + rev01), R0F0), when = !n2.isSymm || n2HasSharedDiag)
        builder ++= withProjections(n~SN & n2~SW, IdTile(off8(id + 0x5000 + rev10), R0F0), when = !n.isSymm)
        builder ++= withProjections(n~SN & n2~ws, IdTile(off8(id + 0x5000 + rev11), R0F0), when = !n.isSymm && (n2.typ == Asymmetrical))
      }
      // D×O
      if (!RhwResolver.greater(n2, n) && !n2.isNwm) {
        builder ++= withProjections(n~ES & n2~EW, IdTile(off8(id + 0x7000 + rev00), R0F0))
        builder ++= withProjections(n~ES & n2~WE, IdTile(off8(id + 0x7000 + rev01), R0F0), when = !n2.isSymm)
        builder ++= withProjections(n~se & n2~EW, IdTile(off8(id + 0x7000 + rev10), R0F0), when = !n.isSymm)
        builder ++= withProjections(n~se & n2~WE, IdTile(off8(id + 0x7000 + rev11), R0F0), when = (n.typ == Asymmetrical) && !n2.isSymm)
      }  // else covered by O×D
      // D×D
      if (!RhwResolver.greater(n2, n)) {
        builder ++= withProjections(n~ES & n2~SW, IdTile(off8(id + 0x8000 + rev00), R0F0))
        builder ++= withProjections(n~ES & n2~ws, IdTile(off8(id + 0x8000 + rev01), R0F0), when = !n2.isSymm || n2HasSharedDiag)
        builder ++= withProjections(n~se & n2~SW, IdTile(off8(id + 0x8000 + rev10), R0F0), when = !n.isSymm && (n != n2))
        builder ++= withProjections(n~se & n2~ws, IdTile(off8(id + 0x8000 + rev11), R0F0), when = !n.isSymm && (!n2.isSymm || n2HasSharedDiag))
      }
    }

    builder.result()
  }
}
