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
    Owr4m         -> 0x51150000,

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

    Owr4m         -> 0x1700,
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
    map.getOrElseUpdate(Ard3 + Str, R2F0)
    for (main <- NwmNetworks; minor <- crossingNetworks if !minor.isNwm || minor <= main) {
      if (main == Ard3) {
        map.getOrElseUpdate(main + minor, R3F0)
      } else if (isSingleTile(main) && (minor == Rail || minor == Str)) {
        map.getOrElseUpdate(main + minor, R0F0)
      } else if (minor == Ard3) {
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
    val builder = new ResolverBuilder(
      // To simplify adding shared diagonals, we automatically add them for avenue-like networks going in the wrong direction.
      remap = (tile: Tile) => NP.transformSharedDiagonals(tile),
    )
    import builder.add

    for (n <- NwmNetworks) {
      val id = nwmRangeId(n)
      val orientA: IntFlags => IntFlags = if (n == Ard3) reverseIntFlags else identity
      add(id + 0x0000, n~orientA(NS))  // orth
      add(id + 0x0100, n~orientA(CS))  // orth stub

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
      }
      if (n == Rd4) {
        add(id + 0x0D00, n~(1,0,0,3)) // diag stub inside
      } 
      // if (n == Owr4 || n == Owr4m) {
        // add(id + 0x0D00, Owr4~(1,0,0,0) && Owr4m~(0,0,0,3)) // diag stub inside
      // } 
      if (n == Tla5 || n == Owr5 || n == Rd6 || n == Ave6 || n == Ave8) {
        // multi-tile networks (partially defined in MiscResolver)
        add(id + 0x0D00, n~orientA(SWC))  // diag stub 1
        add(id + 0x0C00, n~orientA(CWS))  // diag stub 2
      }
      if (n == Tla7m || n == Ave6m) {
        // multi-tile networks (partially defined in MiscResolver)
        add(id + 0x0C00, n~orientA(SWC))  // diag stub 1
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
    for (n <- Seq(Rd4, Owr4, Owr4m)) {
      add(nwmRangeId(n) + 0x0500, n~(0,-2,0,+11))  // shared diagonal curve outside
      add(nwmRangeId(n) + 0x0600, n~(0,+2,0,-11))  // shared diagonal curve inside
      add(nwmRangeId(n) + 0x0700, n~(0,0,-1,+13))  // shared diagonal curve outside
      if (!n.isOwr4Like) {  // corresponding Owr4 tile is defined in MiscResolver to circumvent `remap`
        add(nwmRangeId(n) + 0x0800, n~(+1,-3,+1,-13))  // shared diagonal curve
      }
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
    for ((n, offset) <- Seq(Rd4 -> 0, Tla5 -> 0x0100, Owr4 -> 0, Owr4m -> 0)) {
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
      def asymmOrShared(network: Network) = !network.isSymm && network != Owr4m  // Owr4m shared diagonals use Owr4 IDs instead
      def asymmOrOwr4(network: Network) = network.typ == Asymmetrical || network.isOwr4Like && network != Owr4m  // Owr4 has fewer symmetries than Avenue
      def off8(id: Int): Int =  // map 8th digit 5 to 9, A to E
        if (id % 0x10 != 0 && (n.height == 0 && n2.height == 0)) id + 0x4
        else id

      // O×O
      if (!RhwResolver.greater(n2, n)) {
        builder ++= withProjections(n~NS & n2~EW, IdTile(off8(NwmResolver.nwmRangeId(n) + pid + 0x1000), rfOxO))
      }
      // O×D
      val id = NwmResolver.nwmRangeIdOverflow(n) + pid
      if (!RhwResolver.greater(n2, n) || n2.isNwm) {
        builder ++= withProjections(n~NS & n2~SW, IdTile(off8(id + 0x5000 + rev00), R0F0))
        builder ++= withProjections(n~NS & n2~WS, IdTile(off8(id + 0x5000 + rev01), R0F0), when = asymmOrShared(n2))
        builder ++= withProjections(n~SN & n2~SW, IdTile(off8(id + 0x5000 + rev10), R0F0), when = !n.isSymm)
        builder ++= withProjections(n~SN & n2~WS, IdTile(off8(id + 0x5000 + rev11), R0F0), when = !n.isSymm && asymmOrOwr4(n2))
      }
      // D×O
      if (!RhwResolver.greater(n2, n) && !n2.isNwm) {
        builder ++= withProjections(n~ES & n2~EW, IdTile(off8(id + 0x7000 + rev00), R0F0))
        builder ++= withProjections(n~ES & n2~WE, IdTile(off8(id + 0x7000 + rev01), R0F0), when = !n2.isSymm)
        builder ++= withProjections(n~SE & n2~EW, IdTile(off8(id + 0x7000 + rev10), R0F0), when = asymmOrShared(n))
        builder ++= withProjections(n~SE & n2~WE, IdTile(off8(id + 0x7000 + rev11), R0F0), when = asymmOrOwr4(n) && !n2.isSymm)
      }  // else covered by O×D
      // D×D
      if (!RhwResolver.greater(n2, n)) {
        builder ++= withProjections(n~ES & n2~SW, IdTile(off8(id + 0x8000 + rev00), R0F0))
        builder ++= withProjections(n~ES & n2~WS, IdTile(off8(id + 0x8000 + rev01), R0F0), when = asymmOrShared(n2))
        builder ++= withProjections(n~SE & n2~SW, IdTile(off8(id + 0x8000 + rev10), R0F0), when = asymmOrShared(n) && (n != n2))
        builder ++= withProjections(n~SE & n2~WS, IdTile(off8(id + 0x8000 + rev11), R0F0), when = asymmOrShared(n) && asymmOrShared(n2))
      }

    }

    //T-intersections
    //NWM-Thru
    //Single-Tile Networks except Tla3
    for (n <- Seq(Ave2, Ard3, Owr1, Owr3, Nrd4)) {
      //OxO
      add(nwmRangeId(n) + 0x3000, n~WE & Street~CS)  // Street
      add(nwmRangeId(n) + 0x3100, n~WE & Road~CS)  // Road
      add(nwmRangeId(n) + 0x3200, n~WE & Onewayroad~CS)  // Onewayroad
      add(nwmRangeId(n) + 0x4300, n~WE & Avenue~NC, when = n != Owr1)  // Avenue
      add(nwmRangeId(n) + 0x3800, n~NS & Tla3~CE)  // TLA-3
      add(nwmRangeId(n) + 0x3900, n~NS & Ave2~CE)  // AVE-2
      add(nwmRangeId(n) + 0x3A00, n~NS & Ard3~EC, when = n != Owr1)  // ARD-3a
      add(nwmRangeId(n) + 0x3B00, n~NS & Owr1~CE)  // OWR-1
      add(nwmRangeId(n) + 0x3C00, n~NS & Owr3~CE, when = n != Ave2 || n != Owr1)  // OWR-3
      add(nwmRangeId(n) + 0x3D00, n~NS & Nrd4~CE, when = n != Owr1)  // NRD-4
      add(nwmRangeId(n) + 0x4800, n~NS & Tla5~EC, when = n != Owr1)  // TLA-5
      add(nwmRangeId(n) + 0x4A00, n~NS & Rd4~EC, when = n != Owr1)  // RD-4
      add(nwmRangeId(n) + 0x4B00, n~NS & Rd6~EC, when = (n == Ard3 || n == Owr3 || n == Nrd4))  // RD-4
      add(nwmRangeId(n) + 0x4C00, n~NS & Ave6~EC, when = (n == Ard3 || n == Owr3 || n == Nrd4))  // AVE-6
      add(nwmRangeId(n) + 0x4C09, n~NS & Tla7m~EC, when = (n == Ard3 || n == Owr3 || n == Nrd4))  // TLA-M
      add(nwmRangeId(n) + 0x4D09, n~NS & Ave6m~EC, when = (n == Ard3 || n == Owr3 || n == Nrd4))  // AVE-M
      add(nwmRangeId(n) + 0x4900, n~NS & Owr4~EC, when = n == Nrd4)  // OWR-4
      add(nwmRangeId(n) + 0x4909, n~NS & Owr4m~EC, when = n == Nrd4)  // OWR-4m
      add(nwmRangeId(n) + 0x4D00, n~NS & Ave8~EC, when = n == Nrd4)  // AVE-8
      //OxD
      add(nwmRangeId(n) + 0xA000, n~NS & Street~CSE)  // Street
      add(nwmRangeId(n) + 0xA100, n~NS & Road~CSE)  // Road
      add(nwmRangeId(n) + 0xA200, n~NS & Onewayroad~CSE)  // Onewayroad
      add(nwmRangeId(n) + 0xA300, n~NS & Avenue~(0,0,0,1), when = n != Owr1)  // Avenue
      add(nwmRangeId(n) + 0xA400, n~NS & Avenue~(1,0,0,3), when = n != Owr1)  // Avenue
      add(nwmRangeId(n) + 0xA309, n~NS & Avenue~(0,0,0,3), when = n != Owr1)  // Avenue
      add(nwmRangeId(n) + 0xAA00, n~NS & Tla3~CSE)  // Tla3
      add(nwmRangeId(n) + 0xAB00, n~NS & Ave2~CSE)  // Ave2
      add(nwmRangeId(n) + 0xAC00, n~NS & Ard3~ESC, when = n != Owr1)  // Ard3
      add(nwmRangeId(n) + 0xAC80, n~NS & Ard3~CSE, when = n != Owr1)  // Ard3
      add(nwmRangeId(n) + 0xAD00, n~NS & Owr1~CSE)  // Owr1
      add(nwmRangeId(n) + 0xAE00, n~NS & Owr3~CSE, when = (n == Ard3 || n == Owr3 || n == Nrd4))  // Owr3
      add(nwmRangeId(n) + 0xAF00, n~NS & Nrd4~CSE, when = n != Owr1)  // Nrd4
      //DxO
      add(nwmRangeId(n) + 0xB000, n~SE & Street~WC)  // Street 1
      add(nwmRangeId(n) + 0xB009, n~WN & Street~WC)  // Street 2
      add(nwmRangeId(n) + 0xB100, n~SE & Road~WC)  // Road 1
      add(nwmRangeId(n) + 0xB109, n~WN & Road~WC)  // Road 2
      add(nwmRangeId(n) + 0xB200, n~SE & Onewayroad~WC)  // Onewayroad 1
      add(nwmRangeId(n) + 0xB209, n~WN & Onewayroad~WC)  // Onewayroad 2
      add(nwmRangeId(n) + 0xB309, n~EN & Avenue~CW)  // Avenue 1a
      add(nwmRangeId(n) + 0xB300, n~SW & Avenue~CW)  // Avenue 1b
      add(nwmRangeId(n) + 0xB400, n~EN & Avenue~WC)  // Avenue 2a
      add(nwmRangeId(n) + 0xB409, n~SW & Avenue~WC)  // Avenue 2b
      add(nwmRangeId(n) + 0xBA00, n~SE & Tla3~WC)  // Tla3 1 - might need projection
      add(nwmRangeId(n) + 0xBA09, n~WN & Tla3~WC)  // Tla3 2 - might need projection
      add(nwmRangeId(n) + 0xBB00, n~SE & Ave2~WC)  // Ave2 1
      add(nwmRangeId(n) + 0xBB09, n~WN & Ave2~WC)  // Ave2 2
      add(nwmRangeId(n) + 0xBC00, n~SE & Ard3~WC, when = n != Owr1)  // Ard3 1a
      add(nwmRangeId(n) + 0xBC09, n~WN & Ard3~WC, when = n != Owr1)  // Ard3 1b
      add(nwmRangeId(n) + 0xBC80, n~SE & Ard3~CW, when = n != Owr1)  // Ard3 2a
      add(nwmRangeId(n) + 0xBC89, n~WN & Ard3~CW, when = n != Owr1)  // Ard3 2b
      add(nwmRangeId(n) + 0xBD00, n~SE & Owr1~WC)  // Owr1 1
      add(nwmRangeId(n) + 0xBD09, n~WN & Owr1~WC)  // Owr1 2
      add(nwmRangeId(n) + 0xBE00, n~SE & Owr3~WC, when = (n == Ard3 || n == Owr3 || n == Nrd4))  // Owr3 1
      add(nwmRangeId(n) + 0xBE09, n~WN & Owr3~WC, when = (n == Ard3 || n == Owr3 || n == Nrd4))  // Owr3 2
      add(nwmRangeId(n) + 0xBF00, n~SE & Nrd4~WC)  // Nrd4 1
      add(nwmRangeId(n) + 0xBF09, n~WN & Nrd4~WC)  // Nrd4 2
      //DxD
      add(nwmRangeId(n) + 0xC000, n~SE & Street~CSW)  // Street 1      
      add(nwmRangeId(n) + 0xC009, n~WN & Street~CEN)  // Street 2      
      add(nwmRangeId(n) + 0xC100, n~SE & Road~CSW)  // Road 1      
      add(nwmRangeId(n) + 0xC109, n~WN & Road~CEN)  // Road 2      
      add(nwmRangeId(n) + 0xC200, n~SE & Onewayroad~CSW)  // Onewayroad 1      
      add(nwmRangeId(n) + 0xC209, n~WN & Onewayroad~CEN)  // Onewayroad 2
      add(nwmRangeId(n) + 0xC300, n~SE & Avenue~CSW)  // Avenue 1a     
      add(nwmRangeId(n) + 0xC309, n~SE & Avenue~(3,1,0,0))  // Avenue 2a
      add(nwmRangeId(n) + 0xC400, n~SE & Avenue~CNE)  // Avenue 1b     
      add(nwmRangeId(n) + 0xC409, n~SE & Avenue~(0,0,3,1))  // Avenue 2b
      add(nwmRangeId(n) + 0xCA00, n~SE & Tla3~CSW)  // Tla3 1 - might need projection      
      add(nwmRangeId(n) + 0xCA09, n~WN & Tla3~CEN)  // Tla3 2 - might need projection     
      add(nwmRangeId(n) + 0xCB00, n~SE & Ave2~CSW)  // Ave2 1      
      add(nwmRangeId(n) + 0xCB09, n~WN & Ave2~CEN)  // Ave2 2      
      add(nwmRangeId(n) + 0xCC00, n~SE & Ard3~WSC, when = n != Owr1)  // Ard3 1a      
      add(nwmRangeId(n) + 0xCC09, n~WN & Ard3~CEN, when = n != Owr1)  // Ard3 1b      
      add(nwmRangeId(n) + 0xCC80, n~SE & Ard3~CSW, when = n != Owr1)  // Ard3 2a      
      add(nwmRangeId(n) + 0xCC89, n~WN & Ard3~NEC, when = n != Owr1)  // Ard3 2b      
      add(nwmRangeId(n) + 0xCD00, n~SE & Owr1~CSW)  // Owr1 1      
      add(nwmRangeId(n) + 0xCD09, n~WN & Owr1~CEN)  // Owr1 2      
      add(nwmRangeId(n) + 0xCE00, n~SE & Owr3~CSW, when = (n == Ard3 || n == Owr3 || n == Nrd4))  // Owr3 1      
      add(nwmRangeId(n) + 0xCE09, n~WN & Owr3~CEN, when = (n == Ard3 || n == Owr3 || n == Nrd4))  // Owr3 2      
      add(nwmRangeId(n) + 0xCF00, n~SE & Nrd4~CSW)  // Nrd4 1      
      add(nwmRangeId(n) + 0xCF09, n~WN & Nrd4~CEN)  // Nrd4 2      
    }

    // Tla3 Situations
    for (n <- Seq(Tla3)) {
      //OxO
      add(nwmRangeId(n) + 0x3000, n~WE & Street~CS)  // Street
      add(nwmRangeId(n) + 0x3100, n~WE & Road~CS)  // Road
      add(nwmRangeId(n) + 0x3200, n~WE & Onewayroad~CS)  // Onewayroad
      add(nwmRangeId(n) + 0x4300, n~WE & Avenue~NC, when = n != Owr1)  // Avenue
      add(nwmRangeId(n) + 0x3800, n~NS & Tla3~CE)  // TLA-3
      add(nwmRangeId(n) + 0x3900, n~NS & Ave2~CE)  // AVE-2
      builder.addOne((Tla3~NS).projectLeft  & Ard3~CE, IdTile(0x51003A80, R0F0, nonMirroredOnly)) //Ard-3a with Tla3
      builder.addOne((Tla3~NS).projectRight  & Ard3~EC, IdTile(0x51003A00, R0F0, nonMirroredOnly)) //Ard-3b with Tla3
      add(nwmRangeId(n) + 0x3B00, n~NS & Owr1~CE)  // OWR-1
      add(nwmRangeId(n) + 0x3D00, n~NS & Nrd4~CE, when = n != Owr1)  // NRD-4
      add(nwmRangeId(n) + 0x4800, n~NS & Tla5~EC, when = n != Owr1)  // TLA-5
      add(nwmRangeId(n) + 0x4A00, n~NS & Rd4~EC, when = n != Owr1)  // RD-4
      //OxD
      builder.addOne((Tla3~NS).projectLeft  & Street~CSE, IdTile(0x5100A000, R0F0, nonMirroredOnly)) // Street
      builder.addOne((Tla3~NS).projectRight  & Street~CSE, IdTile(0x7100A000, R0F0, mirroredOnly)) // Street
      builder.addOne((Tla3~NS).projectLeft  & Road~CSE, IdTile(0x5100A100, R0F0, nonMirroredOnly)) // Road
      builder.addOne((Tla3~NS).projectRight  & Road~CSE, IdTile(0x7100A100, R0F0, mirroredOnly)) // Road
      builder.addOne((Tla3~NS).projectLeft  & Onewayroad~CSE, IdTile(0x5100A200, R0F0, nonMirroredOnly)) // Onewayroad
      builder.addOne((Tla3~NS).projectRight  & Onewayroad~CSE, IdTile(0x7100A200, R0F0, mirroredOnly)) // Onewayroad
      builder.addOne((Tla3~NS).projectLeft  & Avenue~(0,0,0,1), IdTile(0x5100A300, R0F0, nonMirroredOnly)) // Avenue
      builder.addOne((Tla3~NS).projectRight  & Avenue~(0,0,0,1), IdTile(0x7100A300, R0F0, mirroredOnly)) // Avenue
      builder.addOne((Tla3~NS).projectLeft  & Avenue~(1,0,0,3), IdTile(0x5100A400, R0F0, nonMirroredOnly)) // Avenue
      builder.addOne((Tla3~NS).projectRight  & Avenue~(1,0,0,3), IdTile(0x7100A400, R0F0, mirroredOnly)) // Avenue
      builder.addOne((Tla3~NS).projectLeft  & Avenue~(0,0,0,3), IdTile(0x5100A309, R0F0, nonMirroredOnly)) // Avenue
      builder.addOne((Tla3~NS).projectRight  & Avenue~(0,0,0,3), IdTile(0x7100A309, R0F0, mirroredOnly)) // Avenue
      builder.addOne((Tla3~NS).projectLeft  & (Tla3~CSE).projectLeft, IdTile(0x5100AA00, R0F0, nonMirroredOnly)) // Tla3
      builder.addOne((Tla3~NS).projectRight  & (Tla3~CSE).projectRight, IdTile(0x7100AA00, R0F0, mirroredOnly)) // Tla3
      builder.addOne((Tla3~NS).projectLeft  & Ave2~CSE, IdTile(0x5100AB00, R0F0, nonMirroredOnly)) // Ave2
      builder.addOne((Tla3~NS).projectRight  & Ave2~CSE, IdTile(0x7100AB00, R0F0, mirroredOnly)) // Ave2
      builder.addOne((Tla3~NS).projectLeft  & Ard3~CSE, IdTile(0x5100AC00, R0F0, nonMirroredOnly)) // Ard3
      builder.addOne((Tla3~NS).projectRight  & Ard3~CSE, IdTile(0x7100AC00, R0F0, mirroredOnly)) // Ard3
      builder.addOne((Tla3~NS).projectLeft  & Ard3~ESC, IdTile(0x5100AC80, R0F0, nonMirroredOnly)) // Ard3
      builder.addOne((Tla3~NS).projectRight  & Ard3~ESC, IdTile(0x7100AC80, R0F0, mirroredOnly)) // Ard3
      builder.addOne((Tla3~NS).projectLeft  & Owr1~CSE, IdTile(0x5100AD00, R0F0, nonMirroredOnly)) // Owr1
      builder.addOne((Tla3~NS).projectRight  & Owr1~CSE, IdTile(0x7100AD00, R0F0, mirroredOnly)) // Owr1
      builder.addOne((Tla3~NS).projectLeft  & Nrd4~CSE, IdTile(0x5100AF00, R0F0, nonMirroredOnly)) // Nrd4
      builder.addOne((Tla3~NS).projectRight  & Nrd4~CSE, IdTile(0x7100AF00, R0F0, mirroredOnly)) // Nrd4
      //DxO
      builder.addOne((Tla3~SE).projectLeft  & Street~WC, IdTile(0x5100B000, R0F0, nonMirroredOnly)) // Street 1
      builder.addOne((Tla3~SE).projectRight  & Street~WC, IdTile(0x7100B000, R0F0, mirroredOnly)) // Street 1
      builder.addOne((Tla3~WN).projectLeft  & Street~WC, IdTile(0x5100B009, R0F0, nonMirroredOnly)) // Street 2
      builder.addOne((Tla3~WN).projectRight  & Street~WC, IdTile(0x7100B009, R0F0, mirroredOnly)) // Street 2
      builder.addOne((Tla3~SE).projectLeft  & Road~WC, IdTile(0x5100B100, R0F0, nonMirroredOnly)) // Road 1
      builder.addOne((Tla3~SE).projectRight  & Road~WC, IdTile(0x7100B100, R0F0, mirroredOnly)) // Road 1
      builder.addOne((Tla3~WN).projectLeft  & Road~WC, IdTile(0x5100B109, R0F0, nonMirroredOnly)) // Road 2
      builder.addOne((Tla3~WN).projectRight  & Road~WC, IdTile(0x7100B109, R0F0, mirroredOnly)) // Road 2
      builder.addOne((Tla3~SE).projectLeft  & Onewayroad~WC, IdTile(0x5100B200, R0F0, nonMirroredOnly)) // Onewayroad 1
      builder.addOne((Tla3~SE).projectRight  & Onewayroad~WC, IdTile(0x7100B200, R0F0, mirroredOnly)) // Onewayroad 1
      builder.addOne((Tla3~WN).projectLeft  & Onewayroad~WC, IdTile(0x5100B209, R0F0, nonMirroredOnly)) // Onewayroad 2
      builder.addOne((Tla3~WN).projectRight  & Onewayroad~WC, IdTile(0x7100B209, R0F0, mirroredOnly)) // Onewayroad 2
      builder.addOne((Tla3~EN).projectLeft  & Avenue~CW, IdTile(0x5100B309, R0F0, nonMirroredOnly)) // Avenue 2
      builder.addOne((Tla3~EN).projectRight  & Avenue~CW, IdTile(0x7100B309, R0F0, mirroredOnly)) // Avenue 2
      builder.addOne((Tla3~SW).projectLeft  & Avenue~CW, IdTile(0x5100B300, R0F0, nonMirroredOnly)) // Avenue 2
      builder.addOne((Tla3~SW).projectRight  & Avenue~CW, IdTile(0x7100B300, R0F0, mirroredOnly)) // Avenue 2
      builder.addOne((Tla3~EN).projectLeft  & Avenue~WC, IdTile(0x5100B400, R0F0, nonMirroredOnly)) // Avenue 2
      builder.addOne((Tla3~EN).projectRight  & Avenue~WC, IdTile(0x5100B400, R0F0, mirroredOnly)) // Avenue 2
      builder.addOne((Tla3~SW).projectLeft  & Avenue~WC, IdTile(0x7100B409, R0F0, nonMirroredOnly)) // Avenue 2
      builder.addOne((Tla3~SW).projectRight  & Avenue~WC, IdTile(0x7100B409, R0F0, mirroredOnly)) // Avenue 2
      builder.addOne((Tla3~SE).projectLeft  & (Tla3~WC).projectLeft, IdTile(0x5100BA00, R0F0, nonMirroredOnly)) // Tla3 1
      builder.addOne((Tla3~SE).projectRight  & (Tla3~WC).projectRight, IdTile(0x7100BA00, R0F0, mirroredOnly)) // Tla3 1
      builder.addOne((Tla3~WN).projectLeft  & (Tla3~WC).projectLeft, IdTile(0x5100BA09, R0F0, nonMirroredOnly)) // Tla3 2
      builder.addOne((Tla3~WN).projectRight  & (Tla3~WC).projectRight, IdTile(0x7100BA09, R0F0, mirroredOnly)) // Tla3 2
      builder.addOne((Tla3~SE).projectLeft  & Ave2~WC, IdTile(0x5100BB00, R0F0, nonMirroredOnly)) // Ave2 1
      builder.addOne((Tla3~SE).projectRight  & Ave2~WC, IdTile(0x7100BB00, R0F0, mirroredOnly)) // Ave2 1
      builder.addOne((Tla3~WN).projectLeft  & Ave2~WC, IdTile(0x5100BB09, R0F0, nonMirroredOnly)) // Ave2 2
      builder.addOne((Tla3~WN).projectRight  & Ave2~WC, IdTile(0x7100BB09, R0F0, mirroredOnly)) // Ave2 2
      builder.addOne((Tla3~SE).projectLeft  & Ard3~WC, IdTile(0x5100BC00, R0F0, nonMirroredOnly)) // Ard3 1a
      builder.addOne((Tla3~SE).projectRight  & Ard3~WC, IdTile(0x7100BC00, R0F0, mirroredOnly)) // Ard3 1a
      builder.addOne((Tla3~WN).projectLeft  & Ard3~WC, IdTile(0x5100BC09, R0F0, nonMirroredOnly)) // Ard3 2a
      builder.addOne((Tla3~WN).projectRight  & Ard3~WC, IdTile(0x7100BC09, R0F0, mirroredOnly)) // Ard3 2a
      builder.addOne((Tla3~SE).projectLeft  & Ard3~CW, IdTile(0x5100BC80, R0F0, nonMirroredOnly)) // Ard3 1a
      builder.addOne((Tla3~SE).projectRight  & Ard3~CW, IdTile(0x7100BC80, R0F0, mirroredOnly)) // Ard3 1a
      builder.addOne((Tla3~WN).projectLeft  & Ard3~CW, IdTile(0x5100BC89, R0F0, nonMirroredOnly)) // Ard3 2a
      builder.addOne((Tla3~WN).projectRight  & Ard3~CW, IdTile(0x7100BC89, R0F0, mirroredOnly)) // Ard3 2a
      builder.addOne((Tla3~SE).projectLeft  & Owr1~WC, IdTile(0x5100BD00, R0F0, nonMirroredOnly)) // Owr1 1a
      builder.addOne((Tla3~SE).projectRight  & Owr1~WC, IdTile(0x7100BD00, R0F0, mirroredOnly)) // Owr1 1a
      builder.addOne((Tla3~WN).projectLeft  & Owr1~WC, IdTile(0x5100BD09, R0F0, nonMirroredOnly)) // Owr1 2a
      builder.addOne((Tla3~WN).projectRight  & Owr1~WC, IdTile(0x7100BD09, R0F0, mirroredOnly)) // Owr1 2a
      builder.addOne((Tla3~SE).projectLeft  & Nrd4~WC, IdTile(0x5100BF00, R0F0, nonMirroredOnly)) // Nrd4 1a
      builder.addOne((Tla3~SE).projectRight  & Nrd4~WC, IdTile(0x7100BF00, R0F0, mirroredOnly)) // Nrd4 1a
      builder.addOne((Tla3~WN).projectLeft  & Nrd4~WC, IdTile(0x5100BF09, R0F0, nonMirroredOnly)) // Nrd4 2a
      builder.addOne((Tla3~WN).projectRight  & Nrd4~WC, IdTile(0x7100BF09, R0F0, mirroredOnly)) // Nrd4 2a
      //DxD
      builder.addOne((Tla3~SE).projectLeft  & Street~CSW, IdTile(0x5100C000, R0F0, nonMirroredOnly)) // Street 1
      builder.addOne((Tla3~SE).projectRight  & Street~CSW, IdTile(0x7100C000, R0F0, mirroredOnly)) // Street 1
      builder.addOne((Tla3~WN).projectLeft  & Street~CEN, IdTile(0x5100C009, R0F0, nonMirroredOnly)) // Street 2
      builder.addOne((Tla3~WN).projectRight  & Street~CEN, IdTile(0x7100C009, R0F0, mirroredOnly)) // Street 2
      builder.addOne((Tla3~SE).projectLeft  & Road~CSW, IdTile(0x5100C100, R0F0, nonMirroredOnly)) // Road 1
      builder.addOne((Tla3~SE).projectRight  & Road~CSW, IdTile(0x7100C100, R0F0, mirroredOnly)) // Road 1
      builder.addOne((Tla3~WN).projectLeft  & Road~CEN, IdTile(0x5100C109, R0F0, nonMirroredOnly)) // Road 2
      builder.addOne((Tla3~WN).projectRight  & Road~CEN, IdTile(0x7100C109, R0F0, mirroredOnly)) // Road 2
      builder.addOne((Tla3~SE).projectLeft  & Onewayroad~CSW, IdTile(0x5100C200, R0F0, nonMirroredOnly)) // Onewayroad 1
      builder.addOne((Tla3~SE).projectRight  & Onewayroad~CSW, IdTile(0x7100C200, R0F0, mirroredOnly)) // Onewayroad 1
      builder.addOne((Tla3~WN).projectLeft  & Onewayroad~CEN, IdTile(0x5100C209, R0F0, nonMirroredOnly)) // Onewayroad 2
      builder.addOne((Tla3~WN).projectRight  & Onewayroad~CEN, IdTile(0x7100C209, R0F0, mirroredOnly)) // Onewayroad 2
      // add Avenue 
      builder.addOne((Tla3~SE).projectLeft  & (Tla3~CSW).projectLeft, IdTile(0x5100CA00, R0F0, nonMirroredOnly)) // Tla3 1
      builder.addOne((Tla3~SE).projectRight  & (Tla3~CSW).projectRight, IdTile(0x7100CA00, R0F0, mirroredOnly)) // Tla3 1
      builder.addOne((Tla3~WN).projectLeft  & (Tla3~CEN).projectLeft, IdTile(0x5100CA09, R0F0, nonMirroredOnly)) // Tla3 2
      builder.addOne((Tla3~WN).projectRight  & (Tla3~CEN).projectRight, IdTile(0x7100CA09, R0F0, mirroredOnly)) // Tla3 2
      builder.addOne((Tla3~SE).projectLeft  & Ave2~CSW, IdTile(0x5100CB00, R0F0, nonMirroredOnly)) // Ave2 1
      builder.addOne((Tla3~SE).projectRight  & Ave2~CSW, IdTile(0x7100CB00, R0F0, mirroredOnly)) // Ave2 1
      builder.addOne((Tla3~WN).projectLeft  & Ave2~CEN, IdTile(0x5100CB09, R0F0, nonMirroredOnly)) // Ave2 2
      builder.addOne((Tla3~WN).projectRight  & Ave2~CEN, IdTile(0x7100CB09, R0F0, mirroredOnly)) // Ave2 2
      builder.addOne((Tla3~SE).projectLeft  & Ard3~CSW, IdTile(0x5100CC00, R0F0, nonMirroredOnly)) // Ard3 1a
      builder.addOne((Tla3~SE).projectRight  & Ard3~CSW, IdTile(0x7100CC00, R0F0, mirroredOnly)) // Ard3 1a
      builder.addOne((Tla3~WN).projectLeft  & Ard3~CEN, IdTile(0x5100CC09, R0F0, nonMirroredOnly)) // Ard3 2a
      builder.addOne((Tla3~WN).projectRight  & Ard3~CEN, IdTile(0x7100CC09, R0F0, mirroredOnly)) // Ard3 2a
      builder.addOne((Tla3~SE).projectLeft  & Ard3~WSC, IdTile(0x5100CC80, R0F0, nonMirroredOnly)) // Ard3 1b
      builder.addOne((Tla3~SE).projectRight  & Ard3~WSC, IdTile(0x7100CC80, R0F0, mirroredOnly)) // Ard3 1b
      builder.addOne((Tla3~WN).projectLeft  & Ard3~NEC, IdTile(0x5100CC89, R0F0, nonMirroredOnly)) // Ard3 2b
      builder.addOne((Tla3~WN).projectRight  & Ard3~NEC, IdTile(0x7100CC89, R0F0, mirroredOnly)) // Ard3 2b
      builder.addOne((Tla3~SE).projectLeft  & Owr1~CSW, IdTile(0x5100CD00, R0F0, nonMirroredOnly)) // Owr1 1
      builder.addOne((Tla3~SE).projectRight  & Owr1~CSW, IdTile(0x7100CD00, R0F0, mirroredOnly)) // Owr1 1
      builder.addOne((Tla3~WN).projectLeft  & Owr1~CEN, IdTile(0x5100CD09, R0F0, nonMirroredOnly)) // Owr1 2
      builder.addOne((Tla3~WN).projectRight  & Owr1~CEN, IdTile(0x7100CD09, R0F0, mirroredOnly)) // Owr1 2
      builder.addOne((Tla3~SE).projectLeft  & Nrd4~CSW, IdTile(0x5100CF00, R0F0, nonMirroredOnly)) // Nrd4 1
      builder.addOne((Tla3~SE).projectRight  & Nrd4~CSW, IdTile(0x7100CF00, R0F0, mirroredOnly)) // Nrd4 1
      builder.addOne((Tla3~WN).projectLeft  & Nrd4~CEN, IdTile(0x5100CF09, R0F0, nonMirroredOnly)) // Nrd4 2
      builder.addOne((Tla3~WN).projectRight  & Nrd4~CEN, IdTile(0x7100CF09, R0F0, mirroredOnly)) // Nrd4 2
   }

    // Ard3-Specific Situations
    for (n <- Seq(Ard3)) {
      //OxO
      add(nwmRangeId(n) + 0x3080, n~EW & Street~CS)  // Street
      add(nwmRangeId(n) + 0x3180, n~EW & Road~CS)  // Road
      add(nwmRangeId(n) + 0x3280, n~EW & Onewayroad~CS)  // Onewayroad
      add(nwmRangeId(n) + 0x4380, n~EW & Avenue~NC)  // Avenue
      add(nwmRangeId(n) + 0x3880, n~SN & Tla3~CE)  // TLA-3b
      add(nwmRangeId(n) + 0x3980, n~SN & Ave2~CE)  // AVE-2b
      add(nwmRangeId(n) + 0x3A80, n~SN & Ard3~CE)  // ARD-3b
      add(nwmRangeId(n) + 0x3B80, n~SN & Owr1~CE)  // OWR-1b
      add(nwmRangeId(n) + 0x3C80, n~SN & Owr3~CE)  // OWR-3b
      add(nwmRangeId(n) + 0x3D80, n~SN & Nrd4~CE)  // NRD-4b
      add(nwmRangeId(n) + 0x4880, n~SN & Tla5~EC)  // TLA-5b
      add(nwmRangeId(n) + 0x4A80, n~SN & Rd4~EC)  // RD-4b
      add(nwmRangeId(n) + 0x4B80, n~SN & Rd6~EC)  // RD-6b
      add(nwmRangeId(n) + 0x4C80, n~SN & Ave6~EC)  // AVE-6b
      add(nwmRangeId(n) + 0x4C89, n~SN & Tla7m~EC)  // TLA-Mb
      add(nwmRangeId(n) + 0x4D89, n~SN & Ave6m~EC)  // AVE-Mb
    }

    //Multi-Tile Networks
    for (n <- Seq(Tla5, Owr4, Owr4m, Owr5, Rd4, Rd6, Ave6, Ave8)) {
      add(nwmRangeId(n) + 0x3000, n~EW & Street~NC)  // Street Short
      add(nwmRangeId(n) + 0x3080, n~WE & Street~NC)  // Street Long
      add(nwmRangeId(n) + 0x3100, n~EW & Road~NC)  // Road Short
      add(nwmRangeId(n) + 0x3180, n~WE & Road~NC)  // Road Long
      add(nwmRangeId(n) + 0x3200, n~EW & Onewayroad~NC)  // Onewayroad Short
      add(nwmRangeId(n) + 0x3280, n~WE & Onewayroad~NC)  // Onewayroad Long
      add(nwmRangeId(n) + 0x4300, n~EW & Avenue~NC)  // Avenue Short
      add(nwmRangeId(n) + 0x4400, n~WE & Avenue~NC)  // Avenue Long
      add(nwmRangeId(n) + 0x3800, n~EW & Tla3~NC)  // TLA-3 Short
      add(nwmRangeId(n) + 0x3880, n~WE & Tla3~NC)  // TLA-3 Long
      add(nwmRangeId(n) + 0x3900, n~EW & Ave2~NC)  // AVE-2 Short
      add(nwmRangeId(n) + 0x3980, n~WE & Ave2~NC)  // AVE-2 Long
      add(nwmRangeId(n) + 0x3A00, n~EW & Ard3~NC)  // ARD-3 Short
      add(nwmRangeId(n) + 0x3A80, n~WE & Ard3~NC)  // ARD-3 Long
      add(nwmRangeId(n) + 0x3B00, n~EW & Owr1~NC)  // OWR-1 Short
      add(nwmRangeId(n) + 0x3B80, n~WE & Owr1~NC)  // OWR-1 Long
      add(nwmRangeId(n) + 0x3C00, n~EW & Owr3~NC)  // OWR-3 Short - restrict to Owr4/m, Owr5, Rd6, Ave6, Ave8
      add(nwmRangeId(n) + 0x3C80, n~WE & Owr3~NC)  // OWR-3 Long
      add(nwmRangeId(n) + 0x3D00, n~EW & Nrd4~NC)  // NRD-4 Short
      add(nwmRangeId(n) + 0x3D80, n~WE & Nrd4~NC)  // NRD-4 Long
      add(nwmRangeId(n) + 0x3E00, n~WE & Tla5~NC)  // TLA-5 Long
      add(nwmRangeId(n) + 0x3F00, n~WE & Owr4~NC)  // OWR-4 Long
      add(nwmRangeId(n) + 0x3700, n~WE & Owr4m~NC)  // OWR-4m Long
      add(nwmRangeId(n) + 0x4000, n~WE & Owr5~NC)  // OWR-5 Long
      add(nwmRangeId(n) + 0x4100, n~WE & Rd4~NC)  // RD-4 Long
      add(nwmRangeId(n) + 0x4200, n~WE & Rd6~NC)  // RD-6 Long
      add(nwmRangeId(n) + 0x4800, n~WE & Ave6~NC)  // AVE-6 Long
      add(nwmRangeId(n) + 0x4880, n~WE & Tla7m~NC)  // TLA-M Long
      add(nwmRangeId(n) + 0x4900, n~WE & Ave8~NC)  // AVE-8 Long
      add(nwmRangeId(n) + 0x4980, n~WE & Ave6m~NC)  // AVE-M Long
    }

    //NWM-End
    //Single-Tile Networks
    for (n <- Seq(Tla3, Ave2, Ard3, Owr1, Owr3, Nrd4)) {
      //OxO
      add(nwmRangeId(n) + 0x4000, n~CE & Street~NS)  // Street
      add(nwmRangeId(n) + 0x4100, n~CE & Road~NS)  // Road
      add(nwmRangeId(n) + 0x4200, n~CE & Onewayroad~NS)  // Onewayroad - dummy for OWR-3
      add(nwmRangeId(n) + 0x3300, n~CE & Avenue~SN, when = n != Owr3)  // Avenue Short - disabled for OWR-3
      add(nwmRangeId(n) + 0x3400, n~WC & Avenue~SN)  // Avenue Long
      //OxD
      add(nwmRangeId(n) + 0xA500, n~NC & Street~WS)  // Street 1
      add(nwmRangeId(n) + 0xA509, n~SC & Street~WS)  // Street 2
      add(nwmRangeId(n) + 0xA600, n~NC & Road~WS)  // Road 1
      add(nwmRangeId(n) + 0xA609, n~SC & Road~WS)  // Road 2
      add(nwmRangeId(n) + 0xA700, n~NC & Onewayroad~WS)  // Onewayroad 1 - dummy for OWR-3
      add(nwmRangeId(n) + 0xA709, n~SC & Onewayroad~WS)  // Onewayroad 2 - dummy for OWR-3
      add(nwmRangeId(n) + 0xA800, n~CW & Avenue~ES, when = n != Owr3)  // Avenue Short 1 - disabled for OWR-3
      add(nwmRangeId(n) + 0xA809, n~CW & Avenue~SharedDiagRight, when = n != Owr3)  // Avenue Short 2 - disabled for OWR-3
      add(nwmRangeId(n) + 0xA900, n~SC & Avenue~SW)  // Avenue Long 1
      // add(nwmRangeId(n) + 0xA909, n~CS & Avenue~SharedDiagLeft)  // Avenue Long 1
      //DxO
      add(nwmRangeId(n) + 0xB500, n~CSE & Street~NS)  // Street
      add(nwmRangeId(n) + 0xB600, n~CSE & Road~NS)  // Road
      add(nwmRangeId(n) + 0xB700, n~CSE & Onewayroad~NS) //, when = n != Ard3 && n != Owr3 && n != Nrd4)  // Onewayroad - dummy for OWR-3
      // builder.addOne(Ard3~CSE  & Onewayroad~NS, IdTile(0x5102B700, R0F0, nonMirroredOnly)) // Onewayroad - dummy for OWR-3
      // builder.addOne(Ard3~CSE  & Onewayroad~SN, IdTile(0x7102B700, R0F0, mirroredOnly)) // Onewayroad - dummy for OWR-3
      // builder.addOne(Owr3~CSE  & Onewayroad~NS, IdTile(0x5104B700, R0F0, nonMirroredOnly)) // Onewayroad - dummy for OWR-3
      // builder.addOne(Owr3~CSE  & Onewayroad~SN, IdTile(0x7104B700, R0F0, mirroredOnly)) // Onewayroad - dummy for OWR-3
      // builder.addOne(Nrd4~CSE  & Onewayroad~NS, IdTile(0x5105B700, R0F0, nonMirroredOnly)) // Onewayroad - dummy for OWR-3
      // builder.addOne(Nrd4~CSE  & Onewayroad~SN, IdTile(0x7105B700, R0F0, mirroredOnly)) // Onewayroad - dummy for OWR-3
      add(nwmRangeId(n) + 0xB800, n~CSE & Avenue~SN, when = n != Owr3)  // Avenue Short - disabled for OWR-3
      add(nwmRangeId(n) + 0xB900, n~SEC & Avenue~EW)  // Avenue Long 1
      add(nwmRangeId(n) + 0xB909, n~WNC & Avenue~EW)  // Avenue Long 2
    }

    //Multi-Tile Networks
    //OWR-5 does not have any valid T-ints with Maxis networks - just itself, RD-6, and the Triple-Tile networks
    for (n <- Seq(Tla5, Owr4, Owr4m, Rd4, Rd6, Ave6, Ave8, Tla7m, Ave6m)) {
      add(nwmRangeId(n) + 0x3400, n~NC & Avenue~WE, when = n != Owr5)  // Avenue Long
      add(nwmRangeId(n) + 0x4500, n~CE & Road~NS)  // Road - dummy for OWR-5/RD-6/AVE-6/AVE-8
      add(nwmRangeId(n) + 0x4600, n~CE & Onewayroad~NS)  // Onewayroad - dummy for OWR-5/RD-6/AVE-6/AVE-8
      add(nwmRangeId(n) + 0x4700, n~CE & Street~NS, when = (n == Tla5 || n == Rd4))  // Street
      // add(nwmRangeId(n) + 0x3300, n~NC & Avenue~EW, when = (n == Tla5 || n == Rd4))  // Avenue Short - not currently supported for multi-tile networks, could add for Tla5 and Rd4
    }

    builder.result()
  }
}
