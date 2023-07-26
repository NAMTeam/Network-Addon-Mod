package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, RotFlip._, Flags._

trait DoubleSegResolver {

  protected case class DoubleProperty(
      val orthDiagOffset: Int,
      val majorSegReversed: Boolean,
      val minorSegReversed: Boolean,
      val majKind: Flag.Kind.Value,
      val minKind: Flag.Kind.Value,
      val rf: RotFlip) extends Ordered[DoubleProperty] {

    def compare(that: DoubleProperty): Int = {
      if (this.orthDiagOffset != that.orthDiagOffset) this.orthDiagOffset - that.orthDiagOffset
      else if (this.majorSegReversed != that.majorSegReversed) if (this.majorSegReversed) 1 else -1
      else if (this.minorSegReversed != that.minorSegReversed) if (this.minorSegReversed) 1 else -1
      else if (this.rf.flip != that.rf.flip) this.rf.flip - that.rf.flip
      else if (this.rf.rot != that.rf.rot) this.rf.rot - that.rf.rot
      else 0
    }
  }

  /** contains mapping of flags of major and minor network (orth and diag only)
    * to DoubleProperty(iid offset, major rev, minor rev, rotflip).
    */
  protected val doubleProps: Map[(Flags, Flags), DoubleProperty] = {
    val tmp = scala.collection.mutable.Map.empty[(Flags, Flags), DoubleProperty]

    import Flag.Kind._
    def flipKind(k: Flag.Kind.Value, rf: RotFlip): Flag.Kind.Value = if (k == Default || !rf.flipped) k else k match {
      case LeftSpin => RightSpin
      case RightSpin => LeftSpin
    }
    def fill(tup1: IntFlags, tup1Rev: IntFlags, tup2: IntFlags, tup2Rev: IntFlags, offset: Int): Unit = {
      for {
        n1 <- Seq(Dirtroad, Mis) // these networks only serve for generating symm and asymm flags
        n2 <- Seq(L1Rhw2, L1Mis)
        (seg2, minRev) <- Seq(n2~tup2, n2~tup2Rev) zip Seq(false, true)
        (seg1, majRev) <- Seq(n1~tup1, n1~tup1Rev) zip Seq(false, true)
        (flags1, kind1) <- Seq(seg1.flags, seg1.flags.spinLeft, seg1.flags.spinRight) zip Seq(Default, LeftSpin, RightSpin)
        (flags2, kind2) <- Seq(seg2.flags, seg2.flags.spinLeft, seg2.flags.spinRight) zip Seq(Default, LeftSpin, RightSpin)
        rf <- Tile(Set(seg1, seg2)).representations
        prop = new DoubleProperty(offset, majRev, minRev, flipKind(kind1, rf), flipKind(kind2, rf), rf)
        // special cases for shared-tile diagonals
        f1 <- if (offset >= 0x6000 && majRev) Seq(flags1, (n1~SharedDiagRight).flags) else Seq(flags1)
        f2 <- if (offset % 0x6000 != 0 && minRev) Seq(flags2, (n2~SharedDiagLeft).flags) else Seq(flags2)
      } /*do*/ {
        tmp.getOrElseUpdate((f1 * rf, f2 * rf), prop)
      }
    }

    fill(NS, SN, EW, WE, 0x0000)
    fill(NS, SN, SW, WS, 0x3000)
    fill(ES, SE, EW, WE, 0x6000)
    fill(ES, SE, SW, WS, 0x9000)
    tmp.toMap
  }

  /** A segment is greater, if it has higher priority, thus dominates the other
    * and determines the main ID range, like 0x5713#### for instance. The other
    * segment determines the piece ID.
    */
  protected def greater(a: Segment, b: Segment): Boolean = {
    if (a.network.isRhw != b.network.isRhw) {
      a.network.isRhw
    } else if (a.network.isNwm != b.network.isNwm) {
      a.network.isNwm
    } else if (a.network.height != b.network.height) {
      a.network.height > b.network.height
    } else if (a.network != b.network) {
      a.network > b.network // both RHW or both NWM with same height
    } else {
      doubleProps(a.flags, b.flags) <= doubleProps(b.flags, a.flags) // doubleProps should always contain the flags, or something is wrong
    }
  }
}

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
    Tla7m    -> 0x3300, Ave8     -> 0x3400, Ave6m    -> 0x3500)

}

class RhwResolver extends IdResolver with RhwSingleSegResolver with DoubleSegResolver {

  def isSingleTileRhw(n: Network): Boolean = n.isRhw && n <= L4Rhw6s
  val isRhwShoulder = Network.ValueSet(
    Rhw8s, L1Rhw8s, L2Rhw8s, Rhw10s, L1Rhw10s, L2Rhw10s, Rhw12s, L1Rhw12s, L2Rhw12s,
    Rhw6c, L1Rhw6c, L2Rhw6c, Rhw8c, L1Rhw8c, L2Rhw8c, Rhw10c, L1Rhw10c, L2Rhw10c)

  /** is defined for all tiles that contain an RHW network */
  def isDefinedAt(t: Tile): Boolean = t.segs.exists(_.network.isRhw)

  def apply(tile: Tile): IdTile = {
    if (!isDefinedAt(tile)) {
      throw new MatchError(tile)
    } else if (tile.segs.size == 2) {
      val List(maj, min) = tile.segs.toList.sortWith(greater)
      assert(maj.network.isRhw)
      doubleProps.get(maj.flags, min.flags) match {
        case Some(prop) =>
          var id = RhwResolver.rhwRangeId(maj.network) + RhwResolver.rhwPieceId(min.network) + prop.orthDiagOffset
          val strangeFlag = isRhwShoulder(maj.network) && (prop.orthDiagOffset >= 0x6000) // strange anomalie for shoulder networks
          val strangeFlagMin = isRhwShoulder(min.network) && (prop.orthDiagOffset % 0x6000 >= 0x3000) // strange anomalie
          if (prop.majorSegReversed ^ strangeFlag) // TODO strange behaviour for shoulder networks
            id += (if (isSingleTileRhw(maj.network)) 0x80 else 0x40)
          if (prop.minorSegReversed ^ strangeFlagMin) // TODO consider changing the IDs to 0x05
            id += (if (maj.network.height == 0 && min.network.height == 0) 0x09 else 0x05)
          if (maj.network.height == 0 && min.network == Str)
            id += 4  // Str has offset 0x09 instead of 0x05
          IdTile(id, prop.rf)
        case None => //??? // TODO T intersections etc. still missing
          throw new UnsupportedOperationException(tile.toString)
      }
    } else if (tile.segs.size == 1) {
      resolveSegment(tile.segs.head)
    } else {
      ??? // three-level overpasses currently left out
    }
  }
}
