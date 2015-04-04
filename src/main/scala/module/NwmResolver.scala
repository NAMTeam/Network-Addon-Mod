package metarules.module

import metarules.meta._
import Network._
import RotFlip._
import Flags._


class NwmResolver extends IdResolver with NwmSingleSegResolver with DoubleSegResolver {

  val isSingleTileNwm = Set(Tla3, Ave2, Ard3, Owr1, Owr3, Nrd4)

  val nwmRangeId = {
    val tmp = NwmNetworks.filter(n => n != Ave6m && n != Tla7m).toSeq.zipWithIndex map { case (n, i) =>
      (n, 0x51700000 + i * 0x10000)
    }
    (tmp :+ ((Tla7m, 0x517B0080)) :+ ((Ave6m, 0x517C0080))).toMap
  }.lift

  val nwmPieceId = Map(
    Street        -> 0x1000,
    Road          -> 0x1100,
    Onewayroad    -> 0x1200,
    Avenue        -> 0x1300,
    Highway       -> 0x1420,
    Groundhighway -> 0x1400,
    Rail          -> 0x1500,
    Str           -> 0x1505,
//    Ttr           -> 0x1600,
//    Qtr           -> 0x1605,
    Lightrail     -> 0x1720,
    Glr1          -> 0x1700,
    Glr2          -> 0x1800,
    Glr3          -> 0x1705,
    Glr4          -> 0x1805,
    Monorail      -> 0x1920,
    Hsr           -> 0x1905,
    L2Hsr         -> 0x1925,
    Tla3          -> 0x1A00,
    Ave2          -> 0x1B00,
    Ard3          -> 0x1C00,
    Nrd4          -> 0x1D00,
    Owr1          -> 0x1E00,
    Owr3          -> 0x1F00,
    Tla5          -> 0x2000,
    Rd4           -> 0x2100,
    Rd6           -> 0x2200,
    Owr4          -> 0x2300,
    Owr5          -> 0x2400,
    Ave6          -> 0x2500,
    Tla7m         -> 0x2600,
    Ave8          -> 0x2700,
    Ave6m         -> 0x2800).lift

  /** is defined for all tiles that do not contain RHW, but NWM */
  def isDefinedAt(t: Tile): Boolean = !t.segs.exists(_.network.isRhw) && t.segs.exists(_.network.isNwm)

  val leftHeadedMappedRepr: Group.QuotientGroup => Set[RotFlip] = _.filter(!_.flipped)
  val rightHeadedMappedRepr: Group.QuotientGroup => Set[RotFlip] = _.filter(_.flipped)

  def apply(tile: Tile): IdTile = {
    if (!isDefinedAt(tile)) {
      throw new MatchError(tile)
    } else if (tile.segs.size == 2) {
      val List(maj, min) = tile.segs.toList.sortWith(greater)
      assert(maj.network.isNwm)
      doubleProps.get(maj.flags, min.flags) match {
        case Some(prop) =>
          var id = nwmRangeId(maj.network).get + nwmPieceId(min.network).get + prop.orthDiagOffset
          if (prop.majorSegReversed)
            id += (if (isSingleTileNwm(maj.network)) 0x80 else 0x40)
          if (prop.minorSegReversed)
            id += (/*if (maj.network.height == 0 && min.network.height == 0) 0x09 else*/ 0x05)
          if (prop.majKind == Flag.Kind.LeftHeaded || prop.minKind == Flag.Kind.LeftHeaded ||
             (prop.majKind == Flag.Kind.RightHeaded || prop.minKind == Flag.Kind.RightHeaded) &&
              tile.symmetries.exists(_.flipped)) // <-- does not have right-headed ID
            IdTile(id, prop.rf, leftHeadedMappedRepr)
          else if (prop.majKind == Flag.Kind.RightHeaded || prop.minKind == Flag.Kind.RightHeaded)
            IdTile(id + 1, prop.rf, rightHeadedMappedRepr) // TODO find suitable ID
          else
            IdTile(id, prop.rf)
        case None => //??? // TODO T intersections etc. still missing
          throw new UnsupportedOperationException(tile.toString)
      }
    } else if (tile.segs.size == 1) {
      resolveNwmSegment(tile.segs.head)
    } else {
      ??? // TODO
    }
  }
}
