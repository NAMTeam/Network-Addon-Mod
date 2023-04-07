package metarules.module

import metarules.meta._
import Network._
import RotFlip._
import Flags._
import NetworkProperties.{isTripleTile, leftHeadedMappedRepr, rightHeadedMappedRepr}


class NwmResolver extends IdResolver with NwmSingleSegResolver with DoubleSegResolver {

  val isSingleTileNwm = Set(Tla3, Ave2, Ard3, Owr1, Owr3, Nrd4)

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
    Tla7m         -> 0x51200080,
    Ave8          -> 0x51210000,
    Ave6m         -> 0x51210080).lift

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

    Str           -> 0x0F00,

    Tla3          -> 0x1000,
    Ave2          -> 0x1100,
    Ard3          -> 0x1200,
    Owr1          -> 0x1300,
    Owr3          -> 0x1400,
    Nrd4          -> 0x1500,

    L2Hsr         -> 0x1700,

    Tla5          -> 0x1800,
    Owr4          -> 0x1900,
    Owr5          -> 0x1A00,
    Rd4           -> 0x1B00,
    Rd6           -> 0x1C00,

    Ave6          -> 0x1D00,
    Tla7m         -> 0x1D0A,
    Ave8          -> 0x1E00,
    Ave6m         -> 0x1E0A).lift
    // currently not defined:
    // Glr3          -> 0x....,
    // Glr4          -> 0x....,
    // Hsr           -> 0x....,

  /** is defined for all tiles that do not contain RHW, but NWM */
  def isDefinedAt(t: Tile): Boolean = !t.segs.exists(_.network.isRhw) && t.segs.exists(_.network.isNwm)

  def apply(tile: Tile): IdTile = {
    if (!isDefinedAt(tile)) {
      throw new MatchError(tile)
    } else if (tile.segs.size == 2) {
      val List(maj0, min0) = tile.segs.toList.sortWith(greater)
      assert(maj0.network.isNwm)
      val (maj, min) = if (min0.network.isNwm && doubleProps.get(maj0.flags, min0.flags).exists(_.orthDiagOffset == 0x6000)) {
        (min0, maj0)  // switch DxO to OxD if both are NWM networks (i.e. orthogonal NWM is primary network)
      } else {
        (maj0, min0)
      }
      doubleProps.get(maj.flags, min.flags) match {
        case Some(prop) =>
          val pieceOffset = prop.orthDiagOffset match {
            case 0x0000 => 0x1000  // OxO
            case 0x3000 => 0x5000  // OxD
            case 0x6000 =>
              assert(!min.network.isNwm) // otherwise this would be covered by OxD
              0x7000  // DxO
            case 0x9000 => 0x8000  // DxD
          }
          var id = nwmRangeId(maj.network).get + nwmPieceId(min.network).get + pieceOffset
          if (prop.majorSegReversed)
            id += (if (isTripleTile(maj.network)) 0x40 else 0x80)  // TODO revise IID scheme to avoid 0x40 for wealthing support
          if (prop.minorSegReversed)
            id += 0x05
          if (id % 0x10 != 0 && (maj.network.height == 0 || min.network.height == 0))
            id += 0x4  // map 8th digit 5 to 9, A to E
          if (prop.majKind == Flag.Kind.LeftHeaded || prop.minKind == Flag.Kind.LeftHeaded ||
             (prop.majKind == Flag.Kind.RightHeaded || prop.minKind == Flag.Kind.RightHeaded) &&
              tile.symmetries.exists(_.flipped)) // <-- does not have right-headed ID
            IdTile(id, prop.rf, leftHeadedMappedRepr)
          else if (prop.majKind == Flag.Kind.RightHeaded || prop.minKind == Flag.Kind.RightHeaded)
            IdTile(id + 0x20000000, prop.rf, rightHeadedMappedRepr) // TODO find suitable ID
          else
            IdTile(id, prop.rf)
        case None => //??? // TODO T intersections etc. still missing
          throw new UnsupportedOperationException(tile.toString)
      }
    } else if (tile.segs.size == 1) {
      resolveNwmSegment(tile.segs.head)
    } else {
      throw new NotImplementedError(tile.toString) // ??? // TODO
    }
  }
}
