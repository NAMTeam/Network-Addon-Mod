package metarules.module

import metarules.meta._
import Network._
import RotFlip._
import Flags._


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
      case LeftHeaded => RightHeaded
      case RightHeaded => LeftHeaded
    }
    def fill(tup1: IntFlags, tup1Rev: IntFlags, tup2: IntFlags, tup2Rev: IntFlags, offset: Int) {
      for {
        n1 <- Seq(Dirtroad, Mis) // these networks only serve for generating symm and asymm flags
        n2 <- Seq(L1Rhw2, L1Mis)
        (seg2, minRev) <- Seq(n2~tup2, n2~tup2Rev) zip Seq(false, true)
        (seg1, majRev) <- Seq(n1~tup1, n1~tup1Rev) zip Seq(false, true)
        (flags1, kind1) <- Seq(seg1.flags, seg1.flags.makeLeftHeaded, seg1.flags.makeRightHeaded) zip Seq(Default, LeftHeaded, RightHeaded)
        (flags2, kind2) <- Seq(seg2.flags, seg2.flags.makeLeftHeaded, seg2.flags.makeRightHeaded) zip Seq(Default, LeftHeaded, RightHeaded)
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
          var id = maj.network.rhwRangeId.get + min.network.rhwPieceId.get + prop.orthDiagOffset
          val strangeFlag = isRhwShoulder(maj.network) && (prop.orthDiagOffset >= 0x6000) // strange anomalie for shoulder networks
          val strangeFlagMin = isRhwShoulder(min.network) && (prop.orthDiagOffset % 0x6000 >= 0x3000) // strange anomalie
          if (prop.majorSegReversed ^ strangeFlag) // TODO strange behaviour for shoulder networks
            id += (if (isSingleTileRhw(maj.network)) 0x80 else 0x40)
          if (prop.minorSegReversed ^ strangeFlagMin) // TODO consider changing the IDs to 0x05
            id += (if (maj.network.height == 0 && min.network.height == 0) 0x09 else 0x05)
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
