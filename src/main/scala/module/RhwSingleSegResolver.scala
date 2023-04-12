package metarules.module

import metarules.meta._
import syntax._, Network._
import RotFlip._
import Flags._


trait SingleSegResolver {

  protected class SingleProperty(val offset: Int, val kind: Flag.Kind.Value, val rf: RotFlip, val swapped: Boolean)

  private def fillMap(m: scala.collection.mutable.Map[Flags, SingleProperty]) = { (tup: IntFlags, offset: Int, swapped: Boolean) =>
    import Flag.Kind._
    def flipKind(k: Flag.Kind.Value, rf: RotFlip): Flag.Kind.Value = if (k == Default || !rf.flipped) k else k match {
      case LeftSpin => RightSpin
      case RightSpin => LeftSpin
    }
    for {
      n <- Seq(Dirtroad, Mis) // Dirtroad and Mis only serve for generating symm and asymm flags
      flagsTmp = (n ~ tup).flags
      (flags, kind) <- Seq(flagsTmp, flagsTmp.spinLeft, flagsTmp.spinRight) zip Seq(Default, LeftSpin, RightSpin) // handle TLA flags, too
      rf <- flags.representations
    } {
      val prop = new SingleProperty(offset, flipKind(kind, rf), rf, swapped)
      m.getOrElseUpdate(flags * rf, prop)
    }
  }

  /** Contains mappings from flags (symm and asymm) to tile IDs/properties of
    * single-segment RHW networks.
    */
  protected val singleProps: Map[Flags, SingleProperty] = {
    val tmp = scala.collection.mutable.Map.empty[Flags, SingleProperty]
    val fill = fillMap(tmp)
    fill(NS, 0, false) // orth
    fill(CS, 0x0100, false) // orth stub
    fill(ES, 0x0200, false) // diag 1
    fill(SE, 0x0900, false) // diag 2
    fill((0,-2,0,+11), 0x0400, false) // 45 curve 1
    fill((0,0,-1,+13), 0x0500, true) // 45 curve 1
    fill((0,+2,0,-11), 0x0b00, false) // 45 curve 2
    fill((0,0,+1,-13), 0x0c00, true) // 45 curve 2
    tmp.toMap
  }

  protected val multiProps: Map[Flags, SingleProperty] = {
    val tmp = scala.collection.mutable.Map.empty[Flags, SingleProperty]
    val fill = fillMap(tmp)
    fill(NS, 0, false) // orth
    fill(CS, 0x0100, false) // orth stub
    fill(ES, 0x0200, false) // diag 1
    fill(SE, 0x0300, false) // diag 2
    fill(SharedDiagRight, 0x0300, false) // shared diag
    fill((0,+2,0,-11), 0x0600, false) // curve assembly..
    fill((0,+111,0,-11), 0x0600, false)
    fill((0,-2,0,+11), 0x0700, false)
    fill((0,-111,0,+11), 0x0700, false)
    fill((0,0,+111,-13), 0x0800, true)
    fill((+1,-3,+1,-13), 0x0800, true) // (avelike)
    fill((0,0,-111,+13), 0x0900, true)
    fill((0,0,-1,+13), 0x0900, true) // (avelike)
    fill((0,+2,0,-111), 0x0a00, false)
    fill((0,-2,0,+111), 0x0b00, false)
    fill((0,0,-111,+3), 0x0c00, true)
    fill((0,0,+111,-3), 0x0d00, true)
    tmp.toMap
  }
}

trait RhwSingleSegResolver extends SingleSegResolver { this: RhwResolver =>

  private[this] val stubSegment = Dirtroad~(0,0,0,0)
  val isRhwShoulderMedian = Network.ValueSet(Rhw8sm, L1Rhw8sm, L2Rhw8sm)

  def resolveSegment(seg: Segment): IdTile = {
    (if (isSingleTileRhw(seg.network)) singleProps else multiProps).get(seg.flags) match {
      case Some(prop) =>
        var id = RhwResolver.rhwRangeId(seg.network)
        val offset = prop.offset
        if (isRhwShoulder(seg.network) && offset == 0x200) { // strange anomalie for shoulder networks
          id += 0x300
        } else if (isRhwShoulder(seg.network) && offset == 0x300) {
          id += 0x200
        } else if (isRhwShoulderMedian(seg.network) && offset >= 0x0600 && offset < 0x0e00) {
          id += offset + (if ((offset & 0x0100) == 0) 0x0100 else -0x0100)  // depends on whether 6th digit is even or odd
        } else {
          id += offset
        }
        IdTile(id, prop.rf)
      case None => if (seg == stubSegment) {
        IdTile(0x57000F00, R0F0)
      } else {
        throw new UnsupportedOperationException(seg.toString) // ??? // TODO curves etc.
      }
    }
  }
}
