package metarules.module

import metarules.meta._
import Network._
import RotFlip._
import Flags._


trait NwmSingleSegResolver extends SingleSegResolver { this: NwmResolver =>

  // TODO this may need to be updated for current IID scheme
  def resolveNwmSegment(seg: Segment): IdTile = {
    (if (isSingleTileNwm(seg.network)) singleProps else multiProps).get(seg.flags) match {
      case Some(prop) =>
        var id = nwmRangeId(seg.network).get + prop.offset  // TODO check offsets in IID scheme
        if (id % 0x10 != 0 && seg.network.height == 0)
          id += 0x4  // map 8th digit 5 to 9, A to E
        if (prop.kind == Flag.Kind.LeftHeaded || prop.kind == Flag.Kind.RightHeaded &&
            seg.flags.symmetries.exists(_.flipped))
          IdTile(id, prop.rf, if (prop.swapped ^ prop.rf.flipped) rightHeadedMappedRepr else leftHeadedMappedRepr)
        else if (prop.kind == Flag.Kind.RightHeaded)
          IdTile(id + 0x20000000, prop.rf, if(prop.swapped ^ prop.rf.flipped) leftHeadedMappedRepr else rightHeadedMappedRepr) // TODO find suitable ID
        else
          IdTile(id, prop.rf)
      case None => throw new NotImplementedError(seg.toString) // ??? // TODO
    }
  }
}
