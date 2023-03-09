package metarules.module

import metarules.meta._
import Network._
import RotFlip._
import Flags._
import Implicits.segmentToTile


trait NwmSingleSegResolver extends SingleSegResolver { this: NwmResolver =>

  lazy val explicitTileMap: scala.collection.Map[Tile, IdTile] = {
    val map = scala.collection.mutable.Map.empty[Tile, IdTile]
    def add0(tile: Tile, id: Int): Unit = {
      assert(!map.contains(tile))
      for (rf <- RotFlip.values) {
        val idTile = IdTile(id, rf)
        map.getOrElseUpdate(tile * rf, idTile)
      }
    }
    def add(tile: Tile, id: Int): Unit = {
      if (tile.segs.exists(_.network.isTla)) {
        // curves do not have turn paths, so we can map both projections to the same ID
        add0(Tile.projectLeft(tile), id)
        add0(Tile.projectRight(tile), id)
      } else {
        add0(tile, id)
      }
    }

    // Multi-tile NWM curve assembly
    // (listed explicitly to simplify maintaining compatibility with old IID scheme)
    // (TODO consider migrating sharp curves to RHW-spec mini curves or extended curves)
    for (n <- Seq(Tla5, Rd6, Owr5)) {
      add(n~(0,-13,0,+2),   nwmRangeId(n).get + 0x0400)  // sharp curve outside
      add(n~(0,0,+1,-13),   nwmRangeId(n).get + 0x0500)  // sharp curve inside
      add(n~(0,+13,0,-2),   nwmRangeId(n).get + 0x0509)  // sharp curve inside (TODO add orthogonal placeholder texture)
      add(n~(0,0,-1,+13),   nwmRangeId(n).get + 0x0600)  // sharp curve outside
    }
    for (n <- Seq(Rd4, Owr4)) {
      add(n~(0,-2,0,+11),   nwmRangeId(n).get + 0x0500)  // shared diagonal curve outside
      add(n~(0,+2,0,-11),   nwmRangeId(n).get + 0x0600)  // shared diagonal curve inside
      add(n~(0,0,-1,+13),   nwmRangeId(n).get + 0x0700)  // shared diagonal curve outside
      add(n~(+1,-3,+1,-13), nwmRangeId(n).get + 0x0800)  // shared diagonal curve
    }
    for (n <- Seq(Ave6, Ave8)) {
      add(n~(0,0,+1,-13),   nwmRangeId(n).get + 0x0400)  // sharp curve inside
      add(n~(0,+13,0,-2),   nwmRangeId(n).get + 0x0409)  // sharp curve inside (TODO add orthogonal placeholder texture)
      add(n~(0,-113,0,+2),  nwmRangeId(n).get + 0x0500)  // extended curve outside
      add(n~(0,-13,0,+113), nwmRangeId(n).get + 0x0600)  // extended curve outside
      add(n~(0,0,-111,+13), nwmRangeId(n).get + 0x0700)  // extended curve outside
      add(n~(+111,-3,0,0),  nwmRangeId(n).get + 0x0800)  // extended curve outside
    }
    for (n <- Seq(Ave6m, Tla7m)) {
      add(n~(0,-13,0,+2),   nwmRangeId(n).get + 0x0400)  // mini curve
      add(n~(0,0,-111,+13), nwmRangeId(n).get + 0x0500)  // mini curve
      add(n~(+111,-3,0,0),  nwmRangeId(n).get + 0x0600)  // mini curve
    }
    map
  }

  // TODO this may need to be updated for current IID scheme
  def resolveNwmSegment(seg: Segment): IdTile = explicitTileMap.getOrElse(seg, {
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
  })
}
