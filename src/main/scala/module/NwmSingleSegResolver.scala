package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, RotFlip._, Flags._
import Implicits.segmentToTile
import NetworkProperties.{isSingleTile, isDoubleTile, nonMirroredOnly, mirroredOnly}
import NwmResolver._

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
        add0(NetworkProperties.projectTlaLeft(tile), id)
        add0(NetworkProperties.projectTlaRight(tile), id)
      } else {
        add0(tile, id)
      }
    }

    // Multi-tile NWM curve assembly
    // (listed explicitly to simplify maintaining compatibility with old IID scheme)
    // (TODO consider migrating sharp curves to RHW-spec mini curves or extended curves)
    for (n <- Seq(Tla5, Rd6, Owr5)) {
      add(n~(0,-13,0,+2),   nwmRangeId(n) + 0x0400)  // sharp curve outside
      add(n~(0,0,+1,-13),   nwmRangeId(n) + 0x0500)  // sharp curve inside
      add(n~(0,+13,0,-2),   nwmRangeId(n) + 0x0509)  // sharp curve inside (TODO add orthogonal placeholder texture)
      add(n~(0,0,-1,+13),   nwmRangeId(n) + 0x0600)  // sharp curve outside
    }
    for (n <- Seq(Rd4, Owr4)) {
      add(n~(0,-2,0,+11),   nwmRangeId(n) + 0x0500)  // shared diagonal curve outside
      add(n~(0,+2,0,-11),   nwmRangeId(n) + 0x0600)  // shared diagonal curve inside
      add(n~(0,0,-1,+13),   nwmRangeId(n) + 0x0700)  // shared diagonal curve outside
      add(n~(+1,-3,+1,-13), nwmRangeId(n) + 0x0800)  // shared diagonal curve
    }
    for (n <- Seq(Ave6, Ave8)) {
      add(n~(0,0,+1,-13),   nwmRangeId(n) + 0x0400)  // sharp curve inside
      add(n~(0,+13,0,-2),   nwmRangeId(n) + 0x0409)  // sharp curve inside (TODO add orthogonal placeholder texture)
      add(n~(0,-113,0,+2),  nwmRangeId(n) + 0x0500)  // extended curve outside
      add(n~(0,-13,0,+113), nwmRangeId(n) + 0x0600)  // extended curve outside
      add(n~(0,0,-111,+13), nwmRangeId(n) + 0x0700)  // extended curve outside
      add(n~(+111,-3,0,0),  nwmRangeId(n) + 0x0800)  // extended curve outside
    }
    for (n <- Seq(Ave6m, Tla7m)) {
      add(n~(0,-13,0,+2),   nwmRangeId(n) + 0x0400)  // mini curve
      add(n~(0,0,-111,+13), nwmRangeId(n) + 0x0500)  // mini curve
      add(n~(+111,-3,0,0),  nwmRangeId(n) + 0x0600)  // mini curve
    }
    for (n <- NwmNetworks if isSingleTile(n)) {
      add(n~(0,0,-2,+2),    nwmRangeId(n) + 0x0800)  // 90 degree curve
      add(n~(0,-2,0,+11),   nwmRangeId(n) + 0x0400)  // 45 degree curve 1
      add(n~(0,0,-1,+13),   nwmRangeId(n) + 0x0500)  // 45 degree curve 1
      if (!n.isSymm) {
        add(n~(0,0,+2,-2),  nwmRangeId(n) + 0x0e00)  // 90 degree curve
        add(n~(0,+2,0,-11), nwmRangeId(n) + 0x0b00)  // 45 degree curve 2
        add(n~(0,0,+1,-13), nwmRangeId(n) + 0x0c00)  // 45 degree curve 2
      }
    }
    for ((n, offset) <- Seq(Rd4 -> 0, Tla5 -> 0x0100)) {
      add(n~(0,0,-2,+2),    nwmRangeId(n) + 0x0980 + offset)  // 90 degree curve outside
      add(n~(0,-113,0,+2),  nwmRangeId(n) + 0x0900 + offset)  // 90 degree curve extended
      add(n~(0,0,+2,-2),    nwmRangeId(n) + 0x0a00 + offset)  // 90 degree curve inside
    }
    map
  }

  // TODO this may need to be updated for current IID scheme
  def resolveNwmSegment(seg: Segment): IdTile = explicitTileMap.getOrElse(seg, {
    (if (isSingleTileNwm(seg.network)) singleProps else multiProps).get(seg.flags) match {
      case Some(prop) =>
        var id = nwmRangeId(seg.network) + prop.offset  // TODO check offsets in IID scheme
        if (id % 0x10 != 0 && seg.network.height == 0)
          id += 0x4  // map 8th digit 5 to 9, A to E
        if (prop.kind == Flag.Kind.LeftSpin || prop.kind == Flag.Kind.RightSpin &&
            seg.flags.symmetries.exists(_.flipped))
          IdTile(id, prop.rf, if (prop.swapped ^ prop.rf.flipped) mirroredOnly else nonMirroredOnly)
        else if (prop.kind == Flag.Kind.RightSpin)
          IdTile(id + 0x20000000, prop.rf, if(prop.swapped ^ prop.rf.flipped) nonMirroredOnly else mirroredOnly) // TODO find suitable ID
        else
          IdTile(id, prop.rf)
      case None => throw new NotImplementedError(seg.toString) // ??? // TODO
    }
  })
}
