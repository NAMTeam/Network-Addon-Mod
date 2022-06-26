package metarules.module

import metarules.meta._, Network._, RotFlip._, Flags._
import Implicits.segmentToTile

class RealRailwayResolver extends IdResolver {

  val tileMap: scala.collection.Map[Tile, IdTile] = {
    val map = scala.collection.mutable.Map.empty[Tile, IdTile]
    def add(tile: Tile, id: Int): Unit = {
      assert(!map.contains(tile))
      for (rf <- RotFlip.values) {
        val idTile = IdTile(id, rf)
        map.getOrElseUpdate(tile * rf, idTile)
      }
    }
    add(Road~NS, 0x00004b00); add(Road~ES, 0x00000a00)
    add(Rail~NS, 0x03031500); add(Rail~ES, 0x03001a00)
    add(Road~NS & Rail~WE, 0x03010100)
    add(Road~NS & Rail~NE, 0x03010200)
    add(Road~WN & Rail~NS, 0x03020100)
    add(Road~ES & Rail~NE, 0x03020200)
    add(Str~NS, 0x5D500000); add(Str~ES, 0x5D500100)
    add(Str~WE & Road~NS, 0x5d341000)
    
    map
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)

}