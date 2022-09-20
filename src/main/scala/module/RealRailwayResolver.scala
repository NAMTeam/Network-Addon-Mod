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
    // ortho
    add(L1Dtr~NS, 0x5d640000)
    add(L2Dtr~NS, 0x5d540000)
    /* DTR Crossings (L0, L1, & L2)
    OxO
    OxD
    DxO
    DxD
    STR Crossings (L0)
    OxO
    OxD
    DxO
    DxD
    */
    // ----- OxO -----
    // -- Street --
    add(Street~NS & Rail~WE, 0x05010100)
    add(Street~NS & L1Dtr~WE, 0x5d671000)
    add(Street~NS & L2Dtr~WE, 0x5d771000)
    add(Street~NS & Str~WE, 0x5d340000)
    // -- Road --
    add(Road~NS & Rail~WE, 0x03010100)
    add(Road~NS & L1Dtr~WE, 0x5d671100)
    add(Road~NS & L2Dtr~WE, 0x5d771100)
    add(Road~NS & Str~WE, 0x5d341000)
    // -- Road L1 --
    add(L1Road~NS & Rail~WE, 0x5c001500)
    add(L1Road~NS & L2Dtr~WE, 0x5d771110)
    add(L1Road~NS & Str~WE, 0x5c001505)
    // -- Road L2 --
    add(L2Road~NS & Rail~WE, 0x5c031500)
    add(L2Road~NS & L1Dtr~WE, 0x5d671120)
    add(L2Road~NS & Str~WE, 0x5c031505)
    // -- OWR --
    add(Onewayroad~NS & Rail~WE, 0x09310100)
    add(Onewayroad~NS & L1Dtr~WE, 0x5d671200)
    add(Onewayroad~NS & L2Dtr~WE, 0x5d771200)
    add(Onewayroad~NS & Str~WE, 0x5d342000)
    // -- Avenue --
    add(Avenue~NS & Rail~WE, 0x04001500)
    add(Avenue~NS & L1Dtr~WE, 0x5d671300)
    add(Avenue~NS & L2Dtr~WE, 0x5d771300)
    add(Avenue~NS & Str~WE, 0x5d343000)
    // -- Avenue L1 --
    add(L1Avenue~NS & Rail~WE, 0x5c021500)
    add(L1Avenue~NS & L2Dtr~WE, 0x5d771300)
    add(L1Avenue~NS & Str~WE, 0x5c021505)
    // ----- OxD -----
    // -- Road --
    // ----- DxO -----
    // -- Road --
    // ----- DxD -----
    // -- Road --
    
    map
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)

}