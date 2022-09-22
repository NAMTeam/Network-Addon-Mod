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
    add(Rail~NS, 0x03031500)
    add(L1Dtr~NS, 0x5d640000)
    add(L2Dtr~NS, 0x5d740000)
    add(Str~NS, 0x5d300000)
    // diag
    // add(L1Dtr~)
    // add(L2Dtr~)
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
    add(Street~NS & Rail~WE,  0x05010100)
    add(Street~NS & L1Dtr~WE, 0x5d671000)
    add(Street~NS & L2Dtr~WE, 0x5d771000)
    add(Street~NS & Str~WE,   0x5d340000)
    // -- Road --
    add(Road~NS & Rail~WE,  0x03010100)
    add(Road~WE & L1Dtr~NS, 0x5d671100)
    add(Road~WE & L2Dtr~NS, 0x5d771100)
    add(Road~NS & Str~WE,   0x5d341000)
    // -- Road L1 --
    add(L1Road~NS & Rail~WE,  0x5c001500)
    add(L1Road~NS & L2Dtr~WE, 0x5d771105)
    add(L1Road~NS & Str~WE,   0x5c001505)
    // -- Road L2 --
    add(L2Road~NS & Rail~WE,  0x5c031500)
    add(L2Road~WE & L1Dtr~NS, 0x5d67110a)
    add(L2Road~NS & Str~WE,   0x5c031505)
    // -- OWR --
    add(Onewayroad~NS & Rail~WE,  0x09310100)
    add(Onewayroad~NS & L1Dtr~WE, 0x5d671200)
    add(Onewayroad~NS & L2Dtr~WE, 0x5d771200)
    add(Onewayroad~NS & Str~WE,   0x5d342000)
    // -- OWR L1 --
    add(L1Onewayroad~NS & Rail~WE,  0x5c011500)
    add(L1Onewayroad~NS & L2Dtr~WE, 0x5d771210)
    add(L1Onewayroad~NS & Str~WE,   0x5c011505)
    // -- Avenue --
    add(Avenue~NS & Rail~WE,  0x04001500)
    add(Avenue~NS & L1Dtr~WE, 0x5d671300)
    add(Avenue~NS & L2Dtr~WE, 0x5d771300)
    add(Avenue~NS & Str~WE,   0x5d343000)
    // -- Avenue L1 --
    add(L1Avenue~NS & Rail~WE,  0x5c021500)
    add(L1Avenue~NS & L2Dtr~WE, 0x5d771300)
    add(L1Avenue~NS & Str~WE,   0x5c021505)
    // -- Avenue L2 --
    add(L2Avenue~NS & Rail~WE,  0x5c051500)
    add(L2Avenue~NS & L1Dtr~WE, 0x5d671320)
    add(L2Avenue~NS & Str~WE,   0x5c051505)
    // ----- OxD -----
    // -- Street --
    add(Street~WN & Rail~NS, 0x5f502600)
    // -- Road --
    add(Road~WN & Rail~NS, 0x03020100)
    add(Road~ES & L1Dtr~NS, 0x5d674100)
    add(Road~ES & L2Dtr~NS, 0x5d774100)
    // -- Road L1 --
    add(L1Road~ES & Rail~WE, 0x5c007500)
    add(L1Road~ES & L2Dtr~NS, 0x5d774110)
    // -- Road L2 --
    add(L2Road~ES & Rail~WE, 0x5c037500)
    add(L2Road~ES & L1Dtr~NS, 0x5d67410a)
    // -- OWR --
    add(Onewayroad~WN & Rail~NS, 0x09320100)
    // -- Avenue --
    add(Avenue~ES & Rail~NS, 0x04004700)
    add(Avenue~SharedDiagRight & Rail~NS, 0x04004600)
    // ----- DxO -----
    // -- Road --
    // ----- DxD -----
    // -- Road --
    
    map
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)

}