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
    add(L2Dtr~NS, 0x5d740000)
    add(Str~NS, 0x5d300000)
    // diag
    add(L1Dtr~SE, 0x5d640100)
    add(L2Dtr~SE, 0x5d740100)
    // temp - needs a better home
    add(L1Road~NS, 0x5c000000)
    add(L1Road~SE, 0x5c000200)
    add(L2Road~NS, 0x5c030000)
    add(L2Road~SE, 0x5c030200)
    // Height Transition
    add(Rail~CS & L1Dtr~CN, 0x5d6f0000)   // Orth OST L0->L1
    add(L1Dtr~CS & L2Dtr~CN, 0x5d6f0600)  // Orth OST L1->L2
    add(Rail~CS & L2Dtr~CN, 0x5d7f0000)   // Orth OST L0->L2
    add(Rail~NS & L1Dtr~CN, 0x5d6e0000)   // Orth Ramp HT Lower L0->L1
    add(Rail~CS & L1Dtr~NS, 0x5d6e0400)   // Orth Ramp HT Upper L0->L1
    add(Rail~NS & L2Dtr~CN, 0x5d7e0000)   // Orth Ramp HT Lower L0->L2
    add(Rail~CS & L2Dtr~NS, 0x5d7e0800)   // Orth Ramp HT Upper L0->L2 
    add(L1Dtr~NS & L2Dtr~CN, 0x5d7e0900)  // Orth Ramp HT Lower L1->L2

    //add(Rail~(0,0,1,3) & L1Dtr(0,0,1,0)) // Diag Ramp HT lower
    //add(Rail~(0,0,0,3) & L1Dtr(0,0,1,3)) // Diag Ramp HT upper

    //add(Rail~(1,13,0,0) & L1Dtr(0,13,0,0)) // Diag OST lower
    //add(Rail~(0,0,0,13) & L1Dtr~(0,0,1,13)) // Diag OST upper

    /* DTR Crossings (L1, & L2)
    This should be a fairly predictable scheme - should be able to automate resolver here.
    Some L0 crossings are to be added, such as 
    */
    // ----- OxO -----
    // -- Street --
    add(Street~NS & L1Dtr~WE, 0x5d671000)
    add(Street~NS & L2Dtr~WE, 0x5d771000)
    // -- Road --
    add(Road~WE & L1Dtr~NS, 0x5d671100)
    add(Road~WE & L2Dtr~NS, 0x5d771100)
    // -- Road L1 --
    add(L1Road~NS & Rail~WE,  0x5c001500)
    add(L1Road~NS & L2Dtr~WE, 0x5d771105)
    // -- Road L2 --
    add(L2Road~NS & Rail~WE,  0x5c031500)
    add(L2Road~WE & L1Dtr~NS, 0x5d67110a)
    // -- OWR --
    add(Onewayroad~NS & L1Dtr~WE, 0x5d671200)
    add(Onewayroad~NS & L2Dtr~WE, 0x5d771200)
    // -- OWR L1 --
    add(L1Onewayroad~NS & Rail~WE,  0x5c011500)
    add(L1Onewayroad~NS & L2Dtr~WE, 0x5d771210)
    // -- Avenue --
    add(Avenue~NS & L1Dtr~WE, 0x5d671300)
    add(Avenue~NS & L2Dtr~WE, 0x5d771300)
    // -- Avenue L1 --
    add(L1Avenue~NS & Rail~WE,  0x5c021500)
    add(L1Avenue~NS & L2Dtr~WE, 0x5d771300)
    // -- Avenue L2 --
    add(L2Avenue~NS & Rail~WE,  0x5c051500)
    add(L2Avenue~NS & L1Dtr~WE, 0x5d671320)
    // ----- OxD -----
    // -- Street --
    add(Street~WN & Rail~NS, 0x5f502600) // move to misc resolver
    add(Street~ES & L1Dtr~NS, 0x5d674000)
    add(Street~ES & L2Dtr~NS, 0x5d774000)
    // -- Road --
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
    // -- Street --
    // -- Road --
    // ----- DxD -----
    // -- Road --

    /*
    STR IIDs are a mess, still RAM spec.  TBD
    */
    // -------- STR --------
    // ----- OxO -----
    add(Street~NS & Str~WE,   0x5d340000)
    add(Road~NS & Str~WE,   0x5d341000)
    add(L1Road~NS & Str~WE,   0x5c001505)
    add(L2Road~NS & Str~WE,   0x5c031505)
    add(Onewayroad~NS & Str~WE,   0x5d342000)
    add(L1Onewayroad~NS & Str~WE,   0x5c011505)
    add(Avenue~NS & Str~WE,   0x5d343000)
    add(L1Avenue~NS & Str~WE,   0x5c021505)
    add(L2Avenue~NS & Str~WE,   0x5c051505)
    // ----- OxD -----
    // -- Street --
    // -- Road --
    // -- Road L1 --
    // -- Road L2 --
    // -- OWR --
    // -- Avenue --
    // ----- DxO -----
    // -- Road --
    // ----- DxD -----
    // -- Road --
    
    map
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)

}