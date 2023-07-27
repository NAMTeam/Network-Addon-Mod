package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, RotFlip._, Flags._
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
    add(Rail~(0,0,0,0), 0x5f33fc00)
    // ortho
    add(Rail~NS, 0x5d540000)
    add(Rail~CS, 0x5d540200)
    add(L1Dtr~NS, 0x5d640000)
    add(L2Dtr~NS, 0x5d740000)
    add(Str~NS, 0x5d300000)
    // diag
    add(L1Dtr~SE, 0x5d640100)
    add(L2Dtr~SE, 0x5d740100)
    // OxO crossing
    add(Rail~NS & Rail~EW, 0x5d548000)
    // OxD crossing
    add(Rail~NS & Rail~NE, 0x5d548100)
    // DxD crossing
    add(Rail~SE & Rail~NE, 0x5d548200)

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
    add(Street~WE & L1Dtr~NS, 0x5d671000)
    add(Street~WE & L2Dtr~NS, 0x5d771000)
    // -- Road --
    add(Road~WE & L1Dtr~NS, 0x5d671100)
    add(Road~WE & L2Dtr~NS, 0x5d771100)
    add(L1Road~NS & Rail~WE,  0x5c001500)
    add(L1Road~NS & L2Dtr~WE, 0x5d771105)
    add(L2Road~NS & Rail~WE,  0x5c031500)
    add(L2Road~WE & L1Dtr~NS, 0x5d67110a)
    // -- OWR --
    add(Onewayroad~WE & L1Dtr~NS, 0x5d671200)
    add(Onewayroad~WE & L2Dtr~NS, 0x5d771200)
    add(L1Onewayroad~NS & Rail~WE,  0x5c011500)
    add(L1Onewayroad~NS & L2Dtr~WE, 0x5d771205)
    add(L2Onewayroad~NS & Rail~WE, 0x5c041500)
    add(L2Onewayroad~NS & L1Dtr~WE, 0x5d67120a)
    // -- Avenue --
    add(Avenue~EW & L1Dtr~NS, 0x5d671300)
    add(Avenue~EW & L2Dtr~NS, 0x5d771300)
    add(L1Avenue~NS & Rail~WE,  0x5c021500)
    add(L1Avenue~NS & L2Dtr~WE, 0x5d771305)
    add(L2Avenue~NS & Rail~WE,  0x5c051500)
    add(L2Avenue~EW & L1Dtr~NS, 0x5d67130a)
    // -- Rail --
    add(Rail~WE & L1Dtr~NS, 0x5d671500)
    add(Rail~WE & L2Dtr~NS, 0x5d771500)
    add(L2Dtr~WE & L1Dtr~NS, 0x5d771510)
    // -- GLR  --
    add(Glr1~WE & L1Dtr~NS, 0x5d671700)
    add(Glr2~WE & L1Dtr~NS, 0x5d671705)
    add(Glr3~WE & L1Dtr~NS, 0x5d671800)
    add(Glr4~WE & L1Dtr~NS, 0x5d671805)
    add(Glr1~WE & L2Dtr~NS, 0x5d771700)
    add(Glr2~WE & L2Dtr~NS, 0x5d771705)
    add(Glr3~WE & L2Dtr~NS, 0x5d771800)
    add(Glr4~WE & L2Dtr~NS, 0x5d771805)
    // -- HSRP --
    add(Hsr~WE & L1Dtr~NS, 0x5d671905)
    add(Hsr~WE & L2Dtr~NS, 0x5d771905)
    // -- RHW-2
    add(Dirtroad~WE & L1Dtr~NS, 0x5d671a00)
    add(Dirtroad~WE & L2Dtr~NS, 0x5d771a00)
    // -- RHW-3 --
    add(Rhw3~WE & L1Dtr~NS, 0x5d671b00)
    add(Rhw3~WE & L2Dtr~NS, 0x5d771b00)
    // -- MIS --
    add(Mis~WE & L1Dtr~NS, 0x5d671c00)
    add(Mis~WE & L2Dtr~NS, 0x5d771c00)
    // -- RHW-4 --
    add(Rhw4~WE & L1Dtr~NS, 0x5d671d00)
    add(Rhw4~WE & L2Dtr~NS, 0x5d771d00)
    // -- RHW-6S --
    add(Rhw6s~WE & L1Dtr~NS, 0x5d671e00)
    add(Rhw6s~WE & L2Dtr~NS, 0x5d771e00)
    // TO DO - what to do with extra tile 5d771e05, 5d771e10?
    // -- RHW-8S Median
    add(Rhw8sm~WE & L1Dtr~NS, 0x5d671f00)
    add(Rhw8sm~WE & L2Dtr~NS, 0x5d771f00)
    // -- RHW-8S Shoulder --
    add(Rhw8s~EW & L1Dtr~NS, 0x5d672000)
    add(Rhw8s~EW & L2Dtr~NS, 0x5d772000)
    // -- RHW-10S Shoulder --
    add(Rhw10s~EW & L1Dtr~NS, 0x5d672100)
    add(Rhw10s~EW & L2Dtr~NS, 0x5d772100)
    // -- RHW-6C Median --
    add(Rhw6cm~WE & L1Dtr~NS, 0x5d672300)
    add(Rhw6cm~WE & L2Dtr~NS, 0x5d772300)
    // -- RHW-6C Shoulder --
    add(Rhw6c~EW & L1Dtr~NS, 0x5d672400)
    add(Rhw6c~EW & L2Dtr~NS, 0x5d772400)
    // -- RHW-8C Shoulder --
    add(Rhw8c~EW & L1Dtr~NS, 0x5d672500)
    add(Rhw8c~EW & L2Dtr~NS, 0x5d772500)
    // -- TLA-3 --
    add((Tla3~WE).projectLeft & L1Dtr~NS, 0x5d672700)
    add((Tla3~WE).projectRight & L1Dtr~NS, 0x5d672700)
    add((Tla3~WE).projectLeft & L2Dtr~NS, 0x5d772700)
    add((Tla3~WE).projectRight & L2Dtr~NS, 0x5d772700)
    // -- AVE-2 --
    add(Ave2~WE & L1Dtr~NS, 0x5d672800)
    add(Ave2~WE & L2Dtr~NS, 0x5d772800)
    // -- ARD-3 --
    add(Ard3~WE & L1Dtr~NS, 0x5d672900)
    add(Ard3~WE & L2Dtr~NS, 0x5d772900)
    // -- OWR-1 --
    add(Owr1~WE & L1Dtr~NS, 0x5d672a00)
    add(Owr1~WE & L2Dtr~NS, 0x5d772a00)
    // -- OWR-3 --
    add(Owr3~WE & L1Dtr~NS, 0x5d672b00)
    add(Owr3~WE & L2Dtr~NS, 0x5d772b00)
    // -- NRD-4 --
    add(Nrd4~WE & L1Dtr~NS, 0x5d672c00) 
    add(Nrd4~WE & L2Dtr~NS, 0x5d772c00)
    // -- TLA-5 --
    add((Tla5~EW).projectLeft & L1Dtr~NS, 0x5d672d00)
    add((Tla5~EW).projectRight & L1Dtr~NS, 0x5d672d00)
    add((Tla5~EW).projectLeft & L2Dtr~NS, 0x5d772d00)
    add((Tla5~EW).projectRight & L2Dtr~NS, 0x5d772d00)
    // -- OWR-4 --
    add(Owr4~WE & L1Dtr~NS, 0x5d672e00)
    add(Owr4~WE & L2Dtr~NS, 0x5d772e00)
    // -- OWR-5 --
    add(Owr5~WE & L1Dtr~NS, 0x5d672f00)
    add(Owr5~WE & L2Dtr~NS, 0x5d772f00)
    // -- RD-4 --
    add(Rd4~WE & L1Dtr~NS, 0x5d673000)
    add(Rd4~WE & L2Dtr~NS, 0x5d773000)
    // -- RD-6 --
    add(Rd6~WE & L1Dtr~NS, 0x5d673100)
    add(Rd6~WE & L2Dtr~NS, 0x5d773100)
    // -- TLA-7 Shoulder / Ave6? -- 3200
    add(Ave6~WE & L1Dtr~NS, 0x5d673200)
    add(Ave6~WE & L2Dtr~NS, 0x5d773200)
    // -- TLA-7 Median --
    add(Tla7m~WE & L1Dtr~NS, 0x5d673300)
    add(Tla7m~WE & L2Dtr~NS, 0x5d773300)
    // -- TLA-9 Shoulder -- 3400
    // -- AVE-6 Median
    add(Ave6m~WE & L1Dtr~NS, 0x5d673500)
    add(Ave6m~WE & L2Dtr~NS, 0x5d773500)
    // -- TOS -- 3600
    // -- TOR -- 3700
    // -- TIR -- 3705
    // -- TIA -- 3800
    // -- EL-Rail over Road -- 3900
    // -- El-Rail over Avenue -- 3905
    // -- SAM-2 --
    add(Sam2~WE & L1Dtr~NS, 0x5d673a00)
    add(Sam2~WE & L2Dtr~NS, 0x5d773a00)
    // -- SAM-3 --
    add(Sam3~WE & L1Dtr~NS, 0x5d673a05)
    add(Sam3~WE & L2Dtr~NS, 0x5d773a05)
    // -- SAM-4 --
    add(Sam4~WE & L1Dtr~NS, 0x5d673a0a)
    add(Sam4~WE & L2Dtr~NS, 0x5d773a0a)
    // -- SAM-5 --
    add(Sam5~WE & L1Dtr~NS, 0x5d673b00)
    add(Sam5~WE & L2Dtr~NS, 0x5d773b00)
    // -- SAM-6 --
    add(Sam6~WE & L1Dtr~NS, 0x5d673b05)
    add(Sam6~WE & L2Dtr~NS, 0x5d773b05)
    // -- SAM-7 --
    add(Sam7~WE & L1Dtr~NS, 0x5d673b0a)
    add(Sam7~WE & L2Dtr~NS, 0x5d773b0a)
    // -- SAM-8 --
    add(Sam8~WE & L1Dtr~NS, 0x5d673c00)
    add(Sam8~WE & L2Dtr~NS, 0x5d773c00)
    // -- SAM-9 --
    add(Sam9~WE & L1Dtr~NS, 0x5d673c05)
    add(Sam9~WE & L2Dtr~NS, 0x5d773c05)
    // -- SAM-10 --
    add(Sam10~WE & L1Dtr~NS, 0x5d673c0a)
    add(Sam10~WE & L2Dtr~NS, 0x5d773c0a)
    // -- SAM-11 --
    add(Sam11~WE & L1Dtr~NS, 0x5d673d00)
    add(Sam11~WE & L2Dtr~NS, 0x5d773d00)
    

    // ----- OxD -----
    // -- Street --
    add(Street~ES & L1Dtr~NS, 0x5d674000)
    add(Street~ES & L2Dtr~NS, 0x5d774000)
    // -- Road --
    add(Road~ES & L1Dtr~NS, 0x5d674100)
    add(Road~ES & L2Dtr~NS, 0x5d774100)
    add(L1Road~ES & Rail~WE, 0x5c007500)
    add(L1Road~ES & L2Dtr~NS, 0x5d774110)
    add(L2Road~ES & Rail~WE, 0x5c037500)
    add(L2Road~ES & L1Dtr~NS, 0x5d67410a)
    // -- OWR --
    add(Onewayroad~ES & L1Dtr~NS, 0x5d674200)
    add(Onewayroad~ES & L2Dtr~NS, 0x5d774200)
    // -- Avenue --
    add(Avenue~ES & L1Dtr~NS, 0x5d674300)
    add(Avenue~SharedDiagRight & L1Dtr~NS, 0x5d674305)
    add(Avenue~ES & L2Dtr~NS, 0x5d774300)
    add(Avenue~SharedDiagRight & L2Dtr~NS, 0x5d774305)
    // -- Rail --
    add(Rail~ES & L1Dtr~NS, 0x5d674500)
    add(Rail~ES & L2Dtr~NS, 0x5d774500)

    // ----- DxO -----
    // -- Street --
    // add(Street~NS & Rail~ES)
    add(Street~NS & L1Dtr~ES, 0x5d677000)
    add(Street~NS & L2Dtr~ES, 0x5d777000)
    // -- Road --
    add(Road~NS & L1Dtr~ES, 0x5d677100)
    add(Road~NS & L2Dtr~ES, 0x5d777100)
    // -- OWR --
    add(Onewayroad~NS & L1Dtr~ES, 0x5d677200)
    add(Onewayroad~NS & L2Dtr~ES, 0x5d777200)
    // -- Avenue --
    add(Avenue~SN & L1Dtr~ES, 0x5d677300)
    add(Avenue~SN & L2Dtr~ES, 0x5d777300)
    add(Avenue~NS & L1Dtr~ES, 0x5d677305)
    add(Avenue~NS & L2Dtr~ES, 0x5d777305)
    // -- Rail --
    add(Rail~NS & L1Dtr~ES, 0x5d677500)
    add(Rail~NS & L2Dtr~ES, 0x5d777500)

    // ----- DxD -----
    // -- Street --
    add(Street~WS & L1Dtr~ES, 0x5d67a000)
    add(Street~WS & L2Dtr~ES, 0x5d77a000)
    // -- Road --
    add(Road~WS & L1Dtr~ES, 0x5d67a100)
    add(Road~WS & L2Dtr~ES, 0x5d77a100)
    // -- OWR --
    add(Onewayroad~WS & L1Dtr~ES, 0x5d67a200)
    add(Onewayroad~WS & L2Dtr~ES, 0x5d77a200)
    // -- Avenue --
    add(Avenue~SW & L1Dtr~ES, 0x5d67a300)
    add(Avenue~SW & L2Dtr~ES, 0x5d77a300)
    add(Avenue~SharedDiagLeft & L1Dtr~ES, 0x5d67a305)
    add(Avenue~SharedDiagLeft & L2Dtr~ES, 0x5d77a305)
    // -- Rail --
    add(Rail~WS & L1Dtr~ES, 0x5d67a500)
    add(Rail~WS & L2Dtr~ES, 0x5d77a500)

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
