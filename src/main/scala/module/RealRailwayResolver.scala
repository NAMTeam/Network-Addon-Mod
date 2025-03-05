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
    // rail tiles defined here with RRW IIDs, are maxis IIDs in MiscResolver
    add(Rail~(0,0,0,0), 0x5f33fc00)
    // ortho
    add(Rail~NS, 0x5d540000)
    add(Rail~ES, 0x5d540100)
    add(Rail~CS, 0x5d540200)
    add(L1Dtr~NS, 0x5d640000)
    add(L2Dtr~NS, 0x5d740000)
    add(Str~NS, 0x5d300000)
    // diag
    add(L1Dtr~SE, 0x5d640100)
    add(L2Dtr~SE, 0x5d740100)
    // diag stub
    add(Rail~CWS, 0x5d540300)
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
    add(L1Dtr~CS & L2Dtr~NS, 0x5d7e0800)  // Orth Ramp HT Upper L0->L2

    add(Rail~(1,993,0,0) & L1Dtr~(0,993,0,0), 0x5d6f020e) // Diag OST ground-side L0->L1
    add(Rail~(0,0,0,993) & L1Dtr~(0,0,1,993), 0x5d6f010e) // Diag OST elevated-side L0->L1
    add(Rail~(1,993,0,0) & L2Dtr~(0,993,0,0), 0x5d7f020e) // Diag OST ground-side L0->L2
    add(Rail~(0,0,0,993) & L2Dtr~(0,0,1,993), 0x5d7f010e) // Diag OST elevated-side L0->L2

    add(Rail~(0,0,981,3) & L1Dtr~(0,0,981,0), 0x5d6e1000) // Diag Ramp HT lower L0->L1
    add(Rail~(0,0,0,983) & L1Dtr~(0,0,1,983), 0x5d6e1700) // Diag Ramp HT upper L0->L1
    add(Rail~(0,0,981,3) & L2Dtr~(0,0,981,0), 0x5d7e1000) // Diag Ramp HT lower L0->L1
    add(Rail~(0,0,0,983) & L2Dtr~(0,0,1,983), 0x5d7e1c00) // Diag Ramp HT upper L0->L2

    /* DTR Crossings (L1, & L2)
    This should be a fairly predictable scheme - should be able to automate resolver here.
    Some L0 crossings are to be added, such as 
    */
    // Special: Extra tiles for crossings of overhanging networks
    add(L1Dtr~(0,2,0,42), 0x5d67d000) // OxO overhang L1
    add(L2Dtr~(0,2,0,42), 0x5d77d000) // OxO overhang L2

    // OxD overhang is asymmetrical   ||
    // It looks like this in 0,0 :    \|
    //
    add(L1Dtr~(0,2,8,72), 0x5d67d100) // OxD overhang L1
    add(L2Dtr~(0,2,8,72), 0x5d77d100) // OxD overhang L2

    add(L1Dtr~(0,0,1,43), 0x5d67d200)   // DxO overhang L1 (end)
    add(L1Dtr~(0,0,41,43), 0x5d67d205)  // DxO overhang L1 (blank girder span)
    add(L2Dtr~(0,0,1,43), 0x5d77d200)   // DxO overhang L2 (end)
    add(L2Dtr~(0,0,41,43), 0x5d77d205)  // DxO overhang L2 (blank girder span)

    add(L1Dtr~(0,0,1,73), 0x5d67d300) // DxO overhang L1
    add(L2Dtr~(0,0,1,73), 0x5d77d300) // DxO overhang L2

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
    // -- STR --
    add(Str~WE & Rail~NS, 0x5d510000)
    add(Str~WE & L1Dtr~NS, 0x5d671505)
    add(Str~WE & L2Dtr~NS, 0x5d771505)
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
    add(Road~ES & L1Dtr~NS,   0x5d674100)
    add(Road~ES & L2Dtr~NS,   0x5d774100)
    add(L1Road~ES & Rail~WE,  0x5c007500)
    add(L1Road~ES & L2Dtr~NS, 0x5d774110)
    add(L2Road~ES & Rail~WE,  0x5c037500)
    add(L2Road~ES & L1Dtr~NS, 0x5d67410a)
    // -- OWR --
    add(Onewayroad~ES & L1Dtr~NS, 0x5d674200)
    add(Onewayroad~ES & L2Dtr~NS, 0x5d774200)
    // -- Avenue --
    add(Avenue~SW & L1Dtr~NS, 0x5d674300)
    add(Avenue~SharedDiagLeft & L1Dtr~NS, 0x5d674305)
    add(Avenue~SW & L2Dtr~NS, 0x5d774300)
    add(Avenue~SharedDiagLeft & L2Dtr~NS, 0x5d774305)
    // -- Rail --
    add(Rail~ES & L1Dtr~NS, 0x5d674500)
    add(Rail~ES & L2Dtr~NS, 0x5d774500)
    // -- STR --
    add(Str~NE & Rail~NS,   0x5d510200)
    add(Str~ES & L1Dtr~NS,  0x5d674505)
    add(Str~ES & L2Dtr~NS,  0x5d774505)
    // -- GLR --
    add(Glr1~ES & L1Dtr~NS, 0x5d674700)
    add(Glr2~ES & L1Dtr~NS, 0x5d674705)
    add(Glr3~ES & L1Dtr~NS, 0x5d674800)
    add(Glr4~ES & L1Dtr~NS, 0x5d674805)
    add(Glr1~ES & L2Dtr~NS, 0x5d774700)
    add(Glr2~ES & L2Dtr~NS, 0x5d774705)
    add(Glr3~ES & L2Dtr~NS, 0x5d774800)
    add(Glr4~ES & L2Dtr~NS, 0x5d774805)
    // -- RHW-2 --
    add(Dirtroad~ES & L1Dtr~NS, 0x5d674a00)
    add(Dirtroad~ES & L2Dtr~NS, 0x5d774a00)
    // -- RHW-3 --
    add(Rhw3~ES & L1Dtr~NS, 0x5d674b00)
    add(Rhw3~SE & L1Dtr~NS, 0x5d674b05)
    add(Rhw3~ES & L2Dtr~NS, 0x5d774b00)
    add(Rhw3~SE & L2Dtr~NS, 0x5d774b05)
    // -- MIS --
    add(Mis~ES & L1Dtr~NS, 0x5d674c00)
    add(Mis~SE & L1Dtr~NS, 0x5d674c05)
    add(Mis~ES & L2Dtr~NS, 0x5d774c00)
    add(Mis~SE & L2Dtr~NS, 0x5d774c05)
    // -- RHW-4 --
    add(Rhw4~ES & L1Dtr~NS, 0x5d674d00)
    add(Rhw4~SE & L1Dtr~NS, 0x5d674d05)
    add(Rhw4~ES & L2Dtr~NS, 0x5d774d00)
    add(Rhw4~SE & L2Dtr~NS, 0x5d774d05)
    // -- RHW-6S --
    add(Rhw6s~ES & L1Dtr~NS, 0x5d674e00)
    add(Rhw6s~SE & L1Dtr~NS, 0x5d674e05)
    add(Rhw6s~ES & L2Dtr~NS, 0x5d774e00)
    add(Rhw6s~SE & L2Dtr~NS, 0x5d774e05)
    // ...
    // -- TLA-3 --
    add((Tla3~ES).projectLeft  & L1Dtr~NS, 0x5d675700)
    add((Tla3~ES).projectRight & L1Dtr~NS, 0x5d675700)
    add((Tla3~ES).projectLeft  & L2Dtr~NS, 0x5d775700)
    add((Tla3~ES).projectRight & L2Dtr~NS, 0x5d775700)
    // -- AVE-2 --
    add(Ave2~ES & L1Dtr~NS, 0x5d675800)
    add(Ave2~ES & L2Dtr~NS, 0x5d775800)
    // -- ARD-3 --
    add(Ard3~ES & L1Dtr~NS, 0x5d675900)
    add(Ard3~SE & L1Dtr~NS, 0x5d675905)
    add(Ard3~ES & L2Dtr~NS, 0x5d775900)
    add(Ard3~SE & L2Dtr~NS, 0x5d775905)
    // -- OWR-1 --
    add(Owr1~ES & L1Dtr~NS, 0x5d675a00)
    add(Owr1~ES & L2Dtr~NS, 0x5d775a00)
    // -- OWR-3 --
    add(Owr3~ES & L1Dtr~NS, 0x5d675b00)
    add(Owr3~ES & L2Dtr~NS, 0x5d775b00)
    // -- NRD-4 --
    add(Nrd4~ES & L1Dtr~NS, 0x5d675c00) 
    add(Nrd4~ES & L2Dtr~NS, 0x5d775c00)
    // ...
    // -- SAM-2 --
    add(Sam2~ES & L1Dtr~NS, 0x5d676a00)
    add(Sam2~ES & L2Dtr~NS, 0x5d776a00)
    // -- SAM-3 --
    add(Sam3~ES & L1Dtr~NS, 0x5d676a05)
    add(Sam3~ES & L2Dtr~NS, 0x5d776a05)
    // -- SAM-4 --
    add(Sam4~ES & L1Dtr~NS, 0x5d676a0a)
    add(Sam4~ES & L2Dtr~NS, 0x5d776a0a)
    // -- SAM-5 --
    add(Sam5~ES & L1Dtr~NS, 0x5d676b00)
    add(Sam5~ES & L2Dtr~NS, 0x5d776b00)
    // -- SAM-6 --
    add(Sam6~ES & L1Dtr~NS, 0x5d676b05)
    add(Sam6~ES & L2Dtr~NS, 0x5d776b05)
    // -- SAM-7 --
    add(Sam7~ES & L1Dtr~NS, 0x5d676b0a)
    add(Sam7~ES & L2Dtr~NS, 0x5d776b0a)
    // -- SAM-8 --
    add(Sam8~ES & L1Dtr~NS, 0x5d676c00)
    add(Sam8~ES & L2Dtr~NS, 0x5d776c00)
    // -- SAM-9 --
    add(Sam9~ES & L1Dtr~NS, 0x5d676c05)
    add(Sam9~ES & L2Dtr~NS, 0x5d776c05)
    // -- SAM-10 --
    add(Sam10~ES & L1Dtr~NS, 0x5d676c0a)
    add(Sam10~ES & L2Dtr~NS, 0x5d776c0a)
    // -- SAM-11 --
    add(Sam11~ES & L1Dtr~NS, 0x5d676d00)
    add(Sam11~ES & L2Dtr~NS, 0x5d776d00)

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
    // -- STR --
    add(Str~NS & Rail~NE, 0x5d510100)
    add(Str~NS & L1Dtr~ES, 0x5d677505)
    add(Str~NS & L2Dtr~ES, 0x5d777505)
    // -- GLR --
    add(Glr1~NS & L1Dtr~ES, 0x5d677700)
    add(Glr2~NS & L1Dtr~ES, 0x5d677705)
    add(Glr3~NS & L1Dtr~ES, 0x5d677800)
    add(Glr4~NS & L1Dtr~ES, 0x5d677805)
    add(Glr1~NS & L2Dtr~ES, 0x5d777700)
    add(Glr2~NS & L2Dtr~ES, 0x5d777705)
    add(Glr3~NS & L2Dtr~ES, 0x5d777800)
    add(Glr4~NS & L2Dtr~ES, 0x5d777805)
    // -- RHW-2 --
    add(Dirtroad~NS & L1Dtr~ES,  0x5d677a00)
    add(Dirtroad~NS & L2Dtr~ES,  0x5d777a00)
    // -- RHW-3 --
    add(Rhw3~NS & L1Dtr~ES,  0x5d677b00)
    add(Rhw3~SN & L1Dtr~ES,  0x5d677b05)
    add(Rhw3~NS & L2Dtr~ES,  0x5d777b00)
    add(Rhw3~SN & L2Dtr~ES,  0x5d777b05)
    // -- MIS --
    add(Mis~NS & L1Dtr~ES,   0x5d677c00)
    add(Mis~SN & L1Dtr~ES,   0x5d677c05)
    add(Mis~NS & L2Dtr~ES,   0x5d777c00)
    add(Mis~SN & L2Dtr~ES,   0x5d777c05)
    // -- RHW-4 --
    add(Rhw4~NS & L1Dtr~ES,  0x5d677d00)
    add(Rhw4~SN & L1Dtr~ES,  0x5d677d05)
    add(Rhw4~NS & L2Dtr~ES,  0x5d777d00)
    add(Rhw4~SN & L2Dtr~ES,  0x5d777d05)
    // -- RHW-6S --
    add(Rhw6s~NS & L1Dtr~ES, 0x5d677e00)
    add(Rhw6s~SN & L1Dtr~ES, 0x5d677e05)
    add(Rhw6s~NS & L2Dtr~ES, 0x5d777e00)
    add(Rhw6s~SN & L2Dtr~ES, 0x5d777e05)
    // ...
    // -- TLA-3 --
    add((Tla3~NS).projectLeft  & L1Dtr~ES, 0x5d678700)
    add((Tla3~NS).projectRight & L1Dtr~ES, 0x5d678700)
    add((Tla3~NS).projectLeft  & L2Dtr~ES, 0x5d778700)
    add((Tla3~NS).projectRight & L2Dtr~ES, 0x5d778700)
    // -- AVE-2 --
    add(Ave2~NS & L1Dtr~ES, 0x5d678800)
    add(Ave2~NS & L2Dtr~ES, 0x5d778800)
    // -- ARD-3 --
    add(Ard3~NS & L1Dtr~ES, 0x5d678900)
    add(Ard3~SN & L1Dtr~ES, 0x5d678905)
    add(Ard3~NS & L2Dtr~ES, 0x5d778900)
    add(Ard3~SN & L2Dtr~ES, 0x5d778905)
    // -- OWR-1 --
    add(Owr1~NS & L1Dtr~ES, 0x5d678a00)
    add(Owr1~NS & L2Dtr~ES, 0x5d778a00)
    // -- OWR-3 --
    add(Owr3~NS & L1Dtr~ES, 0x5d678b00)
    add(Owr3~NS & L2Dtr~ES, 0x5d778b00)
    // -- NRD-4 --
    add(Nrd4~NS & L1Dtr~ES, 0x5d678c00) 
    add(Nrd4~NS & L2Dtr~ES, 0x5d778c00)
    // ...
    // -- SAM-2 --
    add(Sam2~NS & L1Dtr~ES, 0x5d679a00)
    add(Sam2~NS & L2Dtr~ES, 0x5d779a00)
    // -- SAM-3 --
    add(Sam3~NS & L1Dtr~ES, 0x5d679a05)
    add(Sam3~NS & L2Dtr~ES, 0x5d779a05)
    // -- SAM-4 --
    add(Sam4~NS & L1Dtr~ES, 0x5d679a0a)
    add(Sam4~NS & L2Dtr~ES, 0x5d779a0a)
    // -- SAM-5 --
    add(Sam5~NS & L1Dtr~ES, 0x5d679b00)
    add(Sam5~NS & L2Dtr~ES, 0x5d779b00)
    // -- SAM-6 --
    add(Sam6~NS & L1Dtr~ES, 0x5d679b05)
    add(Sam6~NS & L2Dtr~ES, 0x5d779b05)
    // -- SAM-7 --
    add(Sam7~NS & L1Dtr~ES, 0x5d679b0a)
    add(Sam7~NS & L2Dtr~ES, 0x5d779b0a)
    // -- SAM-8 --
    add(Sam8~NS & L1Dtr~ES, 0x5d679c00)
    add(Sam8~NS & L2Dtr~ES, 0x5d779c00)
    // -- SAM-9 --
    add(Sam9~NS & L1Dtr~ES, 0x5d679c05)
    add(Sam9~NS & L2Dtr~ES, 0x5d779c05)
    // -- SAM-10 --
    add(Sam10~NS & L1Dtr~ES, 0x5d679c0a)
    add(Sam10~NS & L2Dtr~ES, 0x5d779c0a)
    // -- SAM-11 --
    add(Sam11~NS & L1Dtr~ES, 0x5d679d00)
    add(Sam11~NS & L2Dtr~ES, 0x5d779d00)
    

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
    // -- STR --
    add(Str~NE & Rail~ES, 0x5d510300)
    add(Str~WS & L1Dtr~ES, 0x5d67a505)
    add(Str~WS & L2Dtr~ES, 0x5d77a505)
    // -- GLR --
    add(Glr1~WS & L1Dtr~ES, 0x5d67a700)
    add(Glr2~WS & L1Dtr~ES, 0x5d67a705)
    add(Glr3~WS & L1Dtr~ES, 0x5d67a800)
    add(Glr4~WS & L1Dtr~ES, 0x5d67a805)
    add(Glr1~WS & L2Dtr~ES, 0x5d77a700)
    add(Glr2~WS & L2Dtr~ES, 0x5d77a705)
    add(Glr3~WS & L2Dtr~ES, 0x5d77a800)
    add(Glr4~WS & L2Dtr~ES, 0x5d77a805)
    // -- RHW-2 ---
    add(Dirtroad~WS & L1Dtr~ES,  0x5d67aa00)
    add(Dirtroad~WS & L2Dtr~ES,  0x5d77aa00)
    // -- RHW-3 ---
    add(Rhw3~WS & L1Dtr~ES,  0x5d67ab00)
    add(Rhw3~SW & L1Dtr~ES,  0x5d67ab05)
    add(Rhw3~WS & L2Dtr~ES,  0x5d77ab00)
    add(Rhw3~SW & L2Dtr~ES,  0x5d77ab05)
    // -- MIS ---
    add(Mis~WS & L1Dtr~ES,   0x5d67ac00)
    add(Mis~SW & L1Dtr~ES,   0x5d67ac05)
    add(Mis~WS & L2Dtr~ES,   0x5d77ac00)
    add(Mis~SW & L2Dtr~ES,   0x5d77ac05)
    // -- RHW-4 ---
    add(Rhw4~WS & L1Dtr~ES,  0x5d67ad00)
    add(Rhw4~SW & L1Dtr~ES,  0x5d67ad05)
    add(Rhw4~WS & L2Dtr~ES,  0x5d77ad00)
    add(Rhw4~SW & L2Dtr~ES,  0x5d77ad05)
    // -- RHW-6S --
    add(Rhw6s~WS & L1Dtr~ES, 0x5d67ae00)
    add(Rhw6s~SW & L1Dtr~ES, 0x5d67ae05)
    add(Rhw6s~WS & L2Dtr~ES, 0x5d77ae00)
    add(Rhw6s~SW & L2Dtr~ES, 0x5d77ae05)
    // ...
    // -- TLA-3 --
    add((Tla3~WS).projectLeft  & L1Dtr~ES, 0x5d67b700)
    add((Tla3~WS).projectRight & L1Dtr~ES, 0x5d67b700)
    add((Tla3~WS).projectLeft  & L2Dtr~ES, 0x5d77b700)
    add((Tla3~WS).projectRight & L2Dtr~ES, 0x5d77b700)
    // -- AVE-2 --
    add(Ave2~WS & L1Dtr~ES, 0x5d67b800)
    add(Ave2~WS & L2Dtr~ES, 0x5d77b800)
    // -- ARD-3 --
    add(Ard3~WS & L1Dtr~ES, 0x5d67b900)
    add(Ard3~SW & L1Dtr~ES, 0x5d67b905)
    add(Ard3~WS & L2Dtr~ES, 0x5d77b900)
    add(Ard3~SW & L2Dtr~ES, 0x5d77b905)
    // -- OWR-1 --
    add(Owr1~WS & L1Dtr~ES, 0x5d67ba00)
    add(Owr1~WS & L2Dtr~ES, 0x5d77ba00)
    // -- OWR-3 --
    add(Owr3~WS & L1Dtr~ES, 0x5d67bb00)
    add(Owr3~WS & L2Dtr~ES, 0x5d77bb00)
    // -- NRD-4 --
    add(Nrd4~WS & L1Dtr~ES, 0x5d67bc00) 
    add(Nrd4~WS & L2Dtr~ES, 0x5d77bc00)
    // ...
    // -- SAM-2 --
    add(Sam2~WS & L1Dtr~ES, 0x5d67ca00)
    add(Sam2~WS & L2Dtr~ES, 0x5d77ca00)
    // -- SAM-3 --
    add(Sam3~WS & L1Dtr~ES, 0x5d67ca05)
    add(Sam3~WS & L2Dtr~ES, 0x5d77ca05)
    // -- SAM-4 --
    add(Sam4~WS & L1Dtr~ES, 0x5d67ca0a)
    add(Sam4~WS & L2Dtr~ES, 0x5d77ca0a)
    // -- SAM-5 --
    add(Sam5~WS & L1Dtr~ES, 0x5d67cb00)
    add(Sam5~WS & L2Dtr~ES, 0x5d77cb00)
    // -- SAM-6 --
    add(Sam6~WS & L1Dtr~ES, 0x5d67cb05)
    add(Sam6~WS & L2Dtr~ES, 0x5d77cb05)
    // -- SAM-7 --
    add(Sam7~WS & L1Dtr~ES, 0x5d67cb0a)
    add(Sam7~WS & L2Dtr~ES, 0x5d77cb0a)
    // -- SAM-8 --
    add(Sam8~WS & L1Dtr~ES, 0x5d67cc00)
    add(Sam8~WS & L2Dtr~ES, 0x5d77cc00)
    // -- SAM-9 --
    add(Sam9~WS & L1Dtr~ES, 0x5d67cc05)
    add(Sam9~WS & L2Dtr~ES, 0x5d77cc05)
    // -- SAM-10 --
    add(Sam10~WS & L1Dtr~ES, 0x5d67cc0a)
    add(Sam10~WS & L2Dtr~ES, 0x5d77cc0a)
    // -- SAM-11 --
    add(Sam11~WS & L1Dtr~ES, 0x5d67cd00)
    add(Sam11~WS & L2Dtr~ES, 0x5d77cd00)

    /*
    STR IIDs are a mess, still RAM spec.  TBD
    */
    // -------- STR --------
    // ----- OxO -----
    // -- Street / SAM --
    add(Street~NS & Str~WE, 0x5d340000)
    add(Sam2~NS   & Str~WE, 0x5d3ec000)
    add(Sam3~NS   & Str~WE, 0x5d3e0000)
    add(Sam4~NS   & Str~WE, 0x5d3e4000)
    add(Sam5~NS   & Str~WE, 0x5d3f0000)
    add(Sam6~NS   & Str~WE, 0x5d3f4000)
    add(Sam7~NS   & Str~WE, 0x5d3e8000)
    add(Sam8~NS   & Str~WE, 0x5d3f8000)
    add(Sam9~NS   & Str~WE, 0x5d3fc000)
    add(Sam10~NS  & Str~WE, 0x5d3d0000)
    add(Sam11~NS  & Str~WE, 0x5e511b09) // TODO: currently the only SAM set with a SAM IID for this crossing
    // -- Road --
    add(Road~NS & Str~WE,   0x5d341000)
    add(L1Road~NS & Str~WE,   0x5c001505)
    add(L2Road~NS & Str~WE,   0x5c031505)
    // -- OWR --
    add(Onewayroad~NS & Str~WE,   0x5d342000)
    add(L1Onewayroad~NS & Str~WE,   0x5c011505)
    // -- Avenue --
    add(Avenue~NS & Str~WE,   0x5d343000)
    add(L1Avenue~NS & Str~WE,   0x5c021505)
    add(L2Avenue~NS & Str~WE,   0x5c051505)
    // ----- OxD -----
    // -- Street / SAM --
    add(Street~WN & Str~NS, 0x5d360000)
    add(Sam2~WN   & Str~NS, 0x5d3ee000)
    add(Sam3~WN   & Str~NS, 0x5d3e2000)
    add(Sam4~WN   & Str~NS, 0x5d3e6000)
    add(Sam5~WN   & Str~NS, 0x5d3f2000)
    add(Sam6~WN   & Str~NS, 0x5d3f6000)
    add(Sam7~WN   & Str~NS, 0x5d3ea000)
    add(Sam8~WN   & Str~NS, 0x5d3fa000)
    add(Sam9~WN   & Str~NS, 0x5d3fe000)
    add(Sam10~WN  & Str~NS, 0x5d3d2000)
    add(Sam11~WN  & Str~NS, 0x5e511b09) // TODO: currently the only SAM set with a SAM IID for this crossing
    // -- Road --
    // -- Road L1 --
    // -- Road L2 --
    // -- OWR --
    // -- Avenue --
    // ----- DxO -----
    // -- Street / SAM --
    add(Street~NS & Str~NE, 0x5d380000)
    add(Sam2~NS   & Str~NE, 0x5d3ed000)
    add(Sam3~NS   & Str~NE, 0x5d3e1000)
    add(Sam4~NS   & Str~NE, 0x5d3e5000)
    add(Sam5~NS   & Str~NE, 0x5d3f1000)
    add(Sam6~NS   & Str~NE, 0x5d3f5000)
    add(Sam7~NS   & Str~NE, 0x5d3e9000)
    add(Sam8~NS   & Str~NE, 0x5d3f9000)
    add(Sam9~NS   & Str~NE, 0x5d3fd000)
    add(Sam10~NS  & Str~NE, 0x5d3d1000)
    add(Sam11~NS  & Str~NE, 0x5e512b09) // TODO: currently the only SAM set with a SAM IID for this crossing
    // -- Road --
    // ----- DxD -----
    // -- Street / SAM --
    add(Street~ES & Str~NE, 0x5d3a0000)
    add(Sam2~ES   & Str~NE, 0x5d3ef000)
    add(Sam3~ES   & Str~NE, 0x5d3e3000)
    add(Sam4~ES   & Str~NE, 0x5d3e7000)
    add(Sam5~ES   & Str~NE, 0x5d3f3000)
    add(Sam6~ES   & Str~NE, 0x5d3f7000)
    add(Sam7~ES   & Str~NE, 0x5d3eb000)
    add(Sam8~ES   & Str~NE, 0x5d3fb000)
    add(Sam9~ES   & Str~NE, 0x5d3ff000)
    add(Sam10~ES  & Str~NE, 0x5d3d3000)
    add(Sam11~ES  & Str~NE, 0x5e515b09) // TODO: currently the only SAM set with a SAM IID for this crossing
    // -- Road --
    
    map
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)

}
