package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, RotFlip._, Flags._
import Implicits.segmentToTile

class RealRailwayResolver extends IdResolver {

  val tileMap: scala.collection.Map[Tile, IdTile] = {
    val builder = new ResolverBuilder
    import builder.add

    // rail tiles defined here with RRW IIDs, are maxis IIDs in MiscResolver
    add(0x5f33fc00, Rail~(0,0,0,0))
    // ortho
    add(0x5d540000, Rail~NS)
    add(0x5d540100, Rail~ES)
    add(0x5d540200, Rail~CS)
    add(0x5d640000, L1Dtr~NS)
    add(0x5d740000, L2Dtr~NS)
    add(0x5d300000, Str~NS)
    // diag
    add(0x5d640100, L1Dtr~SE)
    add(0x5d740100, L2Dtr~SE)
    // diag stub
    add(0x5d540300, Rail~CWS)
    // OxO crossing
    add(0x5d548000, Rail~NS & Rail~EW)
    // OxD crossing
    add(0x5d548100, Rail~NS & Rail~NE)
    // DxD crossing
    add(0x5d548200, Rail~SE & Rail~NE)

    // Height Transition
    add(0x5d6f0000, Rail~CS & L1Dtr~CN)   // Orth OST L0->L1
    add(0x5d6f0600, L1Dtr~CS & L2Dtr~CN)  // Orth OST L1->L2
    add(0x5d7f0000, Rail~CS & L2Dtr~CN)   // Orth OST L0->L2

    add(0x5d6e0000, Rail~NS & L1Dtr~CN)   // Orth Ramp HT Lower L0->L1
    add(0x5d6e0400, Rail~CS & L1Dtr~NS)   // Orth Ramp HT Upper L0->L1
    add(0x5d7e0000, Rail~NS & L2Dtr~CN)   // Orth Ramp HT Lower L0->L2
    add(0x5d7e0800, Rail~CS & L2Dtr~NS)   // Orth Ramp HT Upper L0->L2
    add(0x5d7e0900, L1Dtr~NS & L2Dtr~CN)  // Orth Ramp HT Lower L1->L2
    add(0x5d7e0800, L1Dtr~CS & L2Dtr~NS)  // Orth Ramp HT Upper L0->L2

    add(0x5d6f020e, Rail~(1,993,0,0) & L1Dtr~(0,993,0,0)) // Diag OST ground-side L0->L1
    add(0x5d6f010e, Rail~(0,0,0,993) & L1Dtr~(0,0,1,993)) // Diag OST elevated-side L0->L1
    add(0x5d7f020e, Rail~(1,993,0,0) & L2Dtr~(0,993,0,0)) // Diag OST ground-side L0->L2
    add(0x5d7f010e, Rail~(0,0,0,993) & L2Dtr~(0,0,1,993)) // Diag OST elevated-side L0->L2

    add(0x5d6e1000, Rail~(0,0,981,3) & L1Dtr~(0,0,981,0)) // Diag Ramp HT lower L0->L1
    add(0x5d6e1700, Rail~(0,0,0,983) & L1Dtr~(0,0,1,983)) // Diag Ramp HT upper L0->L1
    add(0x5d7e1000, Rail~(0,0,981,3) & L2Dtr~(0,0,981,0)) // Diag Ramp HT lower L0->L1
    add(0x5d7e1c00, Rail~(0,0,0,983) & L2Dtr~(0,0,1,983)) // Diag Ramp HT upper L0->L2

    /* DTR Crossings (L1, & L2)
    This should be a fairly predictable scheme - should be able to automate resolver here.
    Some L0 crossings are to be added, such as
    */
    // Special: Extra tiles for crossings of overhanging networks
    add(0x5d67d000, L1Dtr~(0,2,0,42)) // OxO overhang L1
    add(0x5d77d000, L2Dtr~(0,2,0,42)) // OxO overhang L2

    // OxD overhang is asymmetrical   ||
    // It looks like this in 0,0 :    \|
    //
    add(0x5d67d100, L1Dtr~(0,2,8,72)) // OxD overhang L1
    add(0x5d77d100, L2Dtr~(0,2,8,72)) // OxD overhang L2

    add(0x5d67d200, L1Dtr~(0,0,1,43))   // DxO overhang L1 (end)
    add(0x5d67d205, L1Dtr~(0,0,41,43))  // DxO overhang L1 (blank girder span)
    add(0x5d77d200, L2Dtr~(0,0,1,43))   // DxO overhang L2 (end)
    add(0x5d77d205, L2Dtr~(0,0,41,43))  // DxO overhang L2 (blank girder span)

    add(0x5d67d300, L1Dtr~(0,0,1,73)) // DxO overhang L1
    add(0x5d77d300, L2Dtr~(0,0,1,73)) // DxO overhang L2

    // ----- OxO -----
    // -- Street --
    add(0x5d671000, Street~WE & L1Dtr~NS)
    add(0x5d771000, Street~WE & L2Dtr~NS)
    // -- Road --
    add(0x5d671100, Road~WE & L1Dtr~NS)
    add(0x5d771100, Road~WE & L2Dtr~NS)
    add(0x5c001500, L1Road~NS & Rail~WE)
    add(0x5d771105, L1Road~NS & L2Dtr~WE)
    add(0x5c031500, L2Road~NS & Rail~WE)
    add(0x5d67110a, L2Road~WE & L1Dtr~NS)
    // -- OWR --
    add(0x5d671200, Onewayroad~WE & L1Dtr~NS)
    add(0x5d771200, Onewayroad~WE & L2Dtr~NS)
    add(0x5c011500, L1Onewayroad~NS & Rail~WE)
    add(0x5d771205, L1Onewayroad~NS & L2Dtr~WE)
    add(0x5c041500, L2Onewayroad~NS & Rail~WE)
    add(0x5d67120a, L2Onewayroad~NS & L1Dtr~WE)
    // -- Avenue --
    add(0x5d671300, Avenue~EW & L1Dtr~NS)
    add(0x5d771300, Avenue~EW & L2Dtr~NS)
    add(0x5c021500, L1Avenue~NS & Rail~WE)
    add(0x5d771305, L1Avenue~NS & L2Dtr~WE)
    add(0x5c051500, L2Avenue~NS & Rail~WE)
    add(0x5d67130a, L2Avenue~EW & L1Dtr~NS)
    // -- Rail --
    add(0x5d671500, Rail~WE & L1Dtr~NS)
    add(0x5d771500, Rail~WE & L2Dtr~NS)
    add(0x5d771510, L2Dtr~WE & L1Dtr~NS)
    // -- STR --
    add(0x5d510000, Str~WE & Rail~NS)
    add(0x5d671505, Str~WE & L1Dtr~NS)
    add(0x5d771505, Str~WE & L2Dtr~NS)
    // -- GLR  --
    add(0x5d671700, Glr1~WE & L1Dtr~NS)
    add(0x5d671705, Glr2~WE & L1Dtr~NS)
    add(0x5d671800, Glr3~WE & L1Dtr~NS)
    add(0x5d671805, Glr4~WE & L1Dtr~NS)
    add(0x5d771700, Glr1~WE & L2Dtr~NS)
    add(0x5d771705, Glr2~WE & L2Dtr~NS)
    add(0x5d771800, Glr3~WE & L2Dtr~NS)
    add(0x5d771805, Glr4~WE & L2Dtr~NS)
    // -- HSRP --
    add(0x5d671905, Hsr~WE & L1Dtr~NS)
    add(0x5d771905, Hsr~WE & L2Dtr~NS)
    // -- RHW-2
    add(0x5d671a00, Dirtroad~WE & L1Dtr~NS)
    add(0x5d771a00, Dirtroad~WE & L2Dtr~NS)
    // -- RHW-3 --
    add(0x5d671b00, Rhw3~WE & L1Dtr~NS)
    add(0x5d771b00, Rhw3~WE & L2Dtr~NS)
    // -- MIS --
    add(0x5d671c00, Mis~WE & L1Dtr~NS)
    add(0x5d771c00, Mis~WE & L2Dtr~NS)
    // -- RHW-4 --
    add(0x5d671d00, Rhw4~WE & L1Dtr~NS)
    add(0x5d771d00, Rhw4~WE & L2Dtr~NS)
    // -- RHW-6S --
    add(0x5d671e00, Rhw6s~WE & L1Dtr~NS)
    add(0x5d771e00, Rhw6s~WE & L2Dtr~NS)
    // TO DO - what to do with extra tile 5d771e05, 5d771e10?
    // -- RHW-8S Median
    add(0x5d671f00, Rhw8sm~WE & L1Dtr~NS)
    add(0x5d771f00, Rhw8sm~WE & L2Dtr~NS)
    // -- RHW-8S Shoulder --
    add(0x5d672000, Rhw8s~EW & L1Dtr~NS)
    add(0x5d772000, Rhw8s~EW & L2Dtr~NS)
    // -- RHW-10S Shoulder --
    add(0x5d672100, Rhw10s~EW & L1Dtr~NS)
    add(0x5d772100, Rhw10s~EW & L2Dtr~NS)
    // -- RHW-6C Median --
    add(0x5d672300, Rhw6cm~WE & L1Dtr~NS)
    add(0x5d772300, Rhw6cm~WE & L2Dtr~NS)
    // -- RHW-6C Shoulder --
    add(0x5d672400, Rhw6c~EW & L1Dtr~NS)
    add(0x5d772400, Rhw6c~EW & L2Dtr~NS)
    // -- RHW-8C Shoulder --
    add(0x5d672500, Rhw8c~EW & L1Dtr~NS)
    add(0x5d772500, Rhw8c~EW & L2Dtr~NS)
    // -- TLA-3 --
    add(0x5d672700, Tla3~WE & L1Dtr~NS)
    add(0x5d772700, Tla3~WE & L2Dtr~NS)
    // -- AVE-2 --
    add(0x5d672800, Ave2~WE & L1Dtr~NS)
    add(0x5d772800, Ave2~WE & L2Dtr~NS)
    // -- ARD-3 --
    add(0x5d672900, Ard3~WE & L1Dtr~NS)
    add(0x5d772900, Ard3~WE & L2Dtr~NS)
    // -- OWR-1 --
    add(0x5d672a00, Owr1~WE & L1Dtr~NS)
    add(0x5d772a00, Owr1~WE & L2Dtr~NS)
    // -- OWR-3 --
    add(0x5d672b00, Owr3~WE & L1Dtr~NS)
    add(0x5d772b00, Owr3~WE & L2Dtr~NS)
    // -- NRD-4 --
    add(0x5d672c00, Nrd4~WE & L1Dtr~NS)
    add(0x5d772c00, Nrd4~WE & L2Dtr~NS)
    // -- TLA-5 --
    add(0x5d672d00, Tla5~EW & L1Dtr~NS)
    add(0x5d772d00, Tla5~EW & L2Dtr~NS)
    // -- OWR-4 --
    add(0x5d672e00, Owr4~WE & L1Dtr~NS)
    add(0x5d772e00, Owr4~WE & L2Dtr~NS)
    // -- OWR-5 --
    add(0x5d672f00, Owr5~WE & L1Dtr~NS)
    add(0x5d772f00, Owr5~WE & L2Dtr~NS)
    // -- RD-4 --
    add(0x5d673000, Rd4~WE & L1Dtr~NS)
    add(0x5d773000, Rd4~WE & L2Dtr~NS)
    // -- RD-6 --
    add(0x5d673100, Rd6~WE & L1Dtr~NS)
    add(0x5d773100, Rd6~WE & L2Dtr~NS)
    // -- TLA-7 Shoulder / Ave6? -- 3200
    add(0x5d673200, Ave6~WE & L1Dtr~NS)
    add(0x5d773200, Ave6~WE & L2Dtr~NS)
    // -- TLA-7 Median --
    add(0x5d673300, Tla7m~WE & L1Dtr~NS)
    add(0x5d773300, Tla7m~WE & L2Dtr~NS)
    // -- TLA-9 Shoulder -- 3400
    // -- AVE-6 Median
    add(0x5d673500, Ave6m~WE & L1Dtr~NS)
    add(0x5d773500, Ave6m~WE & L2Dtr~NS)
    // -- TOS -- 3600
    // -- TOR -- 3700
    // -- TIR -- 3705
    // -- TIA -- 3800
    // -- EL-Rail over Road -- 3900
    // -- El-Rail over Avenue -- 3905
    // -- SAM-2 --
    add(0x5d673a00, Sam2~WE & L1Dtr~NS)
    add(0x5d773a00, Sam2~WE & L2Dtr~NS)
    // -- SAM-3 --
    add(0x5d673a05, Sam3~WE & L1Dtr~NS)
    add(0x5d773a05, Sam3~WE & L2Dtr~NS)
    // -- SAM-4 --
    add(0x5d673a0a, Sam4~WE & L1Dtr~NS)
    add(0x5d773a0a, Sam4~WE & L2Dtr~NS)
    // -- SAM-5 --
    add(0x5d673b00, Sam5~WE & L1Dtr~NS)
    add(0x5d773b00, Sam5~WE & L2Dtr~NS)
    // -- SAM-6 --
    add(0x5d673b05, Sam6~WE & L1Dtr~NS)
    add(0x5d773b05, Sam6~WE & L2Dtr~NS)
    // -- SAM-7 --
    add(0x5d673b0a, Sam7~WE & L1Dtr~NS)
    add(0x5d773b0a, Sam7~WE & L2Dtr~NS)
    // -- SAM-8 --
    add(0x5d673c00, Sam8~WE & L1Dtr~NS)
    add(0x5d773c00, Sam8~WE & L2Dtr~NS)
    // -- SAM-9 --
    add(0x5d673c05, Sam9~WE & L1Dtr~NS)
    add(0x5d773c05, Sam9~WE & L2Dtr~NS)
    // -- SAM-10 --
    add(0x5d673c0a, Sam10~WE & L1Dtr~NS)
    add(0x5d773c0a, Sam10~WE & L2Dtr~NS)
    // -- SAM-11 --
    add(0x5d673d00, Sam11~WE & L1Dtr~NS)
    add(0x5d773d00, Sam11~WE & L2Dtr~NS)


    // ----- OxD -----
    // -- Street --
    add(0x5d674000, Street~ES & L1Dtr~NS)
    add(0x5d774000, Street~ES & L2Dtr~NS)
    // -- Road --
    add(0x5d674100, Road~ES & L1Dtr~NS)
    add(0x5d774100, Road~ES & L2Dtr~NS)
    add(0x5c007500, L1Road~ES & Rail~WE)
    add(0x5d774110, L1Road~ES & L2Dtr~NS)
    add(0x5c037500, L2Road~ES & Rail~WE)
    add(0x5d67410a, L2Road~ES & L1Dtr~NS)
    // -- OWR --
    add(0x5d674200, Onewayroad~ES & L1Dtr~NS)
    add(0x5d774200, Onewayroad~ES & L2Dtr~NS)
    // -- Avenue --
    add(0x5d674300, Avenue~SW & L1Dtr~NS)
    add(0x5d674305, Avenue~SharedDiagLeft & L1Dtr~NS)
    add(0x5d774300, Avenue~SW & L2Dtr~NS)
    add(0x5d774305, Avenue~SharedDiagLeft & L2Dtr~NS)
    // -- Rail --
    add(0x5d674500, Rail~ES & L1Dtr~NS)
    add(0x5d774500, Rail~ES & L2Dtr~NS)
    // -- STR --
    add(0x5d510200, Str~NE & Rail~NS)
    add(0x5d674505, Str~ES & L1Dtr~NS)
    add(0x5d774505, Str~ES & L2Dtr~NS)
    // -- GLR --
    add(0x5d674700, Glr1~ES & L1Dtr~NS)
    add(0x5d674705, Glr2~ES & L1Dtr~NS)
    add(0x5d674800, Glr3~ES & L1Dtr~NS)
    add(0x5d674805, Glr4~ES & L1Dtr~NS)
    add(0x5d774700, Glr1~ES & L2Dtr~NS)
    add(0x5d774705, Glr2~ES & L2Dtr~NS)
    add(0x5d774800, Glr3~ES & L2Dtr~NS)
    add(0x5d774805, Glr4~ES & L2Dtr~NS)
    // -- RHW-2 --
    add(0x5d674a00, Dirtroad~ES & L1Dtr~NS)
    add(0x5d774a00, Dirtroad~ES & L2Dtr~NS)
    // -- RHW-3 --
    add(0x5d674b00, Rhw3~ES & L1Dtr~NS)
    add(0x5d674b05, Rhw3~SE & L1Dtr~NS)
    add(0x5d774b00, Rhw3~ES & L2Dtr~NS)
    add(0x5d774b05, Rhw3~SE & L2Dtr~NS)
    // -- MIS --
    add(0x5d674c00, Mis~ES & L1Dtr~NS)
    add(0x5d674c05, Mis~SE & L1Dtr~NS)
    add(0x5d774c00, Mis~ES & L2Dtr~NS)
    add(0x5d774c05, Mis~SE & L2Dtr~NS)
    // -- RHW-4 --
    add(0x5d674d00, Rhw4~ES & L1Dtr~NS)
    add(0x5d674d05, Rhw4~SE & L1Dtr~NS)
    add(0x5d774d00, Rhw4~ES & L2Dtr~NS)
    add(0x5d774d05, Rhw4~SE & L2Dtr~NS)
    // -- RHW-6S --
    add(0x5d674e00, Rhw6s~ES & L1Dtr~NS)
    add(0x5d674e05, Rhw6s~SE & L1Dtr~NS)
    add(0x5d774e00, Rhw6s~ES & L2Dtr~NS)
    add(0x5d774e05, Rhw6s~SE & L2Dtr~NS)
    // ...
    // -- TLA-3 --
    add(0x5d675700, Tla3~ES & L1Dtr~NS)
    add(0x5d775700, Tla3~ES & L2Dtr~NS)
    // -- AVE-2 --
    add(0x5d675800, Ave2~ES & L1Dtr~NS)
    add(0x5d775800, Ave2~ES & L2Dtr~NS)
    // -- ARD-3 --
    add(0x5d675900, Ard3~ES & L1Dtr~NS)
    add(0x5d675905, Ard3~SE & L1Dtr~NS)
    add(0x5d775900, Ard3~ES & L2Dtr~NS)
    add(0x5d775905, Ard3~SE & L2Dtr~NS)
    // -- OWR-1 --
    add(0x5d675a00, Owr1~ES & L1Dtr~NS)
    add(0x5d775a00, Owr1~ES & L2Dtr~NS)
    // -- OWR-3 --
    add(0x5d675b00, Owr3~ES & L1Dtr~NS)
    add(0x5d775b00, Owr3~ES & L2Dtr~NS)
    // -- NRD-4 --
    add(0x5d675c00, Nrd4~ES & L1Dtr~NS)
    add(0x5d775c00, Nrd4~ES & L2Dtr~NS)
    // ...
    // -- SAM-2 --
    add(0x5d676a00, Sam2~ES & L1Dtr~NS)
    add(0x5d776a00, Sam2~ES & L2Dtr~NS)
    // -- SAM-3 --
    add(0x5d676a05, Sam3~ES & L1Dtr~NS)
    add(0x5d776a05, Sam3~ES & L2Dtr~NS)
    // -- SAM-4 --
    add(0x5d676a0a, Sam4~ES & L1Dtr~NS)
    add(0x5d776a0a, Sam4~ES & L2Dtr~NS)
    // -- SAM-5 --
    add(0x5d676b00, Sam5~ES & L1Dtr~NS)
    add(0x5d776b00, Sam5~ES & L2Dtr~NS)
    // -- SAM-6 --
    add(0x5d676b05, Sam6~ES & L1Dtr~NS)
    add(0x5d776b05, Sam6~ES & L2Dtr~NS)
    // -- SAM-7 --
    add(0x5d676b0a, Sam7~ES & L1Dtr~NS)
    add(0x5d776b0a, Sam7~ES & L2Dtr~NS)
    // -- SAM-8 --
    add(0x5d676c00, Sam8~ES & L1Dtr~NS)
    add(0x5d776c00, Sam8~ES & L2Dtr~NS)
    // -- SAM-9 --
    add(0x5d676c05, Sam9~ES & L1Dtr~NS)
    add(0x5d776c05, Sam9~ES & L2Dtr~NS)
    // -- SAM-10 --
    add(0x5d676c0a, Sam10~ES & L1Dtr~NS)
    add(0x5d776c0a, Sam10~ES & L2Dtr~NS)
    // -- SAM-11 --
    add(0x5d676d00, Sam11~ES & L1Dtr~NS)
    add(0x5d776d00, Sam11~ES & L2Dtr~NS)

    // ----- DxO -----
    // -- Street --
    // add(???, Street~NS & Rail~ES)
    add(0x5d677000, Street~NS & L1Dtr~ES)
    add(0x5d777000, Street~NS & L2Dtr~ES)
    // -- Road --
    add(0x5d677100, Road~NS & L1Dtr~ES)
    add(0x5d777100, Road~NS & L2Dtr~ES)
    // -- OWR --
    add(0x5d677200, Onewayroad~NS & L1Dtr~ES)
    add(0x5d777200, Onewayroad~NS & L2Dtr~ES)
    // -- Avenue --
    add(0x5d677300, Avenue~SN & L1Dtr~ES)
    add(0x5d777300, Avenue~SN & L2Dtr~ES)
    add(0x5d677305, Avenue~NS & L1Dtr~ES)
    add(0x5d777305, Avenue~NS & L2Dtr~ES)
    // -- Rail --
    add(0x5d677500, Rail~NS & L1Dtr~ES)
    add(0x5d777500, Rail~NS & L2Dtr~ES)
    // -- STR --
    add(0x5d510100, Str~NS & Rail~NE)
    add(0x5d677505, Str~NS & L1Dtr~ES)
    add(0x5d777505, Str~NS & L2Dtr~ES)
    // -- GLR --
    add(0x5d677700, Glr1~NS & L1Dtr~ES)
    add(0x5d677705, Glr2~NS & L1Dtr~ES)
    add(0x5d677800, Glr3~NS & L1Dtr~ES)
    add(0x5d677805, Glr4~NS & L1Dtr~ES)
    add(0x5d777700, Glr1~NS & L2Dtr~ES)
    add(0x5d777705, Glr2~NS & L2Dtr~ES)
    add(0x5d777800, Glr3~NS & L2Dtr~ES)
    add(0x5d777805, Glr4~NS & L2Dtr~ES)
    // -- RHW-2 --
    add(0x5d677a00, Dirtroad~NS & L1Dtr~ES)
    add(0x5d777a00, Dirtroad~NS & L2Dtr~ES)
    // -- RHW-3 --
    add(0x5d677b00, Rhw3~NS & L1Dtr~ES)
    add(0x5d677b05, Rhw3~SN & L1Dtr~ES)
    add(0x5d777b00, Rhw3~NS & L2Dtr~ES)
    add(0x5d777b05, Rhw3~SN & L2Dtr~ES)
    // -- MIS --
    add(0x5d677c00, Mis~NS & L1Dtr~ES)
    add(0x5d677c05, Mis~SN & L1Dtr~ES)
    add(0x5d777c00, Mis~NS & L2Dtr~ES)
    add(0x5d777c05, Mis~SN & L2Dtr~ES)
    // -- RHW-4 --
    add(0x5d677d00, Rhw4~NS & L1Dtr~ES)
    add(0x5d677d05, Rhw4~SN & L1Dtr~ES)
    add(0x5d777d00, Rhw4~NS & L2Dtr~ES)
    add(0x5d777d05, Rhw4~SN & L2Dtr~ES)
    // -- RHW-6S --
    add(0x5d677e00, Rhw6s~NS & L1Dtr~ES)
    add(0x5d677e05, Rhw6s~SN & L1Dtr~ES)
    add(0x5d777e00, Rhw6s~NS & L2Dtr~ES)
    add(0x5d777e05, Rhw6s~SN & L2Dtr~ES)
    // ...
    // -- TLA-3 --
    add(0x5d678700, Tla3~NS & L1Dtr~ES)
    add(0x5d778700, Tla3~NS & L2Dtr~ES)
    // -- AVE-2 --
    add(0x5d678800, Ave2~NS & L1Dtr~ES)
    add(0x5d778800, Ave2~NS & L2Dtr~ES)
    // -- ARD-3 --
    add(0x5d678900, Ard3~NS & L1Dtr~ES)
    add(0x5d678905, Ard3~SN & L1Dtr~ES)
    add(0x5d778900, Ard3~NS & L2Dtr~ES)
    add(0x5d778905, Ard3~SN & L2Dtr~ES)
    // -- OWR-1 --
    add(0x5d678a00, Owr1~NS & L1Dtr~ES)
    add(0x5d778a00, Owr1~NS & L2Dtr~ES)
    // -- OWR-3 --
    add(0x5d678b00, Owr3~NS & L1Dtr~ES)
    add(0x5d778b00, Owr3~NS & L2Dtr~ES)
    // -- NRD-4 --
    add(0x5d678c00, Nrd4~NS & L1Dtr~ES)
    add(0x5d778c00, Nrd4~NS & L2Dtr~ES)
    // ...
    // -- SAM-2 --
    add(0x5d679a00, Sam2~NS & L1Dtr~ES)
    add(0x5d779a00, Sam2~NS & L2Dtr~ES)
    // -- SAM-3 --
    add(0x5d679a05, Sam3~NS & L1Dtr~ES)
    add(0x5d779a05, Sam3~NS & L2Dtr~ES)
    // -- SAM-4 --
    add(0x5d679a0a, Sam4~NS & L1Dtr~ES)
    add(0x5d779a0a, Sam4~NS & L2Dtr~ES)
    // -- SAM-5 --
    add(0x5d679b00, Sam5~NS & L1Dtr~ES)
    add(0x5d779b00, Sam5~NS & L2Dtr~ES)
    // -- SAM-6 --
    add(0x5d679b05, Sam6~NS & L1Dtr~ES)
    add(0x5d779b05, Sam6~NS & L2Dtr~ES)
    // -- SAM-7 --
    add(0x5d679b0a, Sam7~NS & L1Dtr~ES)
    add(0x5d779b0a, Sam7~NS & L2Dtr~ES)
    // -- SAM-8 --
    add(0x5d679c00, Sam8~NS & L1Dtr~ES)
    add(0x5d779c00, Sam8~NS & L2Dtr~ES)
    // -- SAM-9 --
    add(0x5d679c05, Sam9~NS & L1Dtr~ES)
    add(0x5d779c05, Sam9~NS & L2Dtr~ES)
    // -- SAM-10 --
    add(0x5d679c0a, Sam10~NS & L1Dtr~ES)
    add(0x5d779c0a, Sam10~NS & L2Dtr~ES)
    // -- SAM-11 --
    add(0x5d679d00, Sam11~NS & L1Dtr~ES)
    add(0x5d779d00, Sam11~NS & L2Dtr~ES)


    // ----- DxD -----
    // -- Street --
    add(0x5d67a000, Street~WS & L1Dtr~ES)
    add(0x5d77a000, Street~WS & L2Dtr~ES)
    // -- Road --
    add(0x5d67a100, Road~WS & L1Dtr~ES)
    add(0x5d77a100, Road~WS & L2Dtr~ES)
    // -- OWR --
    add(0x5d67a200, Onewayroad~WS & L1Dtr~ES)
    add(0x5d77a200, Onewayroad~WS & L2Dtr~ES)
    // -- Avenue --
    add(0x5d67a300, Avenue~SW & L1Dtr~ES)
    add(0x5d77a300, Avenue~SW & L2Dtr~ES)
    add(0x5d67a305, Avenue~SharedDiagLeft & L1Dtr~ES)
    add(0x5d77a305, Avenue~SharedDiagLeft & L2Dtr~ES)
    // -- Rail --
    add(0x5d67a500, Rail~WS & L1Dtr~ES)
    add(0x5d77a500, Rail~WS & L2Dtr~ES)
    // -- STR --
    add(0x5d510300, Str~NE & Rail~ES)
    add(0x5d67a505, Str~WS & L1Dtr~ES)
    add(0x5d77a505, Str~WS & L2Dtr~ES)
    // -- GLR --
    add(0x5d67a700, Glr1~WS & L1Dtr~ES)
    add(0x5d67a705, Glr2~WS & L1Dtr~ES)
    add(0x5d67a800, Glr3~WS & L1Dtr~ES)
    add(0x5d67a805, Glr4~WS & L1Dtr~ES)
    add(0x5d77a700, Glr1~WS & L2Dtr~ES)
    add(0x5d77a705, Glr2~WS & L2Dtr~ES)
    add(0x5d77a800, Glr3~WS & L2Dtr~ES)
    add(0x5d77a805, Glr4~WS & L2Dtr~ES)
    // -- RHW-2 ---
    add(0x5d67aa00, Dirtroad~WS & L1Dtr~ES)
    add(0x5d77aa00, Dirtroad~WS & L2Dtr~ES)
    // -- RHW-3 ---
    add(0x5d67ab00, Rhw3~WS & L1Dtr~ES)
    add(0x5d67ab05, Rhw3~SW & L1Dtr~ES)
    add(0x5d77ab00, Rhw3~WS & L2Dtr~ES)
    add(0x5d77ab05, Rhw3~SW & L2Dtr~ES)
    // -- MIS ---
    add(0x5d67ac00, Mis~WS & L1Dtr~ES)
    add(0x5d67ac05, Mis~SW & L1Dtr~ES)
    add(0x5d77ac00, Mis~WS & L2Dtr~ES)
    add(0x5d77ac05, Mis~SW & L2Dtr~ES)
    // -- RHW-4 ---
    add(0x5d67ad00, Rhw4~WS & L1Dtr~ES)
    add(0x5d67ad05, Rhw4~SW & L1Dtr~ES)
    add(0x5d77ad00, Rhw4~WS & L2Dtr~ES)
    add(0x5d77ad05, Rhw4~SW & L2Dtr~ES)
    // -- RHW-6S --
    add(0x5d67ae00, Rhw6s~WS & L1Dtr~ES)
    add(0x5d67ae05, Rhw6s~SW & L1Dtr~ES)
    add(0x5d77ae00, Rhw6s~WS & L2Dtr~ES)
    add(0x5d77ae05, Rhw6s~SW & L2Dtr~ES)
    // ...
    // -- TLA-3 --
    add(0x5d67b700, Tla3~WS & L1Dtr~ES)
    add(0x5d77b700, Tla3~WS & L2Dtr~ES)
    // -- AVE-2 --
    add(0x5d67b800, Ave2~WS & L1Dtr~ES)
    add(0x5d77b800, Ave2~WS & L2Dtr~ES)
    // -- ARD-3 --
    add(0x5d67b900, Ard3~WS & L1Dtr~ES)
    add(0x5d67b905, Ard3~SW & L1Dtr~ES)
    add(0x5d77b900, Ard3~WS & L2Dtr~ES)
    add(0x5d77b905, Ard3~SW & L2Dtr~ES)
    // -- OWR-1 --
    add(0x5d67ba00, Owr1~WS & L1Dtr~ES)
    add(0x5d77ba00, Owr1~WS & L2Dtr~ES)
    // -- OWR-3 --
    add(0x5d67bb00, Owr3~WS & L1Dtr~ES)
    add(0x5d77bb00, Owr3~WS & L2Dtr~ES)
    // -- NRD-4 --
    add(0x5d67bc00, Nrd4~WS & L1Dtr~ES)
    add(0x5d77bc00, Nrd4~WS & L2Dtr~ES)
    // ...
    // -- SAM-2 --
    add(0x5d67ca00, Sam2~WS & L1Dtr~ES)
    add(0x5d77ca00, Sam2~WS & L2Dtr~ES)
    // -- SAM-3 --
    add(0x5d67ca05, Sam3~WS & L1Dtr~ES)
    add(0x5d77ca05, Sam3~WS & L2Dtr~ES)
    // -- SAM-4 --
    add(0x5d67ca0a, Sam4~WS & L1Dtr~ES)
    add(0x5d77ca0a, Sam4~WS & L2Dtr~ES)
    // -- SAM-5 --
    add(0x5d67cb00, Sam5~WS & L1Dtr~ES)
    add(0x5d77cb00, Sam5~WS & L2Dtr~ES)
    // -- SAM-6 --
    add(0x5d67cb05, Sam6~WS & L1Dtr~ES)
    add(0x5d77cb05, Sam6~WS & L2Dtr~ES)
    // -- SAM-7 --
    add(0x5d67cb0a, Sam7~WS & L1Dtr~ES)
    add(0x5d77cb0a, Sam7~WS & L2Dtr~ES)
    // -- SAM-8 --
    add(0x5d67cc00, Sam8~WS & L1Dtr~ES)
    add(0x5d77cc00, Sam8~WS & L2Dtr~ES)
    // -- SAM-9 --
    add(0x5d67cc05, Sam9~WS & L1Dtr~ES)
    add(0x5d77cc05, Sam9~WS & L2Dtr~ES)
    // -- SAM-10 --
    add(0x5d67cc0a, Sam10~WS & L1Dtr~ES)
    add(0x5d77cc0a, Sam10~WS & L2Dtr~ES)
    // -- SAM-11 --
    add(0x5d67cd00, Sam11~WS & L1Dtr~ES)
    add(0x5d77cd00, Sam11~WS & L2Dtr~ES)

    /*
    STR IIDs are a mess, still RAM spec.  TBD
    */
    // -------- STR --------
    // ----- OxO -----
    // -- Street / SAM --
    add(0x5d340000, Street~NS & Str~WE)
    add(0x5d3ec000, Sam2~NS   & Str~WE)
    add(0x5d3e0000, Sam3~NS   & Str~WE)
    add(0x5d3e4000, Sam4~NS   & Str~WE)
    add(0x5d3f0000, Sam5~NS   & Str~WE)
    add(0x5d3f4000, Sam6~NS   & Str~WE)
    add(0x5d3e8000, Sam7~NS   & Str~WE)
    add(0x5d3f8000, Sam8~NS   & Str~WE)
    add(0x5d3fc000, Sam9~NS   & Str~WE)
    add(0x5d3d0000, Sam10~NS  & Str~WE)
    add(0x5e511b09, Sam11~NS  & Str~WE) // TODO: currently the only SAM set with a SAM IID for this crossing
    // -- Road --
    add(0x5d341000, Road~NS & Str~WE)
    add(0x5c001505, L1Road~NS & Str~WE)
    add(0x5c031505, L2Road~NS & Str~WE)
    // -- OWR --
    add(0x5d342000, Onewayroad~NS & Str~WE)
    add(0x5c011505, L1Onewayroad~NS & Str~WE)
    // -- Avenue --
    add(0x5d343000, Avenue~NS & Str~WE)
    add(0x5c021505, L1Avenue~NS & Str~WE)
    add(0x5c051505, L2Avenue~NS & Str~WE)
    // ----- OxD -----
    // -- Street / SAM --
    add(0x5d360000, Street~WN & Str~NS)
    add(0x5d3ee000, Sam2~WN   & Str~NS)
    add(0x5d3e2000, Sam3~WN   & Str~NS)
    add(0x5d3e6000, Sam4~WN   & Str~NS)
    add(0x5d3f2000, Sam5~WN   & Str~NS)
    add(0x5d3f6000, Sam6~WN   & Str~NS)
    add(0x5d3ea000, Sam7~WN   & Str~NS)
    add(0x5d3fa000, Sam8~WN   & Str~NS)
    add(0x5d3fe000, Sam9~WN   & Str~NS)
    add(0x5d3d2000, Sam10~WN  & Str~NS)
    add(0x5e514b09, Sam11~WN  & Str~NS) // TODO: currently the only SAM set with a SAM IID for this crossing
    // -- Road --
    // -- Road L1 --
    // -- Road L2 --
    // -- OWR --
    // -- Avenue --
    // ----- DxO -----
    // -- Street / SAM --
    add(0x5d380000, Street~NS & Str~NE)
    add(0x5d3ed000, Sam2~NS   & Str~NE)
    add(0x5d3e1000, Sam3~NS   & Str~NE)
    add(0x5d3e5000, Sam4~NS   & Str~NE)
    add(0x5d3f1000, Sam5~NS   & Str~NE)
    add(0x5d3f5000, Sam6~NS   & Str~NE)
    add(0x5d3e9000, Sam7~NS   & Str~NE)
    add(0x5d3f9000, Sam8~NS   & Str~NE)
    add(0x5d3fd000, Sam9~NS   & Str~NE)
    add(0x5d3d1000, Sam10~NS  & Str~NE)
    add(0x5e512b09, Sam11~NS  & Str~NE) // TODO: currently the only SAM set with a SAM IID for this crossing
    // -- Road --
    // ----- DxD -----
    // -- Street / SAM --
    add(0x5d3a0000, Street~ES & Str~NE)
    add(0x5d3ef000, Sam2~ES   & Str~NE)
    add(0x5d3e3000, Sam3~ES   & Str~NE)
    add(0x5d3e7000, Sam4~ES   & Str~NE)
    add(0x5d3f3000, Sam5~ES   & Str~NE)
    add(0x5d3f7000, Sam6~ES   & Str~NE)
    add(0x5d3eb000, Sam7~ES   & Str~NE)
    add(0x5d3fb000, Sam8~ES   & Str~NE)
    add(0x5d3ff000, Sam9~ES   & Str~NE)
    add(0x5d3d3000, Sam10~ES  & Str~NE)
    add(0x5e515b09, Sam11~ES  & Str~NE) // TODO: currently the only SAM set with a SAM IID for this crossing
    // -- Road --

    builder.result()
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)

}
