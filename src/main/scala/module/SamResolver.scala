package com.sc4nam.module

import scala.collection.immutable.ListMap

import io.github.memo33.metarules.meta._
import syntax._, Network._, RotFlip._, Flags._, group.SymGroup
import Implicits.segmentToTile
import NetworkProperties.{isSingleTile, isTripleTile, nonMirroredOnly, mirroredOnly}


class SamResolver extends IdResolver {

  val isSam = Set(Sam1, Sam2, Sam3, Sam4, Sam5, Sam6, Sam7, Sam8, Sam9, Sam10, Sam11)

  val isSimpleSam = Set(Sam1)

  val samOffsets = ListMap(
    Sam1  -> 0x100,
    Sam2  -> 0x200,
    Sam3  -> 0x300,
    Sam4  -> 0x400,
    Sam5  -> 0x500,
    Sam6  -> 0x600,
    Sam7  -> 0x700,
    Sam8  -> 0x800,
    Sam9  -> 0x900,
    Sam10 -> 0xa00,
    Sam11 -> 0xb00
    )

  val tileMap: scala.collection.Map[Tile, IdTile] = {
    val builder = new ResolverBuilder
    import builder.add

    // tiles which are defined for all SAM networks, including SAM-1
    for ((sam, offset) <- samOffsets) {

      // base
      add(0x5e54b000 + offset, sam~NS)           // orth
      add(0x5e500000 + offset, sam~CS)           // orth stub
      add(0x5e590000 + offset, sam~(0,0,2,2))    // 90 degree turn

      // self-intersections
      add(0x5e527000 + offset, sam~NS & sam~WE)  // OxO
      add(0x5e557000 + offset, sam~NS & sam~CE)  // OxO T

      // SAM x SAM transitions
      add(0x5e5c0000 + (offset * 0x11), Street~WC & sam~CE)
      for ((otherSam, otherOffset) <- samOffsets if otherOffset > offset) {
        add(0x5e5c0000 + (offset * 0x10) + otherOffset, sam~WC & otherSam~CE)
      }
    }

    // tiles which are only defined on SAM-2+
    for ((sam, offset) <- samOffsets.drop(1)) {

      // base
      add(0x5e572000 + offset, sam~SE)
      add(0x5e573000 + offset, sam~CES)         // diag stub
      add(0x5e571000 + offset, sam~(0,0,1,13))  // curve
      add(0x5e570000 + offset, sam~(0,2,0,11))  // curve
      add(0x5e577000 + offset, sam~(0,11,0,13))
      add(0x5e578000 + offset, sam~(0,11,0,11))
      add(0x5e56f000 + offset, sam~(0,0,11,13)) // part of 3x3 circle
      add(0x5e56e000 + offset, sam~(0,0,2,13))  // orth to diag kinked bend

      //smaller wide-radius curves
      //2x2 90
      add(0x5e5e8000 + offset, sam~(0,2,0,131))
      add(0x5e5e9000 + offset, sam~(143,0,0,141))
      add(0x5e5ea000 + offset, sam~(0,131,133,0))
      add(0x5e5ef000 + offset, sam~(2,2,0,131)) // T-Intersection off outer tile
      add(0x5e5eb000 + offset, sam~(133,131,133,131))  // diverter
      add(0x5e5ec000 + offset, sam~(0,131,133,0) & Road~(133,0,0,131))    // diverter w/ road
      add(0x5e5ed000 + offset, sam~(0,131,133,0) & Street~(133,0,0,131))  // diverter w/ street
      //3x2 S
      add(0x5e5b0000 + offset, sam~(2,0,153,0))
      add(0x5e5b1000 + offset, sam~(153,0,0,161))
      add(0x5e5b2000 + offset, sam~(173,0,0,181))
      add(0x5e5b3000 + offset, sam~(2,2,153,0)) //T-Intersection off outer tile
      // diagonal s-curve
      add(0x5e5d0000 + offset, sam~(3,0,0,152))
      add(0x5e5d1000 + offset, sam~(0,152,154,1))
      add(0x5e5d2000 + offset, sam~(154,0,0,1))

      //larger wide-radius curves
      //larger 45 (4x3)
      add(0x5e5e4000 + offset, sam~(0,2,0,111))
      add(0x5e5e3000 + offset, sam~(0,111,0,11))
      add(0x5e5e2000 + offset, sam~(15,0,0,14))
      add(0x5e5e1000 + offset, sam~(0,11,113,0))
      add(0x5e5e0000 + offset, sam~(113,0,0,1))
      //Larger 90 (4x4)
      add(0x5e5b5000 + offset, sam~(0,2,0,181)) //start here
      add(0x5e5b6000 + offset, sam~(0,181,11,191))
      add(0x5e5b7000 + offset, sam~(11,0,0,82))
      add(0x5e5b8000 + offset, sam~(0,191,194,0))
      add(0x5e5b9000 + offset, sam~(0,82,82,0))
      //T-ints off
      add(0x5e5bf000 + offset, sam~(2,2,0,181))
      add(0x5e5be000 + offset, sam~(2,181,11,191))

      // self-intersections
      add(0x5e527000 + offset, sam~(2,2,2,2))       // OxO alt
      add(0x5e574000 + offset, sam~NS & sam~NE)     // OxD
      add(0x5e579000 + offset, sam~SE & sam~EN)     // DxD
      add(0x5e575000 + offset, sam~NS & sam~CSE)    // OxD T (also 0,2,11,2)
      add(0x5e597000 + offset, sam~CS & sam~NE)     // DxO T1
      add(0x5e598000 + offset, sam~CS & sam~WS)     // DxO T2
      add(0x5e599000 + offset, sam~SE & sam~CEN)    // DxD T1
      add(0x5e59a000 + offset, sam~WN & sam~CSW)    // DxD T2

      add(0x5e57a000 + offset, sam~(0,0,2,11))
      // add(NoIID + offset, sam~(0,0,2,13))
      add(0x5e57b000 + offset, sam~(0,2,2,11))
      add(0x5e575000 + offset, sam~(0,2,11,2)) // alt version of OxD T
      add(0x5e576000 + offset, sam~(0,2,2,13))
      add(0x5e57c000 + offset, sam~(2,2,2,11))
      add(0x5e57d000 + offset, sam~(0,11,2,13))
      add(0x5e57e000 + offset, sam~(0,2,11,11))
      add(0x5e57f000 + offset, sam~(0,2,13,13))

      // OxO intersections
      add(0x5e520000 + offset, sam~NS & Road~WE)        // Road
      add(0x5e529000 + offset, sam~NS & Onewayroad~WE)  // Onewayoad
      add(0x5e524000 + offset, sam~WE & Avenue~SN)      // Avenue
      add(0x5e53c000 + offset, sam~WE & Highway~NS)     // SAM-Highway
      add(0x5e511000 + offset, sam~NS & Rail~WE)        // SAM-Rail
      add(0x5e516000 + offset, sam~WE & Lightrail~NS)   // SAM-Lightrail
      add(0x5e51a000 + offset, sam~WE & Monorail~NS)    // SAM-Monorail
      // add(0x5e511009 + offset, sam~NS & Str~WE)         // SAM-STR
      add(0x5e538000 + offset, sam~WE & Glr1~NS)        // SAM-GLR 1
      add(0x5e538080 + offset, sam~WE & Glr2~NS)        // SAM-GLR 2
      // add(0x5e538005 + offset, sam~WE & Glr3~NS) // SAM-GLR 3
      // add(0x5e538085 + offset, sam~WE & Glr4~NS) // SAM-GLR 4
      // add(IID + offset, sam~WE & L1Dtr~NS) // SAM-L1 DTR
      // add(IID + offset, sam~WE & L2Dtr~NS) // SAM-L2 DTR
      add(0x5e640000 + offset, sam~NS & Tla3~WE)  // TLA-3 +
      add(0x5e641000 + offset, sam~NS & Ave2~WE)  // AVE-2 +
      add(0x5e642000 + offset, sam~NS & Ard3~WE)  // ARD-3 +
      add(0x5e643000 + offset, sam~NS & Owr1~WE)  // OWR-1 +
      add(0x5e644000 + offset, sam~NS & Owr3~WE)  // OWR-3 +
      add(0x5e645000 + offset, sam~NS & Nrd4~WE)  // NRD-4 +

      add(0x5e646000 + offset, sam~NS & Tla5~EW)  // TLA-5 +
      add(0x5e647000 + offset, sam~NS & Owr4~EW)  // OWR-4 +
      add(0x5e648000 + offset, sam~NS & Owr5~EW)  // OWR-5 +
      add(0x5e649000 + offset, sam~NS & Rd4~EW)   // RD-4 +
      add(0x5e64a000 + offset, sam~NS & Rd6~EW)   // RD-6 +
      add(0x5e64b000 + offset, sam~NS & Ave6~EW)  // AVE-6 + (also TLA-7)
      add(0x5e64b080 + offset, sam~NS & Tla7m~WE) // TLA Inner +
      // add(0x5e64c000 + offset, sam~NS & Ave8~EW) // AVE-8 + (also TLA-9)
      add(0x5e64c080 + offset, sam~NS & Ave6m~WE)               // AVE Inner +
      add(0x5e64d000 + offset, sam~NS & Owr4m~EW)  // OWR-4 Inner

      add(0x5e600000 + offset, sam~WE & Dirtroad~NS) // RHW-2 +

      //Specialized OxO +-intersections
      //1 SAM and 3 Cross
      add(0x5e521000 + offset, sam~(0,0,0,2) & Road~(2,2,2,0))
      add(0x5e52a000 + offset, sam~(0,0,0,2) & Onewayroad~(2,2,2,0))
      //2 SAM and 2 Cross Elbow
      add(0x5e523000 + offset, sam~(0,0,2,2) & Road~(2,2,0,0))
      add(0x5e52c000 + offset, sam~(0,0,2,2) & Onewayroad~(2,2,0,0))
      //3 SAM and 1 Cross
      add(0x5e522000 + offset, sam~(2,2,0,2) & Road~(0,0,2,0))
      add(0x5e52b000 + offset, sam~(2,2,0,2) & Onewayroad~(0,0,2,0))
      add(0x5e52f000 + offset, sam~(2,0,2,2) & Avenue~NC)
      //2 SAM Elbow and Diag Cross
      add(0x5e52e000 + offset, sam~(2,2,0,0) & Road~(0,0,1,3))
      add(0x5e52d000 + offset, sam~(2,2,0,0) & Onewayroad~(0,0,1,3))

      // OxO T-intersections
      add(0x5e550000 + offset, sam~CS & Road~WE)        // Road Thru
      add(0x5e551000 + offset, sam~NS & Road~CE)        // Road Ends
      add(0x5e559000 + offset, sam~CS & Onewayroad~WE)  // Onewayroad Thru
      add(0x5e55a000 + offset, sam~NS & Onewayroad~CE)  // Onewayroad Ends
      add(0x5e554000 + offset, sam~CE & Avenue~SN) // Avenue Thru - Short
      add(0x5e556000 + offset, sam~WC & Avenue~SN) // Avenue Thru - Long
      add(0x5e55f000 + offset, sam~WE & Avenue~NC) // Avenue Ends
      add(0x5e650000 + offset, sam~CS & Tla3~WE)  // Tla3 Thru
      add(0x5e660000 + offset, sam~NS & Tla3~CE)  // Tla3 Ends
      add(0x5e651000 + offset, sam~CS & Ave2~WE)  // Ave2 Thru
      add(0x5e661000 + offset, sam~NS & Ave2~CE)  // Ave2 Ends
      add(0x5e652000 + offset, sam~CS & Ard3~WE)  // Ard3 Thru
      add(0x5e652080 + offset, sam~CS & Ard3~EW)  // Ard3 Thru
      add(0x5e662000 + offset, sam~NS & Ard3~CE)  // Ard3 Ends
      add(0x5e653000 + offset, sam~CS & Owr1~WE)  // Owr1 Thru
      add(0x5e663000 + offset, sam~NS & Owr1~CE)  // Owr1 Ends
      add(0x5e654000 + offset, sam~CS & Owr3~WE)  // Owr3 Thru
      add(0x5e664000 + offset, sam~NS & Owr3~CE)  // Owr3 Ends
      add(0x5e655000 + offset, sam~CS & Nrd4~WE)  // Nrd4 Thru
      add(0x5e665000 + offset, sam~NS & Nrd4~CE)  // Nrd4 Ends

      add(0x5e656000 + offset, sam~CN & Tla5~EW)  // Tla5 short T
      add(0x5e657000 + offset, sam~CN & Owr4~EW)  // Owr4 short T
      add(0x5e658000 + offset, sam~CN & Owr5~EW)  // Owr5 short T
      add(0x5e659000 + offset, sam~CN & Rd4~EW)   // Rd4 short T
      add(0x5e65a000 + offset, sam~CN & Rd6~EW)   // Rd6 short T
      add(0x5e65b000 + offset, sam~CN & Ave6~EW)  // Ave6 short T
      add(0x5e65d000 + offset, sam~CN & Owr4m~EW)  // Owr4m short T

      //Specialized OxO T-intersections
      //1 SAM and 2 Cross Elbow
      add(0x5e552000 + offset, sam~(0,0,0,2) & Road~(2,2,0,0))
      add(0x5e55b000 + offset, sam~(0,0,0,2) & Onewayroad~(2,2,0,0))
      add(0x5e560000 + offset, sam~(0,0,2,0) & Avenue~SC)

      //2 SAM and 1 Cross Elbow
      add(0x5e553000 + offset, sam~(2,2,0,0) & Road~(0,0,2,0))
      add(0x5e55c000 + offset, sam~(2,2,0,0) & Onewayroad~(0,0,2,0))

      // OxD intersections
      add(0x5e555000 + offset, sam~WE & Road~SE)                  // SAM x Road
      add(0x5e55d000 + offset, sam~WE & Onewayroad~SE)            // SAM x Onewayroad
      add(0x5e558000 + offset, sam~WC & Avenue~ES)                // SAM x Avenue Street-End Short
      add(0x5e558080 + offset, sam~WE & Avenue~ES)                // SAM x Avenue +/Long-T
      add(0x5e558089 + offset, sam~NS & Avenue~SharedDiagRight)   // SAM x Avenue-Shared Diag Tile
      add(0x5e53e000 + offset, sam~WE & Highway~ES)               // SAM x Highway
      add(0x5e53e080 + offset, sam~WE & Highway~SharedDiagRight)  // SAM x Highway-Shared Diag Tile
      add(0x5e512000 + offset, sam~NS & Rail~EN)                  // SAM-Rail
      add(0x5e518000 + offset, sam~NS & Lightrail~SE)             // SAM x Lightrail
      add(0x5e51c000 + offset, sam~NS & Monorail~SE)              // SAM x Monorail
      // add(0x5e512009 + offset, sam~NS & Str~EN)                   // SAM x STR
      // add(IID + offset, sam~NS & L1Dtr~EN) // SAM x L1 DTR
      // add(IID + offset, sam~NS & L2Dtr~EN) // SAM x L2 DTR
      add(0x5e539000 + offset, sam~NS & Glr1~EN)                  // SAM x GLR 1
      add(0x5e539080 + offset, sam~NS & Glr2~EN)                  // SAM x GLR 2
      // add(0x5e539009 + offset, sam~NS & Glr3~EN) // SAM x GLR 3
      // add(0x5e539089 + offset, sam~NS & Glr4~EN) // SAM x GLR 4
      add(0x5e670000 + offset, sam~WE & (Tla3~SE).projectLeft)    // SAM x TLA-3
      add(0x7e670000 + offset, sam~WE & (Tla3~SE).projectRight)   // SAM x TLA-3
      add(0x5e671000 + offset, sam~WE & Ave2~SE) // SAM x AVE-2
      add(0x5e672000 + offset, sam~WE & Ard3~ES) // SAM x ARD-3
      add(0x5e672080 + offset, sam~WE & Ard3~SE) // SAM x ARD-3
      add(0x5e673000 + offset, sam~WE & Owr1~ES) // SAM x OWR-1
      add(0x5e674000 + offset, sam~WE & Owr3~SE) // SAM x OWR-3
      add(0x5e675000 + offset, sam~WE & Nrd4~SE) // SAM x NRD-4
      add(0x5e55e000 + offset, sam~WE & Road~CWN) // SAM-Thru x Road-End T
      add(0x5e561000 + offset, sam~WE & Onewayroad~CWN) // SAM-Thru x Onewayroad-End T
      add(0x5e610000 + offset, sam~WE & Dirtroad~SE) // SAM x RHW-2

      //DxO Intersections
      add(0x5e582000 + offset, sam~SE & Road~NS)        // SAM x Road
      add(0x5e58c000 + offset, sam~SE & Onewayroad~NS)  // SAM x Onewayroad
      add(0x5e587000 + offset, sam~SE & Avenue~SN)      // SAM x Avenue 1
      add(0x5e588000 + offset, sam~NW & Avenue~SN)      // SAM x Avenue 2
      add(0x5e53d000 + offset, sam~SE & Highway~NS)     // SAM x Highway 1
      add(0x5e53d080 + offset, sam~WN & Highway~NS)     // SAM x Highway 2
      add(0x5e514000 + offset, sam~NW & Rail~NS)        // SAM-Rail
      add(0x5e517000 + offset, sam~SE & Lightrail~NS)   // SAM x Lightrail
      add(0x5e51b000 + offset, sam~SE & Monorail~NS)    // SAM x Monorail
      // add(0x5e514009 + offset, sam~NW & Str~NS)         // SAM x STR
      // add(IID + offset, sam~NW & L1Dtr~NS) // SAM x L1 DTR
      // add(IID + offset, sam~NW & L2Dtr~NS) // SAM x L2 DTR
      add(0x5e53a000 + offset, sam~NW & Glr1~NS)        // SAM x GLR 1
      add(0x5e53a080 + offset, sam~NW & Glr2~NS)        // SAM x GLR 2
      // add(0x5e53a009 + offset, sam~NW & Glr3~NS)     // SAM x GLR 3
      // add(0x5e53a089 + offset, sam~NW & Glr4~NS)     // SAM x GLR 4
      add(0x5e680000 + offset, sam~SE & (Tla3~NS).projectLeft)  // SAM x Tla3
      add(0x7e680000 + offset, sam~SE & (Tla3~SN).projectRight) // SAM x Tla3
      add(0x5e681000 + offset, sam~SE & Ave2~NS)  // SAM x Ave2
      add(0x5e682000 + offset, sam~SE & Ard3~NS)  // SAM x Ard3
      add(0x5e682080 + offset, sam~SE & Ard3~SN)  // SAM x Ard3
      add(0x5e683000 + offset, sam~SE & Owr1~NS)  // SAM x Owr1
      add(0x5e684000 + offset, sam~SE & Owr3~NS)  // SAM x Owr3
      add(0x5e685000 + offset, sam~SE & Nrd4~NS)  // SAM x Nrd4

      add(0x5e620000 + offset, sam~WS & Dirtroad~NS)  // SAM x Rhw2

      add(0x5e581000 + offset, Road~NS & sam~CSE) // SAM-End Road T-int
      add(0x5e58b000 + offset, Onewayroad~NS & sam~CSE) // SAM-End OWR T-int

      add(0x5e583000 + offset, Road~CS & sam~NE) // SAM-Thru Road T-int
      add(0x5e58d000 + offset, Onewayroad~CS & sam~NE) // SAM-Thru OWR T-int


      //DxD Intersections
      add(0x5e584000 + offset, sam~EN & Road~SE)                  // SAM x Road
      add(0x5e58e000 + offset, sam~EN & Onewayroad~SE)            // SAM x Onewayroad
      add(0x5e589000 + offset, sam~ES & Avenue~NE)                // SAM x Avenue
      add(0x5e58a000 + offset, sam~SE & Avenue~SharedDiagLeft)    // SAM x Avenue-Shared Diag Tile
      add(0x5e53f000 + offset, sam~EN & Highway~SE)               // SAM x Highway 1
      add(0x5e53f080 + offset, sam~SW & Highway~SharedDiagRight)  // SAM x Highway 2
      add(0x5e515000 + offset, sam~NW & Rail~EN)                  // SAM-Rail
      add(0x5e519000 + offset, sam~SW & Lightrail~ES)             // SAM x Lightrail
      add(0x5e51d000 + offset, sam~SW & Monorail~ES)              // SAM x Monorail
      // add(0x5e515009 + offset, sam~NW & Str~EN)                   // SAM x STR
      // add(IID + offset, sam~NW & L1Dtr~EN) // SAM x L1 DTR
      // add(IID + offset, sam~NW & L2Dtr~EN) // SAM x L2 DTR
      add(0x5e53b000 + offset, sam~NW & Glr1~EN)                  // SAM x GLR 1
      add(0x5e53b080 + offset, sam~NW & Glr2~EN)                  // SAM x GLR 2
      // add(0x5e53b009 + offset, sam~NW & Glr3~EN) // SAM x GLR 3
      // add(0x5e53b089 + offset, sam~NW & Glr4~EN) // SAM x GLR 4
      add(0x5e6a0000 + offset, sam~EN & (Tla3~SE).projectLeft)    // SAM x Tla3
      add(0x7e6a0000 + offset, sam~EN & (Tla3~SE).projectRight)   // SAM x Tla3
      add(0x5e6a1000 + offset, sam~EN & Ave2~SE)                  // SAM x Ave2
      add(0x5e6a2000 + offset, sam~EN & Ard3~SE)                  // SAM x Ard3
      add(0x5e6a2080 + offset, sam~EN & Ard3~ES)                  // SAM x Ard3
      add(0x5e6a3000 + offset, sam~EN & Owr1~SE)                  // SAM x Owr1
      add(0x5e6a4000 + offset, sam~EN & Owr3~SE)                  // SAM x Owr3
      add(0x5e6a5000 + offset, sam~EN & Nrd4~SE)                  // SAM x Nrd4

      add(0x5e630000 + offset, sam~WS & Dirtroad~SE)              // SAM x Rhw2

      add(0x5e584000 + offset, Road~ES & sam~CEN) // temporary IID
      add(0x5e58e000 + offset, Onewayroad~ES & sam~CEN) // temporary IID

      add(0x5e585000 + offset, sam~NE & Road~CES) // SAM x Road (SAM Thru)
      add(0x5e58f000 + offset, sam~NE & Onewayroad~CES) // SAM x Onewayroad (SAM Thru)

      //Transitions
      //Ortho
      add(0x5e54a000 + offset, sam~CS & Road~NC)
      add(0x5e549000 + offset, sam~CS & Onewayroad~NC)
      add(0x5e54c000 + offset, sam~(0,0,0,2) & Avenue~NC)

      //Diag
      add(0x5e580000 + offset, sam~CSE & Road~CES)
      // add(NoIID + offset, sam~CSE & Onewayroad~CES)

      //Bending
      add(0x5e591000 + offset, sam~NC & Road~CE)
      add(0x5e592000 + offset, sam~NC & Onewayroad~CE)
      add(0x5e593000 + offset, sam~WC & Road~CWN)
      add(0x5e594000 + offset, sam~CE & Road~CWN)
      add(0x5e595000 + offset, sam~WC & Onewayroad~CWN)
      add(0x5e596000 + offset, sam~CE & Onewayroad~CWN)

      //Street Roundabouts
      add(0x5e5a1000 + offset, sam~(0,0,102,102))                 // base - no connections
      add(0x5e5a5000 + offset, sam~(0,0,102,102) & sam~NC)        // orth sam connection
      add(0x5e5a0000 + offset, sam~(0,0,102,102) & Road~NC)       // orth road connection
      add(0x5e5a9000 + offset, sam~(0,0,102,102) & Onewayroad~NC) // orth onewayroad connection

      // add(NoIID + offset, sam~(0,0,102,102) & sam~(0,11,0,0))       // diag sam connection
      // add(NoIID + offset, sam~(0,0,102,102) & Road~(0,11,0,0))      // diag road connection
      // add(NoIID + offset, sam~(0,0,102,102) & Onewayoad~(0,11,0,0)) // diag onewayroad connection

    }
    builder.result()
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)

}