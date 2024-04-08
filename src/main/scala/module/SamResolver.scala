package com.sc4nam.module

import scala.collection.immutable.ListMap

import io.github.memo33.metarules.meta._
import syntax._, Network._, RotFlip._, Flags._, group.SymGroup
import Implicits.segmentToTile
import NetworkProperties.{isSingleTile, isTripleTile, nonMirroredOnly, mirroredOnly}


class SamResolver extends IdResolver {

  val isSam = Set(Sam1, Sam2, Sam3, Sam4, Sam5, Sam6, Sam7, Sam8, Sam9, Sam10, Sam11)

  val isSimpleSam = Set(Sam1)

  val samOffset = ListMap(
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
    val map = scala.collection.mutable.Map.empty[Tile, IdTile]
    def add(tile: Tile, id: Int, mappedRepr: group.Quotient => Set[RotFlip] = null): Unit = {
      assert(!map.contains(tile))
      for (rf <- RotFlip.values) {
        val idTile = if (mappedRepr == null) IdTile(id, rf) else IdTile(id, rf, mappedRepr)
        map.getOrElseUpdate(tile * rf, idTile)
        }
      }

    for ((sam, offset) <- samOffset.drop(1)) {

      // base
      add(sam~NS, 0x5e54b000 + offset)
      add(sam~SE,	0x5e572000 + offset)
      add(sam~CS, 0x5e500000 + offset)          // orth stub
      add(sam~CES, 0x5e573000 + offset)         // diag stub
      add(sam~(0,0,2,2), 0x5e590000 + offset)
      add(sam~(0,0,1,13), 0x5e571000 + offset)  // curve
      add(sam~(0,2,0,11), 0x5e570000 + offset)  // curve
      add(sam~(0,11,0,13), 0x5e577000 + offset)
      add(sam~(0,11,0,11), 0x5e578000 + offset)
      // add(sam~(0,0,11,13), NoIID + offset) // part of 3x3 circle

      //smaller wide-radius curves
      //2x2 90
      add(sam~(0,2,0,131), 0x5e5e8000 + offset)
      add(sam~(143,0,0,141), 0x5e5e9000 + offset)
      add(sam~(0,131,133,0), 0x5e5ea000 + offset)
      add(sam~(2,2,0,131), 0x5e5ef000 + offset) // T-Intersection off outer tile
      add(sam~(133,131,133,131), 0x5e5eb000 + offset)  // diverter
      add(sam~(0,131,133,0) & Road~(133,0,0,131), 0x5e5ec000 + offset)    // diverter w/ road
      add(sam~(0,131,133,0) & Street~(133,0,0,131), 0x5e5ed000 + offset)  // diverter w/ street
      //3x2 S
      add(sam~(2,0,153,0), 0x5e5b0000 + offset)
      add(sam~(153,0,0,161), 0x5e5b1000 + offset)
      add(sam~(173,0,0,181), 0x5e5b2000 + offset)
      add(sam~(2,2,153,0), 0x5e5b3000 + offset) //T-Intersection off outer tile

      //larger wide-radius curves
      //larger 45 (4x3)
      add(sam~(0,2,0,111), 0x5e5e4000 + offset)
      add(sam~(0,111,0,11), 0x5e5e3000 + offset)
      add(sam~(15,0,0,14), 0x5e5e2000 + offset)
      add(sam~(0,11,113,0), 0x5e5e1000 + offset)
      add(sam~(113,0,0,1), 0x5e5e0000 + offset)
      //Larger 90 (4x4)
      add(sam~(0,2,0,181), 0x5e5b5000 + offset) //start here
      add(sam~(0,181,11,191), 0x5e5b6000 + offset)
      add(sam~(11,0,0,82), 0x5e5b7000 + offset)
      add(sam~(0,191,193,0), 0x5e5b8000 + offset)
      add(sam~(0,82,82,0), 0x5e5b9000 + offset)
      //T-ints off
      add(sam~(2,2,0,181), 0x5e5bf000 + offset)
      add(sam~(2,181,11,191), 0x5e5be000 + offset)

	    // self-intersections
      add(sam~NS & sam~WE, 0x5e527000 + offset)     // OxO
      add(sam~(2,2,2,2), 0x5e527000 + offset)		// OxO alt
      add(sam~NS & sam~NE, 0x5e574000 + offset)     // OxD
      add(sam~SE & sam~EN, 0x5e579000 + offset)     // DxD
      add(sam~NS & sam~CE, 0x5e557000 + offset)     // OxO T
      add(sam~NS & sam~CSE, 0x5e575000 + offset) 	// OxD T (also 0,2,11,2)
      add(sam~CS & sam~NE, 0x5e597000 + offset) 	// DxO T1
      add(sam~CS & sam~WS, 0x5e598000 + offset) 	// DxO T2
      add(sam~SE & sam~CEN, 0x5e599000 + offset) 	// DxD T1
      add(sam~(1,3,0,0) & sam~(0,1,0,0), 0x5e59a000 + offset) 	// DxD T2

      add(sam~(0,0,2,11),	0x5e57a000 + offset)
	  // add(sam~(0,0,2,13),   NoIID + offset)
      add(sam~(0,2,2,11),	0x5e57b000 + offset)
      add(sam~(0,2,11,2),	0x5e575000 + offset) // alt version of OxD T
      add(sam~(0,2,2,13),	0x5e576000 + offset)
      add(sam~(2,2,2,11),	0x5e57c000 + offset)
      add(sam~(0,11,2,13), 0x5e57d000 + offset)
      add(sam~(0,2,11,11), 0x5e57e000 + offset)
      add(sam~(0,2,13,13), 0x5e57f000 + offset)

      // OxO intersections
      add(sam~NS & Road~WE, 0x5e520000 + offset)        // Road
      add(sam~NS & Onewayroad~WE, 0x5e529000 + offset)  // Onewayoad
      add(sam~WE & Avenue~SN, 0x5e524000 + offset)      // Avenue
      add(sam~WE & Highway~NS, 0x5e53c000 + offset)     // SAM-Highway
      add(sam~NS & Rail~WE, 0x5e511000 + offset)        // SAM-Rail
      add(sam~WE & Lightrail~NS, 0x5e516000 + offset)   // SAM-Lightrail
      add(sam~WE & Monorail~NS, 0x5e51a000 + offset)    // SAM-Monorail
      add(sam~NS & Str~WE, 0x5e511009 + offset)         // SAM-STR
      add(sam~WE & Glr1~NS, 0x5e538000 + offset)        // SAM-GLR 1
      add(sam~WE & Glr2~NS, 0x5e538080 + offset)        // SAM-GLR 2
      // add(sam~WE & Glr3~NS, 0x5e538005 + offset) // SAM-GLR 3
      // add(sam~WE & Glr4~NS, 0x5e538085 + offset) // SAM-GLR 4
      // add(sam~WE & L1Dtr~NS, IID + offset) // SAM-L1 DTR
      // add(sam~WE & L2Dtr~NS, IID + offset) // SAM-L2 DTR
      // check why this code is problematic
      // add(sam~NS & Tla3~WE, 0x5e640000 + offset) // TLA-3 +
      add(sam~NS & (Tla3~WE).projectLeft, 0x5e640000 + offset)  // TLA-3 +
      add(sam~NS & (Tla3~WE).projectRight, 0x5e640000 + offset) // TLA-3 +
      add(sam~NS & Ave2~WE, 0x5e641000 + offset)                // AVE-2 +
      add(sam~NS & Ard3~WE, 0x5e642000 + offset)                // ARD-3 +
      add(sam~NS & Owr1~WE, 0x5e643000 + offset)                // OWR-1 +
      add(sam~NS & Owr3~WE, 0x5e644000 + offset)                // OWR-3 +
      add(sam~NS & Nrd4~WE, 0x5e645000 + offset)                // NRD-4 +
      /*
      add(sam~NS & (Tla5~EW).projectLeft, 0x5e646000 + offset) // TLA-5 +
      add(sam~NS & Owr4~EW, 0x5e647000 + offset) // OWR-4 +
      add(sam~NS & Owr5~EW, 0x5e648000 + offset) // OWR-5 +
      add(sam~NS & Rd4~EW, 0x5e649000 + offset) // RD-4 +
      add(sam~NS & Rd6~EW, 0x5e64a000 + offset) // RD-6 +
      add(sam~NS & Ave6~EW, 0x5e64b000 + offset) // AVE-6 + (also TLA-7)
      add(sam~NS & Tla7m~WE, 0x5e64b080 + offset) // TLA Inner +
      // add(sam~NS & Ave8~EW, 0x5e64c000 + offset) // AVE-8 + (also TLA-9)
      add(sam~NS & Ave6m~WE, 0x5e64c080 + offset) // AVE Inner +
      */
      add(sam~WE & Dirtroad~NS, 0x5e600000 + offset) // RHW-2 +

      //Specialized OxO +-intersections
      //1 SAM and 3 Cross
      add(sam~(0,0,0,2) & Road~(2,2,2,0), 0x5e521000 + offset)
      add(sam~(0,0,0,2) & Onewayroad~(2,2,2,0), 0x5e52a000 + offset)
      //2 SAM and 2 Cross Elbow
      add(sam~(0,0,2,2) & Road~(2,2,0,0), 0x5e523000 + offset)
      add(sam~(0,0,2,2) & Onewayroad~(2,2,0,0), 0x5e52c000 + offset)
      //3 SAM and 1 Cross
      add(sam~(2,2,0,2) & Road~(0,0,2,0), 0x5e522000 + offset)
      add(sam~(2,2,0,2) & Onewayroad~(0,0,2,0), 0x5e52b000 + offset)
      add(sam~(2,0,2,2) & Avenue~NC, 0x5e52f000 + offset)
      //2 SAM Elbow and Diag Cross
      add(sam~(2,2,0,0) & Road~(0,0,1,3), 0x5e52e000 + offset)
      add(sam~(2,2,0,0) & Onewayroad~(0,0,1,3), 0x5e52d000 + offset)

      // OxO T-intersections
      add(sam~CS & Road~WE, 0x5e550000 + offset)        // Road Thru
      add(sam~NS & Road~CE, 0x5e551000 + offset)        // Road Ends
      add(sam~CS & Onewayroad~WE, 0x5e559000 + offset)  // Onewayroad Thru
      add(sam~NS & Onewayroad~CE, 0x5e55a000 + offset)  // Onewayroad Ends
      add(sam~CE & Avenue~SN, 0x5e554000 + offset) // Avenue Thru - Short
      add(sam~WC & Avenue~SN, 0x5e556000 + offset) // Avenue Thru - Long
      add(sam~WE & Avenue~NC, 0x5e55f000 + offset) // Avenue Ends

      //Specialized OxO T-intersections
      //1 SAM and 2 Cross Elbow
      add(sam~(0,0,0,2) & Road~(2,2,0,0), 0x5e552000 + offset)
      add(sam~(0,0,0,2) & Onewayroad~(2,2,0,0), 0x5e55b000 + offset)
      add(sam~(0,0,2,0) & Avenue~(4,0,0,2), 0x5e560000 + offset)

      //2 SAM and 1 Cross Elbow
      add(sam~(2,2,0,0) & Road~(0,0,2,0), 0x5e553000 + offset)
      add(sam~(2,2,0,0) & Onewayroad~(0,0,2,0), 0x5e55c000 + offset)

      // OxD intersections
      add(sam~WE & Road~SE, 0x5e555000 + offset)                  // SAM x Road
      add(sam~WE & Onewayroad~SE, 0x5e55d000 + offset)            // SAM x Onewayroad
      add(sam~WC & Avenue~ES, 0x5e558000 + offset)                // SAM x Avenue Street-End Short
      add(sam~WE & Avenue~ES, 0x5e558080 + offset)                // SAM x Avenue +/Long-T
      add(sam~WC & Avenue~(0,0,+1,-3), 0x5e558000 + offset)       // SAM x Avenue Street-End Short
      add(sam~WE & Avenue~(0,0,+1,-3), 0x5e558080 + offset)       // SAM x Avenue
      add(sam~NS & Avenue~SharedDiagRight, 0x5e558089 + offset)   // SAM x Avenue-Shared Diag Tile
      add(sam~WE & Highway~ES, 0x5e53e000 + offset)               // SAM x Highway
      add(sam~WE & Highway~SharedDiagRight, 0x5e53e080 + offset)  // SAM x Highway-Shared Diag Tile
      add(sam~NS & Rail~EN, 0x5e512000 + offset)                  // SAM-Rail
      add(sam~NS & Lightrail~SE, 0x5e518000 + offset)             // SAM x Lightrail
      add(sam~NS & Monorail~SE, 0x5e51c000 + offset)              // SAM x Monorail
      add(sam~NS & Str~EN, 0x5e512009 + offset)                   // SAM x STR
      // add(sam~NS & L1Dtr~EN, IID + offset) // SAM x L1 DTR
      // add(sam~NS & L2Dtr~EN, IID + offset) // SAM x L2 DTR
      add(sam~NS & Glr1~EN, 0x5e539000 + offset)                  // SAM x GLR 1
      add(sam~NS & Glr2~EN, 0x5e539080 + offset)                  // SAM x GLR 2
      // add(sam~NS & Glr3~EN, 0x5e539009 + offset) // SAM x GLR 3
      // add(sam~NS & Glr4~EN, 0x5e539089 + offset) // SAM x GLR 4
      add(sam~WE & (Tla3~SE).projectLeft, 0x5e670000 + offset)    // SAM x TLA-3
      add(sam~WE & (Tla3~SE).projectRight, 0x7e670000 + offset)   // SAM x TLA-3
      add(sam~WE & Ave2~SE, 0x5e671000 + offset) // SAM x AVE-2
      add(sam~WE & Ard3~ES, 0x5e672000 + offset) // SAM x ARD-3
      add(sam~WE & Ard3~SE, 0x5e672080 + offset) // SAM x ARD-3
      add(sam~WE & Owr1~ES, 0x5e673000 + offset) // SAM x OWR-1
      add(sam~WE & Owr3~SE, 0x5e674000 + offset) // SAM x OWR-3
      add(sam~WE & Nrd4~SE, 0x5e675000 + offset) // SAM x NRD-4
      add(sam~WE & Road~CWN, 0x5e55e000 + offset) // SAM-Thru x Road-End T
      add(sam~WE & Onewayroad~CWN, 0x5e561000 + offset) // SAM-Thru x Onewayroad-End T
      add(sam~WE & Dirtroad~SE, 0x5e610000 + offset) // SAM x RHW-2

      //DxO Intersections
      add(sam~SE & Road~NS, 0x5e582000 + offset)        // SAM x Road
      add(sam~SE & Onewayroad~NS, 0x5e58c000 + offset)  // SAM x Onewayroad
      add(sam~SE & Avenue~SN,	0x5e587000 + offset)      // SAM x Avenue 1
      add(sam~NW & Avenue~SN,	0x5e588000 + offset)      // SAM x Avenue 2
      add(sam~SE & Highway~NS, 0x5e53d000 + offset)     // SAM x Highway 1
      add(sam~WN & Highway~NS, 0x5e53d080 + offset)     // SAM x Highway 2
      add(sam~NW & Rail~NS, 0x5e514000 + offset)        // SAM-Rail
      add(sam~SE & Lightrail~NS, 0x5e517000 + offset)   // SAM x Lightrail
      add(sam~SE & Monorail~NS, 0x5e51b000 + offset)    // SAM x Monorail
      add(sam~NW & Str~NS, 0x5e514009 + offset)         // SAM x STR
      // add(sam~NW & L1Dtr~NS, IID + offset) // SAM x L1 DTR
      // add(sam~NW & L2Dtr~NS, IID + offset) // SAM x L2 DTR
      add(sam~NW & Glr1~NS, 0x5e53a000 + offset)        // SAM x GLR 1
      add(sam~NW & Glr2~NS, 0x5e53a080 + offset)        // SAM x GLR 2
      // add(sam~NW & Glr3~NS, 0x5e53a009 + offset)     // SAM x GLR 3
      // add(sam~NW & Glr4~NS, 0x5e53a089 + offset)     // SAM x GLR 4
      add(sam~SE & (Tla3~NS).projectLeft, 0x5e680000 + offset)  // SAM x Tla3
      add(sam~SE & (Tla3~SN).projectRight, 0x7e680000 + offset) // SAM x Tla3
      add(sam~SE & Ave2~NS, 0x5e681000 + offset)  // SAM x Ave2
      add(sam~SE & Ard3~NS, 0x5e682000 + offset)  // SAM x Ard3
      add(sam~SE & Ard3~SN, 0x5e682080 + offset)  // SAM x Ard3
      add(sam~SE & Owr1~NS, 0x5e683000 + offset)  // SAM x Owr1
      add(sam~SE & Owr3~NS, 0x5e684000 + offset)  // SAM x Owr3
      add(sam~SE & Nrd4~NS, 0x5e685000 + offset)  // SAM x Nrd4

      add(sam~WS & Dirtroad~NS, 0x5e620000 + offset)  // SAM x Rhw2

      add(Road~NS & sam~CSE, 0x5e581000 + offset) // SAM-End Road T-int
      add(Onewayroad~NS & sam~CSE, 0x5e58b000 + offset) // SAM-End OWR T-int

      add(Road~CS & sam~NE, 0x5e583000 + offset) // SAM-Thru Road T-int
      add(Onewayroad~CS & sam~NE, 0x5e58d000 + offset) // SAM-Thru OWR T-int


      //DxD Intersections
      add(sam~EN & Road~SE, 0x5e584000 + offset)                  // SAM x Road
      add(sam~EN & Onewayroad~SE, 0x5e58e000 + offset)            // SAM x Onewayroad
      add(sam~ES & Avenue~NE,	0x5e589000 + offset)                // SAM x Avenue
      add(sam~ES & Avenue~(0,+1,-3,0),	0x5e589000 + offset)                // SAM x Avenue
      add(sam~SE & Avenue~SharedDiagLeft, 0x5e58a000 + offset)    // SAM x Avenue-Shared Diag Tile
      add(sam~EN & Highway~SE, 0x5e53f000 + offset)               // SAM x Highway 1
      add(sam~SW & Highway~SharedDiagRight, 0x5e53f080 + offset)  // SAM x Highway 2
      add(sam~NW & Rail~EN, 0x5e515000 + offset)                  // SAM-Rail
      add(sam~SW & Lightrail~ES, 0x5e519000 + offset)             // SAM x Lightrail
      add(sam~SW & Monorail~ES, 0x5e51d000 + offset)              // SAM x Monorail
      add(sam~NW & Str~EN, 0x5e515009 + offset)                   // SAM x STR
      // add(sam~NW & L1Dtr~EN, IID + offset) // SAM x L1 DTR
      // add(sam~NW & L2Dtr~EN, IID + offset) // SAM x L2 DTR
      add(sam~NW & Glr1~EN, 0x5e53b000 + offset)                  // SAM x GLR 1
      add(sam~NW & Glr2~EN, 0x5e53b080 + offset)                  // SAM x GLR 2
      // add(sam~NW & Glr3~EN, 0x5e53b009 + offset) // SAM x GLR 3
      // add(sam~NW & Glr4~EN, 0x5e53b089 + offset) // SAM x GLR 4
      add(sam~EN & (Tla3~SE).projectLeft, 0x5e6a0000 + offset)    // SAM x Tla3
      add(sam~EN & (Tla3~SE).projectRight, 0x7e6a0000 + offset)   // SAM x Tla3
      add(sam~EN & Ave2~SE, 0x5e6a1000 + offset)                  // SAM x Ave2
      add(sam~EN & Ard3~SE, 0x5e6a2000 + offset)                  // SAM x Ard3
      add(sam~EN & Ard3~ES, 0x5e6a2080 + offset)                  // SAM x Ard3
      add(sam~EN & Owr1~SE, 0x5e6a3000 + offset)                  // SAM x Owr1
      add(sam~EN & Owr3~SE, 0x5e6a4000 + offset)                  // SAM x Owr3
      add(sam~EN & Nrd4~SE, 0x5e6a5000 + offset)                  // SAM x Nrd4

      add(sam~WS & Dirtroad~SE, 0x5e630000 + offset)                  // SAM x Rhw2

      add(Road~ES & sam~CEN, 0x5e584000 + offset)				// temporary IID
      add(Onewayroad~ES & sam~CEN, 0x5e58e000 + offset)			// temporary IID

      add(sam~NE & Road~CES, 0x5e585000 + offset)				// SAM x Road (SAM Thru)
      add(sam~NE & Onewayroad~CES, 0x5e58f000 + offset)			// SAM x Onewayroad (SAM Thru)

      //Transitions
      //Ortho
      add(sam~CS & Road~NC, 0x5e54a000 + offset)
      add(sam~CS & Onewayroad~NC, 0x5e549000 + offset)
      add(sam~(0,0,0,2) & Avenue~(0,2,4,0), 0x5e54c000 + offset)

      //Diag
      add(sam~CSE & Road~CES, 0x5e580000 + offset)
      // add(sam~CSE & Onewayroad~CES, NoIID + offset)

      //Bending
      add(sam~NC & Road~CE, 0x5e591000 + offset)
      add(sam~NC & Onewayroad~CE, 0x5e592000 + offset)
      add(sam~WC & Road~CWN, 0x5e593000 + offset)
      add(sam~CE & Road~CWN, 0x5e594000 + offset)
      add(sam~WC & Onewayroad~CWN, 0x5e595000 + offset)
      add(sam~CE & Onewayroad~CWN, 0x5e596000 + offset)

      //Street Roundabouts
      add(sam~(0,0,102,102), 0x5e5a1000 + offset) //Base
      add(sam~(0,2,102,102), 0x5e5a5000 + offset) //Base with Orth Street connection
      // add(sam~(0,13,102,102), 0x5e5a1000 + offset) //Base with Diag Street connection (does not exist yet)

      add(Road~(0,2,0,0) & sam~(0,0,102,102), 0x5e5a0000 + offset) //Base with Orth Road connection
      // add(Road~(0,13,0,0) & sam~(0,0,102,102), 0x5F084900) //Base with Diag Road connection (does not exist yet)
      add(Onewayroad~(0,2,0,0) & sam~(0,0,102,102), 0x5e5a9000 + offset) //Base with Orth Onewayroad connection
      // add(Onewayroad~(0,13,0,0) & sam~(0,0,102,102), 0x5F984900) //Base with Diag Onewayroad connection (does not exist yet)

    }
    map
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)

}