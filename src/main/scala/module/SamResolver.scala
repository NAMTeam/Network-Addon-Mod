package metarules.module

import scala.collection.immutable.ListMap

import metarules.meta._
import Network._
import RotFlip._
import Flags._
import group.SymGroup
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
      add(sam~(0,0,0,1), 0x5e573000 + offset)   // diag stub
      add(sam~(0,0,2,2), 0x5e590000 + offset)
      add(sam~(0,0,1,13), 0x5e571000 + offset)  // curve
      add(sam~(0,2,0,11), 0x5e570000 + offset)  // curve
      add(sam~(0,11,0,13), 0x5e577000 + offset)
      add(sam~(0,11,0,11), 0x5e578000 + offset)
		
	    // self-intersections	
      add(sam~NS & sam~WE, 0x5e527000 + offset)     // OxO
	  add(sam~(2,2,2,2), 0x5e527000 + offset)		// OxO alt
      add(sam~NS & sam~CE, 0x5e557000 + offset)     // OxO T
      add(sam~NS & sam~NE, 0x5e574000 + offset)     // OxD
      add(sam~SW & sam~ES, 0x5e579000 + offset)     // DxD

      add(sam~(0,0,2,11),	0x5e57a000 + offset)
      add(sam~(0,2,2,11),	0x5e57b000 + offset)
      add(sam~(0,2,11,2),	0x5e575000 + offset)
      add(sam~(0,2,2,13),	0x5e576000 + offset)
      add(sam~(2,2,2,13),	0x5e57c000 + offset)
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
      add(sam~NS & Glr1~WE, 0x5e538000 + offset)        // SAM-GLR 1
      add(sam~NS & Glr2~WE, 0x5e538080 + offset)        // SAM-GLR 2
      // add(sam~NS & Glr3~WE, 0x5e538005 + offset) // SAM-GLR 3
      // add(sam~NS & Glr4~WE, 0x5e538085 + offset) // SAM-GLR 4
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
	
      // OxO T-intersections
      add(sam~CS & Road~WE, 0x5e550000 + offset)        // Road Thru
      add(sam~NS & Road~CE, 0x5e551000 + offset)        // Road Ends
      add(sam~CS & Onewayroad~WE, 0x5e559000 + offset)  // Onewayroad Thru
      add(sam~NS & Onewayroad~CE, 0x5e55a000 + offset)  // Onewayroad Ends
		
      // OxD intersections		
      add(sam~WE & Road~SE, 0x5e555000 + offset)                  // SAM x Road
      add(sam~WE & Onewayroad~SE, 0x5e55d000 + offset)            // SAM x Road
      add(sam~WE & Avenue~ES, 0x5e558000 + offset)                // SAM x Avenue
      add(sam~NS & Avenue~SharedDiagRight, 0x5e558080 + offset)   // SAM x Avenue-Shared Diag Tile
      add(sam~WE & Highway~ES, 0x5e53e000 + offset)               // SAM x Highway
      add(sam~WE & Highway~SharedDiagRight, 0x5e53e080 + offset)  // SAM x Highway-Shared Diag Tile
      add(sam~NS & Rail~EN, 0x5e512000 + offset)                  // SAM-Rail
      add(sam~NS & Lightrail~SE, 0x5e518000 + offset)             // SAM x Lightrail
      add(sam~NS & Monorail~SE, 0x5e51c000 + offset)              // SAM x Monorail
      add(sam~NS & Str~EN, 0x5e512009 + offset)                   // SAM x STR
      // add(sam~NS & L1Dtr~EN, IID + offset) // SAM x L1 DTR
      // add(sam~NS & L2Dtr~EN, IID + offset) // SAM x L1 DTR
      add(sam~NS & Glr1~SE, 0x5e53a000 + offset)                  // SAM x GLR 1
      add(sam~NS & Glr2~SE, 0x5e53a080 + offset)                  // SAM x GLR 2
      // add(sam~NS & Glr3~SE, 0x5e53a009 + offset) // SAM x GLR 3
      // add(sam~NS & Glr4~SE, 0x5e53a089 + offset) // SAM x GLR 4
      add(sam~WE & (Tla3~SE).projectLeft, 0x5e670000 + offset)    // SAM x TLA-3
      add(sam~WE & (Tla3~SE).projectRight, 0x7e670000 + offset)   // SAM x TLA-3
      add(sam~WE & Ave2~SE, 0x5e671000 + offset) // SAM x AVE-2
      add(sam~WE & Ard3~ES, 0x5e672000 + offset) // SAM x ARD-3
      add(sam~WE & Ard3~SE, 0x5e672080 + offset) // SAM x ARD-3
      add(sam~WE & Owr1~ES, 0x5e673000 + offset) // SAM x OWR-1
      add(sam~WE & Owr3~SE, 0x5e674000 + offset) // SAM x OWR-3
      add(sam~WE & Nrd4~SE, 0x5e675000 + offset) // SAM x NRD-4

      //DxO Intersections		
      add(sam~SE & Road~NS, 0x5e582000 + offset)        // SAM x Road
      add(sam~SE & Onewayroad~NS, 0x5e58b000 + offset)  // SAM x Onewayroad
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

		
      //DxD Intersections		
      add(sam~EN & Road~SE, 0x5e584000 + offset)                  // SAM x Road
      add(sam~EN & Onewayroad~SE, 0x5e58e000 + offset)            // SAM x Onewayroad
      add(sam~ES & Avenue~NE,	0x5e589000 + offset)                // SAM x Avenue
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

		}
    map
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)

}