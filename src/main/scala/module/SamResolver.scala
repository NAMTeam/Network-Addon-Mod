package metarules.module

import metarules.meta._
import Network._
import RotFlip._
import Flags._
import group.SymGroup
import Implicits.segmentToTile
import NetworkProperties.{isSingleTile, isTripleTile, nonMirroredOnly, mirroredOnly}


class SamResolver extends IdResolver {

	val isSimpleSam = Set(Sam1)
  
	val tileMap: scala.collection.Map[Tile, IdTile] = {
		val map = scala.collection.mutable.Map.empty[Tile, IdTile]
		def add(tile: Tile, id: Int, mappedRepr: group.Quotient => Set[RotFlip] = null): Unit = {
			assert(!map.contains(tile))
			for (rf <- RotFlip.values) {
				val idTile = if (mappedRepr == null) IdTile(id, rf) else IdTile(id, rf, mappedRepr)
				map.getOrElseUpdate(tile * rf, idTile)
			}
		}

    for ((sam, offset) <- Seq(Sam2, Sam3, Sam4, Sam5, Sam6, Sam7, Sam8, Sam9, Sam10, Sam11).zip(Seq(0x200, 0x300, 0x400, 0x500, 0x600, 0x700, 0x800, 0x900, 0xa00, 0xb00))) {
    // base

		add(sam~NS, 0x5e54b000 + offset)
		add(sam~SE,	0x5e572000 + offset)
		add(sam~CS, 0x5e500000 + offset) // orth stub
		add(sam~(0,0,0,1), 0x5e573000 + offset) // diag stub
		add(sam~(0,0,2,2), 0x5e590000 + offset)
		add(sam~(0,0,1,13), 0x5e571000 + offset) // curve
		add(sam~(0,2,0,11), 0x5e570000 + offset) // curve
		add(sam~(0,11,0,13), 0x5e577000 + offset)
		add(sam~(0,11,0,11), 0x5e578000 + offset)
		
	// self-intersections	
		add(sam~NS & sam~WE, 0x5e527000 + offset) // OxO
		add(sam~NS & Street~WE, 0x5e527000 + offset) // OxO
		
		add(sam~NS & sam~CE, 0x5e557000 + offset) // OxO T
		add(sam~NS & Street~CE, 0x5e557000 + offset) // OxO T
		add(Street~NS & sam~CE, 0x5e557000 + offset) // OxO T

		add(sam~NS & sam~NE, 0x5e574000 + offset) // OxD
		add(sam~NS & Street~NE, 0x5e574000 + offset) // OxD
		add(Street~NS & sam~NE, 0x5e574000 + offset) // OxD

		add(sam~SW & sam~ES, 0x5e579000 + offset) // DxD
		add(sam~SW & Street~ES, 0x5e579000 + offset) // DxD

		add(sam~(0,0,2,11),	0x5e57a000 + offset)
		add(sam~(0,2,2,11),	0x5e57b000 + offset)
		add(sam~(0,2,11,2),	0x5e575000 + offset)
		add(sam~(0,2,2,13),	0x5e576000 + offset)
		add(sam~(2,2,2,13),	0x5e57c000 + offset)
		add(sam~(0,11,2,13), 0x5e57d000 + offset)
		add(sam~(0,2,11,11), 0x5e57e000 + offset)
		add(sam~(0,2,13,13), 0x5e57f000 + offset)
		
	// OxO intersections		
		add(sam~NS & Road~WE, 0x5e520000 + offset) // Road
		add(sam~NS & Onewayroad~WE, 0x5e529000 + offset) // Onewayoad
		add(sam~NS & Rail~WE, 0x5e511000 + offset) // SAM-Rail
		add(sam~WE & Lightrail~NS, 0x5e516000 + offset) // SAM-Lightrail
		add(sam~WE & Monorail~NS, 0x5e51a000 + offset) // SAM-Monorail
	
	// OxO T-intersections
		add(sam~CS & Road~WE, 0x5e550000 + offset) // Road Thru
		add(sam~NS & Road~CE, 0x5e551000 + offset) // Road Ends
		add(sam~CS & Onewayroad~WE, 0x5e559000 + offset) // Onewayroad Thru
		add(sam~NS & Onewayroad~CE, 0x5e55a000 + offset) // Onewayroad Ends
		
	// OxD intersections		
		add(sam~WE & Road~SE, 0x5e555000 + offset) // SAM x Road
		add(sam~WE & Onewayroad~SE, 0x5e55d000 + offset) // SAM x Road
		add(sam~NS & Rail~EN, 0x5e512000 + offset) // SAM-Rail
		add(sam~NS & Lightrail~SE, 0x5e518000 + offset) // SAM x Lightrail
		add(sam~NS & Monorail~SE, 0x5e51c000 + offset) // SAM x Monorail

	//DxO Intersections		
		add(sam~SE & Road~NS, 0x5e582000 + offset) // SAM x Road
		add(sam~SE & Onewayroad~NS, 0x5e58b000 + offset) // SAM x Onewayroad	
		add(sam~NW & Rail~NS, 0x5e514000 + offset) // SAM-Rail
		add(sam~SE & Lightrail~NS, 0x5e517000 + offset) // SAM x Lightrail
		add(sam~SE & Monorail~NS, 0x5e51b000 + offset) // SAM x Monorail

	//DxD Intersections		
		add(sam~EN & Road~SE, 0x5e584000 + offset) // SAM x Road
		add(sam~EN & Onewayroad~SE, 0x5e58e000 + offset) // SAM x Onewayroad
		add(sam~NW & Rail~EN, 0x5e515000 + offset) // SAM-Rail
		add(sam~SW & Lightrail~ES, 0x5e519000 + offset) // SAM x Lightrail
		add(sam~SW & Monorail~ES, 0x5e51d000 + offset) // SAM x Monorail

		}
    map
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)

}