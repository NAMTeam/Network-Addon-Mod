package metarules.module

import metarules.meta._
import Network._
import RotFlip._
import Flags._
import Group.SymGroup
import NetworkProperties.{isSingleTile, isTripleTile, nonMirroredOnly, mirroredOnly}


class SamResolver extends IdResolver {

	val isSimpleSam = Set(Sam1)
  
	val tileMap: scala.collection.Map[Tile, IdTile] = {
		val map = scala.collection.mutable.Map.empty[Tile, IdTile]
		def add(tile: Tile, id: Int, mappedRepr: Group.QuotientGroup => Set[RotFlip] = null): Unit = {
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
		add(sam~NS & Road~WE, 0x5e520000 + offset) // Road
		}
    map
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)

}