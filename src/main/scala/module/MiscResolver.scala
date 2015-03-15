package metarules.module

import metarules.meta._, Network._, RotFlip._, Flags._
import Implicits.segmentToTile

class MiscResolver extends IdResolver {

  val tileMap: scala.collection.Map[Tile, IdTile] = {
    val map = scala.collection.mutable.Map.empty[Tile, IdTile]
    def add(tile: Tile, id: Int): Unit = {
      assert(!map.contains(tile))
      for (rf <- RotFlip.values) {
        val idTile = IdTile(id, rf)
        map.getOrElseUpdate(tile * rf, idTile)
      }
    }
    add(Road~NS, 0x00004b00); add(Road~ES, 0x00000a00)
    add(Rail~NS, 0x03031500); add(Rail~ES, 0x03001a00)
    add(Street~NS, 0x05004b00); add(Street~ES, 0x05000a00)
    add(Avenue~NS, 0x04006100); add(Avenue~NE, 0x04000200); add(Avenue~SharedDiagLeft, 0x04003800)
    add(Lightrail~NS, 0x08031500); add(Lightrail~ES, 0x08001a00)
    add(Monorail~NS, 0x0d031500); add(Monorail~ES, 0x0d001a00)
    add(Onewayroad~NS, 0x09004b00); add(Onewayroad~ES, 0x09000a00)

    add(Str  ~NS, 0x5d300000); add(Str  ~ES, 0x5d302000)
    add(Glr1 ~NS, 0x5f880000); add(Glr1 ~NW, 0x5f880600)
    add(Glr2 ~NS, 0x5f884000); add(Glr2 ~NW, 0x5f884600)
    add(Glr3 ~NS, 0x5f888000); add(Glr3 ~NW, 0x5f888600)
    add(Glr4 ~NS, 0x5f88c000); add(Glr4 ~NW, 0x5f88c600)
    add(Hsr  ~NS, 0x5dc31500); add(Hsr  ~NW, 0x5dc01a00)
    add(L2Hsr~NS, 0x5dd31500); add(L2Hsr~NW, 0x5dd01a00)
    //add(Tla3 ~NS, 0x51000000); add(Tla3 ~ES, 0x51000200)
    add(Tile.projectLeft(Tla3~NS), 0x51000000); add(Tile.projectLeft(Tla3~ES), 0x51000200)
    add(Tile.projectRight(Tla3~NS), 0x51000000); add(Tile.projectRight(Tla3~ES), 0x51000200) // TODO should right-headed TLAs even be resolved?
    add(Ave2 ~NS, 0x51010000); add(Ave2 ~ES, 0x51010200)
    add(Ard3 ~SN, 0x51020000); add(Ard3 ~SE, 0x51020200); add(Ard3 ~ES, 0x51020900)
    add(Owr1 ~NS, 0x51030000); add(Owr1 ~ES, 0x51030200)
    add(Owr3 ~NS, 0x51040000); add(Owr3 ~ES, 0x51040200)
    add(Nrd4 ~NS, 0x51050000); add(Nrd4 ~ES, 0x51050200)
    //add(Tla5 ~NS, 0x51100000); add(Tla5 ~ES, 0x51100200); add(Tla5 ~NW, 0x51100300)
    add(Tile.projectLeft(Tla5~NS), 0x51100000); add(Tile.projectLeft(Tla5~ES), 0x51100200); add(Tile.projectLeft(Tla5~NW), 0x51100300)
    add(Tile.projectRight(Tla5~NS), 0x51100000); add(Tile.projectRight(Tla5~ES), 0x51100200); add(Tile.projectRight(Tla5~NW), 0x51100300) // TODO should right-headed TLAs even be resolved?
    add(Owr4 ~NS, 0x51110000); add(Owr4 ~ES, 0x51110200); add(Owr4~SharedDiagRight, 0x51110300)
    add(Owr5 ~NS, 0x51120000); add(Owr5 ~ES, 0x51120200); add(Owr5 ~NW, 0x51120300)
    add(Rd4  ~NS, 0x51130000); add(Rd4  ~ES, 0x51130200); add(Rd4~SharedDiagRight, 0x51130300)
    add(Rd6  ~NS, 0x51140000); add(Rd6  ~ES, 0x51140200); add(Rd6  ~NW, 0x51140300)
    add(Ave6 ~NS, 0x51200000); add(Ave6 ~NW, 0x51200200); add(Ave6 ~ES, 0x51200300)
    add(Tla7m~NS, 0x51200080); add(Tla7m~ES, 0x51200280)
    add(Ave8 ~NS, 0x51210000); add(Ave8 ~NW, 0x51210200); add(Ave8 ~ES, 0x51210300)
    add(Ave6m~NS, 0x51210080); add(Ave6m~ES, 0x51210280)

    add(Road~CS, 0x00000300) // orth stub
    add(Road~(0,0,0,1), 0x00000200) // diag stub
    add(Road~NS & Road~WE, 0x00020700) // OxO
    add(Road~NS & Road~NE, 0x00003900) // OxD
    //add(Road~NS & Road~SharedDiagRight, 0x5f040500) // OxD shared-diag TODO create this
    //add(Road~SW & Road~SharedDiagRight, 0x5f040600) // DxD shared-diag TODO create this
    //add(Road~SharedDiagRight & Road~SharedDiagLeft, 0x5f040700) // DxD shared-diag TODO create this
    add(Road~SW & Road~ES, 0x00000700) // DxD
    add(Road~(0,0,1,13), 0x00000C00) // curve
    add(Road~(0,2,0,11), 0x00004D00) // curve
    //add(Road~SharedDiagRight, 0x00014E00) // shared diag TODO check IID
    add(Road~(1,3,1,13), 0x5f040400) // shared-diag curve

    add(Onewayroad~CS, 0x09000300) // orth stub
    add(Onewayroad~(0,0,0,1), 0x09000200) // diag stub
    add(Onewayroad~NS & Onewayroad~WE, 0x09020700) // OxO
    add(Onewayroad~NS & Onewayroad~NE, 0x09003900) // OxD
    add(Onewayroad~NS & Onewayroad~SharedDiagRight, 0x5f940500) // OxD shared-diag TODO create this
    add(Onewayroad~SW & Onewayroad~SharedDiagRight, 0x5f940600) // DxD shared-diag TODO create this
    add(Onewayroad~SharedDiagRight & Onewayroad~SharedDiagLeft, 0x5f940700) // DxD shared-diag TODO create this
    add(Onewayroad~SW & Onewayroad~ES, 0x09000700) // DxD
    add(Onewayroad~(0,0,1,13), 0x09000C00) // curve
    add(Onewayroad~(0,2,0,11), 0x09004D00) // curve
    add(Onewayroad~SharedDiagRight, 0x09014E00) // shared diag
    add(Onewayroad~(1,3,1,13), 0x5f940400) // shared-diag curve

    // shared diag OWR intersections TODO create these
    add(Onewayroad~SharedDiagRight & Road~NS, 0x5f940800)
    add(Onewayroad~SharedDiagRight & Road~SW, 0x5f940900)
    //add(Onewayroad~SharedDiagRight & Road~SharedDiagLeft, 0x5f940a00)
    add(Onewayroad~SharedDiagRight & Street~NS, 0x5f940b00)
    add(Onewayroad~SharedDiagRight & Street~SW, 0x5f940c00)
    //add(Onewayroad~SharedDiagRight & Street~SharedDiagLeft, 0x5f940d00)
    add(Onewayroad~SharedDiagRight & Avenue~NS, 0x5f940e00)
    add(Onewayroad~SharedDiagRight & Avenue~SW, 0x5f940f00)
    add(Onewayroad~SharedDiagRight & Avenue~SharedDiagLeft, 0x5f941000)
    add(Onewayroad~SharedDiagRight & Highway~SN, 0x5f232b00) // 0x5f941100
    add(Onewayroad~SharedDiagLeft & Highway~ES, 0x5f232900) // 0x5f941200
    add(Onewayroad~SharedDiagLeft & Highway~SharedDiagRight, 0x5f232a00) // 0x5f941300
    //add(Onewayroad~SharedDiagRight & Groundhighway~NS, 0x5f941400)
    //add(Onewayroad~SharedDiagRight & Groundhighway~SW, 0x5f941500)
    //add(Onewayroad~SharedDiagRight & Groundhighway~SharedDiagLeft, 0x5f941600)
    add(Onewayroad~SharedDiagRight & Rail~NS, 0x5f372300) // 0x5f941700
    add(Onewayroad~SharedDiagRight & Rail~SW, 0x5f371e00) // 0x5f941800
    //add(Onewayroad~SharedDiagRight & Rail~SharedDiagLeft, 0x5f3572200) // 0x5f941900
    add(Onewayroad~SharedDiagLeft & Lightrail~NS, 0x5f832900) // 0x5f941a00
    add(Onewayroad~SharedDiagLeft & Lightrail~ES, 0x5f832a00) // 0x5f941b00
    //add(Onewayroad~SharedDiagRight & Lightrail~SharedDiagLeft, 0x5f941c00)
    add(Onewayroad~SharedDiagLeft & Monorail~NS, 0x5fd32900) // 0x5f941d00
    add(Onewayroad~SharedDiagLeft & Monorail~ES, 0x5fd32a00) // 0x5f941e00
    //add(Onewayroad~SharedDiagRight & Monorail~SharedDiagLeft, 0x5f941f00)

    add(Avenue~CS, 0x04007300) // orth stub
    add(Avenue~(0,-2,0,+11), 0x04007600) // curve
    add(Avenue~(0,+2,0,-11), 0x04007700) // curve
    add(Avenue~(0,-11,+3,0), 0x04007800) // curve
    add(Avenue~(-3,+11,-3,+1), 0x04007900) // curve
    add(Avenue~WE & Avenue~NS, 0x04009000) // OxO
    add(Avenue~NS & Avenue~NE, 0x04007400) // OxD
    add(Avenue~SN & Avenue~NE, 0x04007500) // OxD
    add(Avenue~SN & Avenue~SharedDiagLeft, 0x04009300) // OxD
    add(Avenue~ES & Avenue~SharedDiagLeft, 0x04006900) // DxD
    add(Avenue~SharedDiagRight & Avenue~SharedDiagLeft, 0x04009400) // DxD
    add(Avenue~ES & Avenue~NE, 0x04003600) // DxD

    // Road + intersections
    add(Road~NS & Rail~WE, 0x03010100)
    add(Road~NS & Rail~NE, 0x03010200)
    add(Road~WN & Rail~NS, 0x03020100)
    add(Road~ES & Rail~NE, 0x03020200)
    add(Road~WE & Highway~NS, 0x02014000)
    add(Road~ES & Highway~NS, 0x02014100)
    add(Road~WN & Highway~NS, 0x02014200)
    add(Road~WE & Highway~ES, 0x02014300)
    add(Road~WE & Highway~SharedDiagRight, 0x02014400)
    add(Road~NE & Highway~ES, 0x02014500)
    add(Road~SW & Highway~SharedDiagRight, 0x02014600)
    add(Road~WE & Street~NS, 0x00aa0a00)
    add(Road~ES & Street~WE, 0x00aa0c00)
    add(Road~NS & Street~ES, 0x5f502200)
    add(Road~ES & Street~NE, 0x5f502400)
    add(Road~WE & Avenue~NS, 0x04008900)
    add(Road~ES & Avenue~SN, 0x04001300)
    add(Road~WN & Avenue~SN, 0x04001900)
    add(Road~NS & Avenue~ES, 0x04005700)
    add(Road~NS & Avenue~SharedDiagRight, 0x04006000)
    add(Road~ES & Avenue~NE, 0x04020200)
    add(Road~ES & Avenue~SharedDiagLeft, 0x04023800)
    add(Road~ES & Lightrail~NS, 0x08dd0100)
    add(Road~NS & Lightrail~ES, 0x08dd1600)
    add(Road~SW & Lightrail~ES, 0x08dd1700)
    add(Road~EW & Lightrail~NS, 0x08dd0200)
    add(Road~ES & Monorail~NS, 0x0ddd0100)
    add(Road~NS & Monorail~ES, 0x0ddd1600)
    add(Road~SW & Monorail~ES, 0x0ddd1700)
    add(Road~EW & Monorail~NS, 0x0ddd0200)
    add(Road~ES & Onewayroad~SW, 0x09700700)
    add(Road~NE & Onewayroad~NS, 0x09703900)
    add(Road~NS & Onewayroad~EW, 0x09720700)
    add(Road~NS & Onewayroad~NE, 0x09803900)

    // OWR + intersections
    add(Onewayroad~NS & Rail~WE, 0x09310100)
    add(Onewayroad~NS & Rail~NE, 0x09310200)
    add(Onewayroad~WN & Rail~NS, 0x09320100)
    add(Onewayroad~ES & Rail~NE, 0x09320200) // TODO handle flipped version 0x09320300 – possibly analogous to TLA tiles
    add(Onewayroad~WE & Highway~NS, 0x09b14000)
    add(Onewayroad~ES & Highway~NS, 0x09b14100)
    add(Onewayroad~WN & Highway~NS, 0x09b14200)
    add(Onewayroad~WE & Highway~ES, 0x09b14300)
    add(Onewayroad~WE & Highway~SharedDiagRight, 0x09b14400)
    add(Onewayroad~NE & Highway~ES, 0x09b14500)
    add(Onewayroad~SW & Highway~SharedDiagRight, 0x09b14600)
    add(Onewayroad~WE & Street~NS, 0x09aa0a00)
    add(Onewayroad~ES & Street~WE, 0x09aa0c00)
    add(Onewayroad~NS & Street~ES, 0x5f503400)
    add(Onewayroad~ES & Street~NE, 0x5f503600)
    add(Onewayroad~WE & Avenue~NS, 0x091a8900)
    add(Onewayroad~ES & Avenue~SN, 0x091a1300)
    add(Onewayroad~WN & Avenue~SN, 0x091a1900)
    add(Onewayroad~NS & Avenue~ES, 0x091a5700)
    add(Onewayroad~NS & Avenue~SharedDiagRight, 0x091a6000)
    add(Onewayroad~ES & Avenue~NE, 0x091a0200)
    add(Onewayroad~ES & Avenue~SharedDiagLeft, 0x091a3800)
    add(Onewayroad~ES & Lightrail~NS, 0x091d0100)
    add(Onewayroad~NS & Lightrail~ES, 0x091d1600)
    add(Onewayroad~SW & Lightrail~ES, 0x091d1700)
    add(Onewayroad~EW & Lightrail~NS, 0x091d0200)
    add(Onewayroad~ES & Monorail~NS, 0x092d0100)
    add(Onewayroad~NS & Monorail~ES, 0x092d1600)
    add(Onewayroad~SW & Monorail~ES, 0x092d1700)
    add(Onewayroad~EW & Monorail~NS, 0x092d0200)

    // Avenue + intersections
    add(Avenue~ES & Rail~NE, 0x04002100)
    add(Avenue~SharedDiagRight & Rail~SW, 0x04004300)
    add(Avenue~SN & Rail~NE, 0x04001600)
    add(Avenue~NS & Rail~NE, 0x04001700)
    add(Avenue~SN & Rail~WE, 0x04001500)
    add(Avenue~ES & Rail~NS, 0x04004700)
    add(Avenue~SharedDiagRight & Rail~NS, 0x04004600)
    add(Avenue~NS & Highway~EW, 0x04010000)
    add(Avenue~SN & Highway~SW, 0x04010700)
    add(Avenue~NS & Highway~SW, 0x04010600)
    add(Avenue~NS & Highway~SharedDiagRight, 0x04010500)
    add(Avenue~NE & Highway~SharedDiagRight, 0x04010300)
    add(Avenue~SharedDiagLeft & Highway~ES, 0x04010400)
    add(Avenue~NE & Highway~ES, 0x04010100)
    add(Avenue~SharedDiagLeft & Highway~SharedDiagRight, 0x04010200)
    add(Avenue~SharedDiagRight & Highway~SN, 0x04010800)
    add(Avenue~ES & Highway~NS, 0x04010900)
    add(Avenue~ES & Highway~SN, 0x04011000)
    add(Avenue~SN & Street~WE, 0x04008300)
    add(Avenue~SharedDiagRight & Street~NS, 0x5f577800)
    add(Avenue~ES & Street~WE, 0x5f577900)
    add(Avenue~SN & Street~ES, 0x5f503000)
    add(Avenue~SN & Street~WN, 0x5f503100)
    add(Avenue~NE & Street~ES, 0x5f503200)
    add(Avenue~SharedDiagLeft & Street~ES, 0x5f503300)
    add(Avenue~NE & Lightrail~NS, 0x08dd0500)
    add(Avenue~SharedDiagLeft & Lightrail~NS, 0x08dd0600)
    add(Avenue~EW & Lightrail~NS, 0x08dd0700)
    add(Avenue~NS & Lightrail~ES, 0x08dd1100)
    add(Avenue~SN & Lightrail~ES, 0x08dd0900)
    add(Avenue~SharedDiagLeft & Lightrail~ES, 0x08dd1200)
    add(Avenue~NE & Lightrail~ES, 0x08dd1300)
    add(Avenue~NE & Monorail~NS, 0x0ddd0500)
    add(Avenue~SharedDiagLeft & Monorail~NS, 0x0ddd0600)
    add(Avenue~EW & Monorail~NS, 0x0ddd0700)
    add(Avenue~NS & Monorail~ES, 0x0ddd1100)
    add(Avenue~SN & Monorail~ES, 0x0ddd0900)
    add(Avenue~SharedDiagLeft & Monorail~ES, 0x0ddd1200)
    add(Avenue~NE & Monorail~ES, 0x0ddd1300)

    map
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)
}
