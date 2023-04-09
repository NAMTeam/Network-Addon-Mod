package metarules.module

import metarules.meta._, Network._, RotFlip._, Flags._
import Implicits.segmentToTile
import NetworkProperties._

class MiscResolver extends IdResolver {

  val tileMap: scala.collection.Map[Tile, IdTile] = {
    val map = scala.collection.mutable.Map.empty[Tile, IdTile]
    def add(tile: Tile, id: Int, mappedRepr: Group.QuotientGroup => Set[RotFlip] = null): Unit = {
      assert(!map.contains(tile))
      for (rf <- RotFlip.values) {
        val idTile = if (mappedRepr == null) IdTile(id, rf) else IdTile(id, rf, mappedRepr)
        map.getOrElseUpdate(tile * rf, idTile)
      }
    }
    add(Road~NS, 0x00004b00); add(Road~ES, 0x00000a00)
    add(Rail~NS, 0x03031500); add(Rail~ES, 0x03001a00)
    add(Street~NS, 0x05004b00); add(Street~ES, 0x5f500200)
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
    add((Tla3~NS).projectLeft, 0x51000000); add((Tla3~ES).projectLeft, 0x51000200)
    add((Tla3~NS).projectRight, 0x51000000); add((Tla3~ES).projectRight, 0x51000200) // TODO should right-headed TLAs even be resolved?
    add(Ave2 ~NS, 0x51010000); add(Ave2 ~ES, 0x51010200)
    add(Ard3 ~SN, 0x51020000); add(Ard3 ~SE, 0x51020200); add(Ard3 ~ES, 0x51020900)
    add(Owr1 ~NS, 0x51030000); add(Owr1 ~ES, 0x51030200)
    add(Owr3 ~NS, 0x51040000); add(Owr3 ~ES, 0x51040200)
    add(Nrd4 ~NS, 0x51050000); add(Nrd4 ~ES, 0x51050200)
    //add(Tla5 ~NS, 0x51100000); add(Tla5 ~ES, 0x51100200); add(Tla5 ~NW, 0x51100300)
    add((Tla5~NS).projectLeft, 0x51100000); add((Tla5~ES).projectLeft, 0x51100200); add((Tla5~NW).projectLeft, 0x51100300)
    add((Tla5~NS).projectRight, 0x51100000); add((Tla5~ES).projectRight, 0x51100200); add((Tla5~NW).projectRight, 0x51100300) // TODO should right-headed TLAs even be resolved?
    add((Tla5~CS).projectLeft, 0x51100100); add((Tla5~CS).projectRight, 0x51100100)  // overwritten explicitly here to avoid 0x71.. ID for stubs
    add(Owr4 ~NS, 0x51110000); add(Owr4 ~ES, 0x51110200); add(Owr4~SharedDiagRight, 0x51110300)
    add(Owr5 ~NS, 0x51120000); add(Owr5 ~ES, 0x51120200); add(Owr5 ~NW, 0x51120300)
    add(Rd4  ~NS, 0x51130000); add(Rd4  ~ES, 0x51130200); add(Rd4~SharedDiagRight, 0x51130300)
    add(Rd6  ~NS, 0x51140000); add(Rd6  ~ES, 0x51140200); add(Rd6  ~NW, 0x51140300)
    add(Ave6 ~NS, 0x51200000); add(Ave6 ~NW, 0x51200200); add(Ave6 ~ES, 0x51200300)
    add(Tla7m~NS, 0x51200080); add(Tla7m~ES, 0x51200280)
    add(Ave8 ~NS, 0x51210000); add(Ave8 ~NW, 0x51210200); add(Ave8 ~ES, 0x51210300)
    add(Ave6m~NS, 0x51210080); add(Ave6m~ES, 0x51210280)

    add(L1Road      ~NS, 0x5c000000); add(L1Road      ~ES, 0x5c000200)
    add(L1Onewayroad~NS, 0x5c010000); add(L1Onewayroad~ES, 0x5c010200)
    add(L1Avenue    ~NS, 0x5c020000); add(L1Avenue    ~NE, 0x5c020200); add(L1Avenue~SharedDiagLeft, 0x5c020300)
    add(L2Road      ~NS, 0x5c030000); add(L2Road      ~ES, 0x5c030200)
    add(L2Onewayroad~NS, 0x5c040000); add(L2Onewayroad~ES, 0x5c040200)
    add(L2Avenue    ~NS, 0x5c050000); add(L2Avenue    ~NE, 0x5c050200); add(L2Avenue~SharedDiagLeft, 0x5c050300)

    add(Road~CS, 0x00000300) // orth stub
    add(Road~(0,0,0,1), 0x00000200) // diag stub
    add(Road~NS & Road~WE, 0x00020700) // OxO
    add(Road~NS & Road~NE, 0x00003900) // OxD
    //add(Road~NS & Road~SharedDiagRight, 0x5f040500) // OxD shared-diag TODO create this
    //add(Road~SW & Road~SharedDiagRight, 0x5f040600) // DxD shared-diag TODO create this
    //add(Road~SharedDiagRight & Road~SharedDiagLeft, 0x5f040700) // DxD shared-diag TODO create this
    add(Road~SW & Road~ES, 0x00000700) // DxD
    add(Road~(0,0,2,2), 0x00000F00) // 90 curve
    add(Road~(0,0,1,13), 0x00000C00) // curve
    add(Road~(0,2,0,11), 0x00004D00) // curve
    //add(Road~SharedDiagRight, 0x00014E00) // shared diag TODO check IID
    add(Road~(1,3,11,3), 0x00015600) // add(Road~(1,3,1,13), 0x5f040400) // shared-diag curve
	
    add(Street~CS, 0x05000300) // orth stub
    add(Street~(0,0,0,1), 0x5F500300) // diag stub
    add(Street~NS & Street~WE, 0x05020700) // OxO
    add(Street~NS & Street~NE, 0x5F500700) // OxD
    add(Street~SW & Street~ES, 0x5F500600) // DxD
    add(Street~(0,0,2,2), 0x05000F00) // 90 curve
    add(Street~(0,0,1,13), 0x5F500400) // curve
    add(Street~(0,2,0,11), 0x5F500500) // curve
	add(Street~(0,2,2,11), 0x5F500800)
	add(Street~(0,2,2,13), 0x5F500900)
	add(Street~(0,2,11,2), 0x5F500A00)
	add(Street~(0,11,2,13), 0x5F500C00)
	add(Street~(0,0,2,11), 0x5F500D00)
	add(Street~(0,2,11,11), 0x5F500E00)
	add(Street~(0,2,13,13), 0x5F500F00)
	add(Street~(0,11,0,13), 0x5F501100)
	add(Street~(0,11,0,11), 0x5F501000)

    add(Onewayroad~CS, 0x09000300) // orth stub
    add(Onewayroad~(0,0,0,1), 0x09000200) // diag stub
    add(Onewayroad~NS & Onewayroad~WE, 0x09020700) // OxO
    add(Onewayroad~NS & Onewayroad~NE, 0x09003900) // OxD
    add(Onewayroad~NS & Onewayroad~SharedDiagRight, 0x5f940500) // OxD shared-diag TODO create this
    add(Onewayroad~SW & Onewayroad~SharedDiagRight, 0x5f940600) // DxD shared-diag TODO create this
    add(Onewayroad~SharedDiagRight & Onewayroad~SharedDiagLeft, 0x5f940700) // DxD shared-diag TODO create this
    add(Onewayroad~SW & Onewayroad~ES, 0x09000700) // DxD
    add(Onewayroad~(0,0,2,2), 0x09000F00) // 90 curve
    add(Onewayroad~(0,0,1,13), 0x09000C00) // curve
    add(Onewayroad~(0,2,0,11), 0x09004D00) // curve
    add(Onewayroad~SharedDiagRight, 0x09014E00) // shared diag
    add(Onewayroad~(1,3,11,3), 0x09015600) // add(Onewayroad~(1,3,1,13), 0x5f940400) // shared-diag curve

    // shared diag OWR intersections TODO create these
    add(Onewayroad~SharedDiagRight & Road~NS, 0x5f94a800)
    add(Onewayroad~SharedDiagRight & Road~SW, 0x5f94a900)
    //add(Onewayroad~SharedDiagRight & Road~SharedDiagLeft, 0x5f94aa00)
    add(Onewayroad~SharedDiagRight & Street~NS, 0x5f94ab00)
    add(Onewayroad~SharedDiagRight & Street~SW, 0x5f94ac00)
    //add(Onewayroad~SharedDiagRight & Street~SharedDiagLeft, 0x5f94ad00)
    add(Onewayroad~SharedDiagRight & Avenue~NS, 0x5f94ae00)
    add(Onewayroad~SharedDiagRight & Avenue~SW, 0x5f94af00)
    add(Onewayroad~SharedDiagRight & Avenue~SharedDiagLeft, 0x5f94b000)
    add(Onewayroad~SharedDiagRight & Highway~SN, 0x5f232b00) // 0x5f94b100
    add(Onewayroad~SharedDiagLeft & Highway~ES, 0x5f232900) // 0x5f94b200
    add(Onewayroad~SharedDiagLeft & Highway~SharedDiagRight, 0x5f232a00) // 0x5f94b300
    //add(Onewayroad~SharedDiagRight & Groundhighway~NS, 0x5f94b400)
    //add(Onewayroad~SharedDiagRight & Groundhighway~SW, 0x5f94b500)
    //add(Onewayroad~SharedDiagRight & Groundhighway~SharedDiagLeft, 0x5f94b600)
    add(Onewayroad~SharedDiagRight & Rail~NS, 0x5f372300) // 0x5f94b700
    add(Onewayroad~SharedDiagRight & Rail~SW, 0x5f371e00) // 0x5f94b800
    //add(Onewayroad~SharedDiagRight & Rail~SharedDiagLeft, 0x5f3572200) // 0x5f94b900
    add(Onewayroad~SharedDiagLeft & Lightrail~NS, 0x5f832900) // 0x5f94ba00
    add(Onewayroad~SharedDiagLeft & Lightrail~ES, 0x5f832a00) // 0x5f94bb00 (<-- potentially occupied by a legacy piece)
    //add(Onewayroad~SharedDiagRight & Lightrail~SharedDiagLeft, 0x5f94bc00)
    add(Onewayroad~SharedDiagLeft & Monorail~NS, 0x5fd32900) // 0x5f94bd00
    add(Onewayroad~SharedDiagLeft & Monorail~ES, 0x5fd32a00) // 0x5f94be00
    //add(Onewayroad~SharedDiagRight & Monorail~SharedDiagLeft, 0x5f94bf00)

    add(Avenue~CS, 0x04007300) // orth stub
    add(Avenue~(-2,0,0,+2), 0x04006400) // 90 curve inside
    add(Avenue~(+2,0,0,-2), 0x04006500) // 90 curve outside
    add(Avenue~(+2,0,-113,0), 0x04006300) // 90 curve extended
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
    add((Road~NS).projectLeft & Rail~NE, 0x03010200, nonMirroredOnly)
    add((Road~NS).projectRight & Rail~NE, 0x03020500, mirroredOnly)
    add((Road~WN).projectLeft & Rail~NS, 0x03020100, nonMirroredOnly)
    add((Road~WN).projectRight & Rail~NS, 0x03020400, mirroredOnly)
    add((Road~ES).projectLeft & Rail~NE, 0x03020200, nonMirroredOnly)
    add((Road~NW).projectRight & Rail~NE, 0x03020300, nonMirroredOnly)
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

    // Street intersections
    add(Street~NS & Rail~WE, 0x05010100)
    add(Street~NS & Rail~NE, 0x05010200)
    add((Street~WN).projectLeft & Rail~NS, 0x5f502600, nonMirroredOnly)
    add((Street~WN).projectRight & Rail~NS, 0x5f502900, mirroredOnly)
    add((Street~ES).projectLeft & Rail~NE, 0x5f502700, nonMirroredOnly)
    add((Street~NW).projectRight & Rail~NE, 0x5f502800, nonMirroredOnly)
	add(Street~WE & Highway~NS, 0x02015000)
    add(Street~ES & Highway~NS, 0x5F514100)
    add(Street~WN & Highway~NS, 0x5F514200)
    add(Street~WE & Highway~ES, 0x02015300)
    add(Street~WE & Highway~SharedDiagRight, 0x02015400)
    add(Street~NE & Highway~ES, 0x5F514500)
    add(Street~SW & Highway~SharedDiagRight, 0x5F514600)
	add(Street~ES & Lightrail~NS, 0x5F502B00)
    add(Street~NS & Lightrail~ES, 0x08dd1600)
    add(Street~SW & Lightrail~ES, 0x5F502C00)
    add(Street~EW & Lightrail~NS, 0x08dd1600)
    add(Street~ES & Monorail~NS, 0x5F502D00)
    add(Street~NS & Monorail~ES, 0x0ddd1600)
    add(Street~SW & Monorail~ES, 0x5F502E00)
    add(Street~EW & Monorail~NS, 0x0ddd0200)


    // OWR + intersections
    add(Onewayroad~NS & Rail~WE, 0x09310100)
    add(Onewayroad~NS & Rail~NE, 0x09310200)
    add(Onewayroad~WN & Rail~NS, 0x09320100)
    add((Onewayroad~ES).projectLeft & Rail~NE, 0x09320200, nonMirroredOnly)
    add((Onewayroad~NW).projectRight & Rail~NE, 0x09320300, nonMirroredOnly)
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
    add((Avenue~SN).projectLeft & Rail~NE, 0x04001600, nonMirroredOnly)
    add((Avenue~SN).projectRight & Rail~NE, 0x5d571600, mirroredOnly)
    add((Avenue~NS).projectLeft & Rail~NE, 0x04001700, nonMirroredOnly)
    add((Avenue~NS).projectRight & Rail~NE, 0x5d571700, mirroredOnly)
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

    add(Dirtroad~(0,0,2,2), 0x57000800)
    for (n <- RhwNetworks from Mis to L4Mis) {
      val offset = 0x100000 * n.height
      add(n~(0,0,-2,+2),   0x57020800 + offset)  // Mis 90 curve
      add(n~(0,0,+2,-2),   0x57020e00 + offset)  //
      add(n~(+111,0,-2,0), 0x57020880 + offset)  // Mis 90 curve approach
      add(n~(-111,0,+2,0), 0x57020e80 + offset)  //
    }
    for (n <- RhwNetworks from Dirtroad to L4Rhw6s) {  // single-tile RHW networks
      val rangeId = n.rhwRangeId.get & 0x000F0000
      val offset = rangeId + n.height * 0x10 + (if (n.height == 0) 0 else 5)
      add(n~(+2,0,-123,0),   0x57905000 + offset)  // R1 curve
      add(n~(+123,0,0,-111), 0x57905100 + offset)  // R1 curve
      add(n~(0,+111,-3,0),   0x57905200 + offset)  // R1 curve
      add(n~(0,+111,-113,0), 0x57905F00 + offset)  // R1 curve 90 degree
      if (!n.isSymm) {
        add(n~(-2,0,+123,0),   0x57905080 + offset)  // R1 curve
        add(n~(-123,0,0,+111), 0x57905180 + offset)  // R1 curve
        add(n~(0,-111,+3,0),   0x57905280 + offset)  // R1 curve
        add(n~(0,-111,+113,0), 0x57905F80 + offset)  // R1 curve 90 degree
      }
    }

    // GLR + intersections
    for ((glr, offset) <- Seq(Glr1, Glr2, Glr3, Glr4).zip(Seq(0, 0x4000, 0x8000, 0xb000))) {
      // O×O
      add(glr~NS & Road~WE,       0x5f880300 + offset)
      add(glr~NS & Street~WE,     0x5f880d00 + offset)
      add(glr~NS & Onewayroad~WE, 0x5f880e00 + offset)
      add(glr~NS & Avenue~WE,     0x5f880f00 + offset)
      add(glr~NS & Rail~WE,       0x5f881000 + offset)
      // O×D
      add(glr~NS & Road~WN,       0x5f881800 + offset)
      add(glr~NS & Street~NW,     0x5f882800 + offset)
      add(glr~NS & Onewayroad~WN, 0x5f881900 + offset)
      add(glr~NS & Onewayroad~SharedDiagRight, 0x5f881909 + offset)  // TODO add placeholder texture
      add(glr~NS & Rail~WN,       0x5f881a00 + offset)
      add(glr~NS & Avenue~ES,     0x5f881b00 + offset)
      add(glr~NS & Avenue~SharedDiagRight, 0x5f881c00 + offset)
      // D×O
      add(glr~NE & Road~NS,       0x5f881d00 + offset)
      add(glr~NE & Street~NS,     0x5f882900 + offset)
      add(glr~NE & Onewayroad~NS, 0x5f881e00 + offset)
      add(glr~NE & Rail~NS,       0x5f881f00 + offset)
      add(glr~NE & Avenue~SN,     0x5f882000 + offset)
      add(glr~NE & Avenue~NS,     0x5f882100 + offset)
      // D×D
      add(glr~NE & Road~NW,       0x5f882200 + offset)
      add(glr~NE & Street~NW,     0x5f882a00 + offset)
      add(glr~NE & Onewayroad~NW, 0x5f882300 + offset)
      add(glr~WS & Onewayroad~SharedDiagRight, 0x5f882309 + offset)  // TODO add placeholder texture
      add(glr~NE & Rail~NW,       0x5f882400 + offset)
      add(glr~NE & Avenue~ES,     0x5f882500 + offset)
      add(glr~WS & Avenue~SharedDiagRight, 0x5f882600 + offset)
    }

    map
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)
}
