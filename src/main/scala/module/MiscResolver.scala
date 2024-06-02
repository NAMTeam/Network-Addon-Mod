package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, RotFlip._, Flags._
import Implicits.segmentToTile
import NetworkProperties._

class MiscResolver extends IdResolver {

  val tileMap: scala.collection.Map[Tile, IdTile] = {
    val map = scala.collection.mutable.Map.empty[Tile, IdTile]
    def add(tile: Tile, id: Int, mappedRepr: group.Quotient => Set[RotFlip] = null): Unit = {
      assert(!map.contains(tile))
      for (rf <- RotFlip.values) {
        val idTile = if (mappedRepr == null) IdTile(id, rf) else IdTile(id, rf, mappedRepr)
        map.getOrElseUpdate(tile * rf, idTile)
      }
    }
    add(Road~NS, 0x00004b00); add(Road~ES, 0x00000a00)
    add(Rail~NS, 0x03031500); add(Rail~ES, 0x03001a00) // maxis IIDs, RRW IIDs in RealRailwayResolver
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
	
	add(Sam1~NS, 0x5e54b100)
	add(Sam2~NS, 0x5e54b200)
	add(Sam3~NS, 0x5e54b300)
	add(Sam4~NS, 0x5e54b400)
	add(Sam5~NS, 0x5e54b500)
	add(Sam6~NS, 0x5e54b600)
	add(Sam7~NS, 0x5e54b700)
	add(Sam8~NS, 0x5e54b800)
	add(Sam9~NS, 0x5e54b900)
	add(Sam10~NS, 0x5e54ba00)
	add(Sam11~NS, 0x5e54bb00)
	
	add(Sam2~SE, 0x5e572200)
	add(Sam3~SE, 0x5e572300)
	add(Sam4~SE, 0x5e572400)
	add(Sam5~SE, 0x5e572500)
	add(Sam6~SE, 0x5e572600)
	add(Sam7~SE, 0x5e572700)
	add(Sam8~SE, 0x5e572800)
	add(Sam9~SE, 0x5e572900)
	add(Sam10~SE, 0x5e572a00)
	add(Sam11~SE, 0x5e572b00)

    add(Road~CS, 0x00000300) // orth stub
    add(Road~(0,0,0,1), 0x00000200) // diag stub
    add(Road~NS & Road~WE, 0x00020700) // OxO
    add(Road~NS & Road~NE, 0x00003900) // OxD
    //add(Road~NS & Road~SharedDiagRight, 0x5f040500) // OxD shared-diag TODO create this
    //add(Road~SW & Road~SharedDiagRight, 0x5f040600) // DxD shared-diag TODO create this
    //add(Road~SharedDiagRight & Road~SharedDiagLeft, 0x5f040700) // DxD shared-diag TODO create this
    add(Road~SW & Road~ES, 0x00000700) // DxD
	add(Road~NS & Road~CE, 0x00005700) // OxO T
	add(Road~NS & Road~CSE, 0x00006300) // OxD T
    add(Road~(0,0,2,2), 0x00000F00) // 90 curve
    add(Road~(0,0,1,13), 0x00000C00) // curve
    add(Road~(0,2,0,11), 0x00004D00) // curve
    //add(Road~SharedDiagRight, 0x00014E00) // shared diag TODO check IID
    add(Road~(1,3,11,3), 0x00015600) // add(Road~(1,3,1,13), 0x5f040400) // shared-diag curve
	
	add(Street~(0,0,0,0), 0x00000100)
    add(Street~CS, 0x05000300) // orth stub
    add(Street~(0,0,0,1), 0x5F500300) // diag stub
	add(Street~(2,2,2,2), 0x05020700) // OxO alt
    add(Street~NS & Street~WE, 0x05020700) // OxO
    add(Street~NS & Street~NE, 0x5F500700) // OxD
    add(Street~SE & Street~EN, 0x5F500600) // DxD
	add(Street~NS & Street~CE, 0x05005700) // OxO T
	add(Street~NS & Street~CSE, 0x5F500A00) // OxD T (also has (0,2,11,2) alt implementation)
    add(Street~CS & Street~NE, 0x5F504000) // DxO T1
    add(Street~CS & Street~WS, 0x5F504100) // DxO T2
    add(Street~SE & Street~CEN, 0x5F504200) // DxD T1
    add(Street~WN & Street~CSW, 0x5F504300) // DxD T2
    add(Street~(0,0,2,2), 0x05000F00) // 90 curve
    add(Street~(0,0,1,13), 0x5F500400) // curve
    add(Street~(0,2,0,11), 0x5F500500) // curve
	add(Street~(0,2,2,11), 0x5F500800)
	add(Street~(0,2,2,13), 0x5F500900)
	add(Street~(0,2,11,2), 0x5F500A00) // alt OxD T
	add(Street~(2,2,2,11), 0x5F500B00)
	add(Street~(0,11,2,13), 0x5F500C00)
	add(Street~(0,0,2,11), 0x5F500D00)
	add(Street~(0,2,11,11), 0x5F500E00)
	add(Street~(0,2,13,13), 0x5F500F00)
	add(Street~(0,11,0,13), 0x5F501100)
	add(Street~(0,11,0,11), 0x5F501000)
	add(Street~(0,0,2,13), 0x5F501D00)
	add(Street~(0,0,11,13), 0x5F501400)
	add(Street~(1,3,2,2), 0x5F501500)
	add(Street~(1,3,2,11), 0x5F501600)
	add(Street~(2,2,11,11), 0x5F501700)
	add(Street~(2,11,2,11), 0x5F501800)
	add(Street~(2,11,2,13), 0x5F501900)
	add(Street~(2,11,11,11), 0x5F501A00)
	add(Street~(11,0,13,2), 0x5F501B00)
	add(Street~(0,1,0,3), 0x5F501100)
	add(Street~(1,1,1,1), 0x5F501E00)
	add(Street~(3,0,3,3), 0x5F501300)
	add(Street~(11,0,11,2), 0x5F501200)
	//smaller wide-radius curves
	//2x2 90
	add(Street~(0,2,0,131), 0x5F593800)
	add(Street~(143,0,0,141), 0x5F593900)
	add(Street~(0,131,133,0), 0x5F593A00)
	add(Street~(2,2,0,131), 0x5F593F00) // T-Intersection off outer tile
	add(Street~(133,131,133,131), 0x5F593D00) // Diverter
	add(Street~(133,0,0,131) & Street~(0,131,133,0), 0x5F593D00) // Diverter (alt)
	add(Road~(133,0,0,131) & Street~(0,131,133,0), 0x5F093E00)   // Diverter street & road
	//3x2 S
	add(Street~(2,0,153,0), 0x5F594000)
	add(Street~(153,0,0,161), 0x5F594100)
	add(Street~(173,0,0,181), 0x5F594200)
	add(Street~(2,2,153,0), 0x5F594300) //T-Intersection off outer tile
    // diagonal s-curve
    add(Street~(3,0,0,152), 0x5F595C00 )
    add(Street~(0,152,154,1), 0x5F595D00)
    add(Street~(154,0,0,1), 0x5F595E00)
	//Street Roundabout
	add(Street~(0,0,102,102), 0x5F56BB00) //Base
	add(Street~(0,2,102,102), 0x5F56BA00) //Base with Orth Street connection
	add(Street~(0,13,102,102), 0x5F56B900) //Base with Diag Street connection
	//Larger (R2-esque) 45 (4x3)
	//Tile 1F is override of Diagonal
	//Tile 22 is override of 1x1 Stub
	add(Street~(0,2,0,111), 0x5F592400)
	add(Street~(0,111,0,11), 0x5F592300)
	add(Street~(15,0,0,14), 0x5F592200)
	add(Street~(0,11,113,0), 0x5F592100)
	add(Street~(113,0,0,1), 0x5F592000)
	//Larger 90 (4x4)
	add(Street~(0,2,0,181), 0x5F592500)
	add(Street~(0,181,11,191), 0x5F592600)
	add(Street~(11,0,0,82), 0x5F592700)
	add(Street~(0,191,194,0), 0x5F592800)
	add(Street~(0,82,82,0), 0x5F592900)
	//T-ints off
	add(Street~(2,2,0,181), 0x5F592F00)
	add(Street~(2,181,11,191), 0x5F592E00)
	
	
    add(Onewayroad~CS, 0x09000300) // orth stub
    add(Onewayroad~(0,0,0,1), 0x09000200) // diag stub
    add(Onewayroad~NS & Onewayroad~WE, 0x09020700) // OxO
    add(Onewayroad~NS & Onewayroad~NE, 0x09003900) // OxD
    add(Onewayroad~NS & Onewayroad~SharedDiagRight, 0x5f940500) // OxD shared-diag TODO create this
    add(Onewayroad~SW & Onewayroad~SharedDiagRight, 0x5f940600) // DxD shared-diag TODO create this
    add(Onewayroad~SharedDiagRight & Onewayroad~SharedDiagLeft, 0x5f940700) // DxD shared-diag TODO create this
    add(Onewayroad~SW & Onewayroad~ES, 0x09000700) // DxD
	add(Onewayroad~NS & Onewayroad~CE, 0x90005700) // OxO T
	add(Onewayroad~NS & Onewayroad~CSE, 0x09006300) // OxD T
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

    // Road intersections
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
    add(Road~NS & Street~CSE, 0x5f502100)
    add(Road~NS & Street~ES, 0x5f502200)
	add(Road~CS & Street~NE, 0x5f502300)
    add(Road~ES & Street~NE, 0x5f502400)
    add(Road~ES & Street~CEN, 0x5f504400)
	add(Road~WE & Street~CS, 0x00aa0800)
	add(Road~CE & Street~NS, 0x00aa0400)
	add(Road~NC & Street~CS, 0x00aa0100)
	add(Road~CE & Street~NC, 0x00aa0200)
	add(Road~CWN & Street~WE, 0x00aa0d00)
	add(Road~(0,0,2,0) & Street~(2,2,0,0), 0x00aa0300)
	add(Road~(0,0,2,0) & Street~(2,2,0,2), 0x00aa0500)
	add(Road~(2,2,0,0) & Street~(0,0,0,2), 0x00aa0700)
	add(Road~(2,2,0,0) & Street~(0,0,2,2), 0x00aa0900)
	add(Road~(2,2,2,0) & Street~(0,0,0,2), 0x00aa0b00)
	// add(Road~(0,1,0,0) & Street~WE, 0x00aa0d00) //alt implementation of 0x00aa0d00
	// add(Road~(0,11,0,0) & Street~WE, 0x00aa0d00) //alt implementation of 0x00aa0d00
	add(Road~CWN & Street~CE, 0x00aa0e00)
	add(Road~CWN & Street~WC, 0x00aa1100)
	add(Road~CES & Street~CSE, 0x5F502000)
	add(Road~CES & Street~NE, 0x5F502500)
	add(Road~(0,0,1,3) & Street~(2,2,0,0), 0x5F072300)
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
    add(Street~NS & Lightrail~ES, 0x08dd1000)
    add(Street~SW & Lightrail~ES, 0x5F502C00)
    add(Street~EW & Lightrail~NS, 0x08dd0800)
    add(Street~ES & Monorail~NS, 0x5F502D00)
    add(Street~NS & Monorail~ES, 0x0ddd1000)
    add(Street~SW & Monorail~ES, 0x5F502E00)
    add(Street~EW & Monorail~NS, 0x0ddd0800)
	//Street Roundabout Connections
	add(Road~(0,2,0,0) & Street~(0,0,102,102), 0x5F084800) //Base with Orth Road connection
	add(Road~(0,13,0,0) & Street~(0,0,102,102), 0x5F084900) //Base with Diag Road connection
	add(Onewayroad~(0,2,0,0) & Street~(0,0,102,102), 0x5F984800) //Base with Orth Onewayroad connection
	add(Onewayroad~(0,13,0,0) & Street~(0,0,102,102), 0x5F984900) //Base with Diag Onewayroad connection


    // OWR intersections
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
    add(Onewayroad~NS & Street~CSE, 0x5f503500)
    add(Onewayroad~ES & Street~NE, 0x5f503600)
	add(Onewayroad~CS & Street~NE, 0x5f503700)
    add(Onewayroad~ES & Street~CEN, 0x5f504600)
	add(Onewayroad~WE & Street~CS, 0x09aa0800)
	add(Onewayroad~CE & Street~NS, 0x09aa0400)
	add(Onewayroad~NC & Street~CS, 0x09aa0100)
	add(Onewayroad~CE & Street~NC, 0x09aa0200)
	add(Onewayroad~CWN & Street~WE, 0x09aa0d00)
	add(Onewayroad~(0,0,2,0) & Street~(2,2,0,0), 0x09aa0300)
	add(Onewayroad~(0,0,2,0) & Street~(2,2,0,2), 0x09aa0500)
	add(Onewayroad~(2,2,0,0) & Street~(0,0,0,2), 0x09aa0700)
	add(Onewayroad~(2,2,0,0) & Street~(0,0,2,2), 0x09aa0900)
	add(Onewayroad~(2,2,2,0) & Street~(0,0,0,2), 0x09aa0b00)
	add(Onewayroad~CWN & Street~CE, 0x09aa0d00)
	add(Onewayroad~CWN & Street~WC, 0x09aa1100)
	add(Onewayroad~CES & Street~NE, 0x5F503800)
	add(Onewayroad~(0,0,1,3) & Street~(2,2,0,0), 0x5F974300)
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
	add(Avenue~ES & Street~WC, 0x04004500)
    add(Avenue~ES & Street~WE, 0x5f577900)
	add(Avenue~(0,0,+1,-3) & Street~WC, 0x04004500)
	add(Avenue~(0,0,+1,-3) & Street~WE, 0x5f577900)
    add(Avenue~SN & Street~ES, 0x5f503000)
    add(Avenue~SN & Street~WN, 0x5f503100)
    add(Avenue~NE & Street~ES, 0x5f503200)
	add(Avenue~(0,+1,-3,0) & Street~ES, 0x5f503200)
    add(Avenue~SharedDiagLeft & Street~ES, 0x5f503300)
	add(Avenue~SN & Street~CE, 0x04004400)
	add(Avenue~SN & Street~WC, 0x04008600)
	add(Avenue~NC & Street~WE, 0x04005600)
	add(Avenue~NC & Street~(2,0,2,2), 0x04009600)
	add(Avenue~(0,2,4,0) & Street~(0,0,0,2), 0x04009700)
	add(Avenue~(4,0,0,2) & Street~(0,0,2,0), 0x04008400)
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
    for (n <- RhwNetworks rangeFrom Mis rangeTo L4Mis) {
      val offset = 0x100000 * n.height
      add(n~(0,0,-2,+2),   0x57020800 + offset)  // Mis 90 curve
      add(n~(0,0,+2,-2),   0x57020e00 + offset)  //
      add(n~(+111,0,-2,0), 0x57020880 + offset)  // Mis 90 curve approach
      add(n~(-111,0,+2,0), 0x57020e80 + offset)  //
    }
    for (n <- RhwNetworks rangeFrom Dirtroad rangeTo L4Rhw6s) {  // single-tile RHW networks
      val rangeId = RhwResolver.rhwRangeId(n) & 0x000F0000
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
	add(Dirtroad~NS & Street~WE, 0x57001000)
	add(Dirtroad~NS & Street~WS, 0x57004000)
	add(Dirtroad~SE & Street~WE, 0x57007000)
	add(Dirtroad~SE & Street~WS, 0x5700a000)

	add(Dirtroad~NS & Street~CE, 0x57600000)
	add(Dirtroad~CE & Street~NS, 0x57601000)
	add(Dirtroad~NS & Road~CE, 0x57600100)
	add(Dirtroad~CE & Road~NS, 0x57601100)
	add(Dirtroad~NS & Onewayroad~CE, 0x57600200)
	add(Dirtroad~CE & Onewayroad~NS, 0x57601200)

    // RHW on-slopes (orthogonal)
    for (rhw <- RhwNetworks if rhw.height == 0) {
      val maxHeight = if ((Mis + Rhw4 + Rhw6s).contains(rhw)) 4 else 2
      val minHeight = 0
      for {
        levelDiff <- Seq(1, 2)  // L1 vs L2 onslopes
        height <- minHeight to (maxHeight-levelDiff)
      } /*do*/ {
        import RhwRuleGenerator.HeightLevel
        val lower: Network = height~rhw
        val upper: Network = (height+levelDiff)~rhw
        add(upper~NC & lower~CS, RhwResolver.rhwHtRangeId(rhw) + 0x100*(levelDiff-1) + 0x10*height)  // direction North (upper) to South (lower)
      }
    }

    // GLR + intersections
    for ((glr, offset) <- Seq(Glr1, Glr2, Glr3, Glr4).zip(Seq(0, 0x4000, 0x8000, 0xc000))) {
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

    // NWM x Street T-intersections
      // Street thru, NWM ends
        // OxO
        add(Street~NS & (Tla3~CE).projectLeft,  0x51004000)  // Tla3 Ends
        add(Street~NS & (Tla3~CE).projectRight, 0x51004000)  // Tla3 Ends
        add(Street~NS & Ave2~CE, 0x51014000)                 // Ave2 Ends
        add(Street~NS & Ard3~CE, 0x51024000)                 // Ard3 Ends
        add(Street~NS & Owr1~CE, 0x51034000)                 // Owr1 Ends
        add(Street~NS & Owr3~CE, 0x51044000)                 // Owr3 Ends
        add(Street~NS & Nrd4~CE, 0x51054000)                 // Nrd4 Ends
        // OxD
        add(Street~NS & (Tla3~CSE).projectLeft,  0x5100B500)  // Tla3 Ends
        add(Street~NS & (Tla3~CSE).projectRight, 0x5100B500)  // Tla3 Ends
        add(Street~NS & Ave2~CSE, 0x5101B500)                 // Ave2 Ends
        add(Street~NS & Ard3~CSE, 0x5102B500)                 // Ard3 Ends
        add(Street~NS & Owr1~CSE, 0x5103B500)                 // Owr1 Ends
        add(Street~NS & Owr3~CSE, 0x5104B500)                 // Owr1 Ends
        add(Street~NS & Nrd4~CSE, 0x5105B500)                 // Nrd4 Ends
      // NWM thru, Street ends
        // OxO short T
        add(Street~CS & (Tla3~WE).projectLeft,  0x51003000)
        add(Street~CS & (Tla3~WE).projectRight, 0x51003000)
        add(Street~CS & Ave2~WE,                0x51013000)
        add(Street~CS & Ard3~WE,                0x51023000)
        add(Street~CS & Ard3~EW,                0x51023080)
        add(Street~CS & Owr1~WE,                0x51033000)
        add(Street~CS & Owr3~WE,                0x51043000)
        add(Street~CS & Nrd4~WE,                0x51053000)
        add(Street~CN & (Tla5~EW).projectLeft,  0x51103000)
        add(Street~CN & (Tla5~EW).projectRight, 0x71103000)
        add(Street~CN & Owr4~EW, 0x51113000)
        add(Street~CN & Owr5~EW, 0x51123000)
        add(Street~CN & Rd4~EW,  0x51133000)
        add(Street~CN & Rd6~EW,  0x51143000)
        add(Street~CN & Ave6~EW, 0x51203000)

    map
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)
}
