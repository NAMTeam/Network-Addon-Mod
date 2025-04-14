package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, RotFlip._, Flags._
import Implicits.segmentToTile
import NetworkProperties._

class MiscResolver extends IdResolver {

  val tileMap: scala.collection.Map[Tile, IdTile] = {
    val builder = new ResolverBuilder
    import builder.add

    add(0x00004b00, Road~NS); add(0x00000a00, Road~ES)
    add(0x03031500, Rail~NS); add(0x03001a00, Rail~ES) // maxis IIDs, RRW IIDs in RealRailwayResolver
    add(0x05004b00, Street~NS); add(0x5f500200, Street~ES)
    add(0x04006100, Avenue~NS); add(0x04000200, Avenue~NE); add(0x04003800, Avenue~SharedDiagLeft)
    add(0x08031500, Lightrail~NS); add(0x08001a00, Lightrail~ES)
    add(0x0d031500, Monorail~NS); add(0x0d001a00, Monorail~ES)
    add(0x09004b00, Onewayroad~NS); add(0x09000a00, Onewayroad~ES)

    add(0x5d300000, Str  ~NS); add(0x5d302000, Str  ~ES)
    add(0x5f880000, Glr1 ~NS); add(0x5f880600, Glr1 ~NW)
    add(0x5f884000, Glr2 ~NS); add(0x5f884600, Glr2 ~NW)
    add(0x5f888000, Glr3 ~NS); add(0x5f888600, Glr3 ~NW)
    add(0x5f88c000, Glr4 ~NS); add(0x5f88c600, Glr4 ~NW)
    add(0x5dc31500, Hsr  ~NS); add(0x5dc01a00, Hsr  ~NW)
    add(0x5dd31500, L2Hsr~NS); add(0x5dd01a00, L2Hsr~NW)
    add(0x51000000, Tla3 ~NS); add(0x51000200, Tla3 ~ES)
    add(0x51010000, Ave2 ~NS); add(0x51010200, Ave2 ~ES)
    add(0x51020000, Ard3 ~SN); add(0x51020200, Ard3 ~SE); add(0x51020900, Ard3 ~ES)
    add(0x51030000, Owr1 ~NS); add(0x51030200, Owr1 ~ES)
    add(0x51040000, Owr3 ~NS); add(0x51040200, Owr3 ~ES)
    add(0x51050000, Nrd4 ~NS); add(0x51050200, Nrd4 ~ES)
    add(0x51100000, Tla5 ~NS); add(0x51100200, Tla5 ~ES); add(0x51100300, Tla5 ~NW)
    add(0x51100100, Tla5 ~CS)  // overwritten explicitly here to avoid 0x71.. ID for stubs
    add(0x51110000, Owr4 ~NS); add(0x51110200, Owr4 ~ES); add(0x51110300, Owr4~SharedDiagRight)
    add(0x51120000, Owr5 ~NS); add(0x51120200, Owr5 ~ES); add(0x51120300, Owr5 ~NW)
    add(0x51130000, Rd4  ~NS); add(0x51130200, Rd4  ~ES); add(0x51130300, Rd4~SharedDiagRight)
    add(0x51140000, Rd6  ~NS); add(0x51140200, Rd6  ~ES); add(0x51140300, Rd6  ~NW)
    add(0x51200000, Ave6 ~NS); add(0x51200200, Ave6 ~NW); add(0x51200300, Ave6 ~ES)
    add(0x51200080, Tla7m~NS); add(0x51200280, Tla7m~ES)
    add(0x51210000, Ave8 ~NS); add(0x51210200, Ave8 ~NW); add(0x51210300, Ave8 ~ES)
    add(0x51210080, Ave6m~NS); add(0x51210280, Ave6m~ES)

    add(0x5c000000, L1Road      ~NS); add(0x5c000200, L1Road      ~ES)
    add(0x5c010000, L1Onewayroad~NS); add(0x5c010200, L1Onewayroad~ES)
    add(0x5c020000, L1Avenue    ~NS); add(0x5c020200, L1Avenue    ~NE); add(0x5c020300, L1Avenue~SharedDiagLeft)
    add(0x5c030000, L2Road      ~NS); add(0x5c030200, L2Road      ~ES)
    add(0x5c040000, L2Onewayroad~NS); add(0x5c040200, L2Onewayroad~ES)
    add(0x5c050000, L2Avenue    ~NS); add(0x5c050200, L2Avenue    ~NE); add(0x5c050300, L2Avenue~SharedDiagLeft)

    add(0x5e54b100, Sam1~NS)
    add(0x5e54b200, Sam2~NS)
    add(0x5e54b300, Sam3~NS)
    add(0x5e54b400, Sam4~NS)
    add(0x5e54b500, Sam5~NS)
    add(0x5e54b600, Sam6~NS)
    add(0x5e54b700, Sam7~NS)
    add(0x5e54b800, Sam8~NS)
    add(0x5e54b900, Sam9~NS)
    add(0x5e54ba00, Sam10~NS)
    add(0x5e54bb00, Sam11~NS)

    add(0x5e572200, Sam2~SE)
    add(0x5e572300, Sam3~SE)
    add(0x5e572400, Sam4~SE)
    add(0x5e572500, Sam5~SE)
    add(0x5e572600, Sam6~SE)
    add(0x5e572700, Sam7~SE)
    add(0x5e572800, Sam8~SE)
    add(0x5e572900, Sam9~SE)
    add(0x5e572a00, Sam10~SE)
    add(0x5e572b00, Sam11~SE)

    add(0x00000300, Road~CS) // orth stub
    add(0x00000200, Road~(0,0,0,1)) // diag stub
    add(0x00020700, Road~NS & Road~WE) // OxO
    add(0x00003900, Road~NS & Road~NE) // OxD
    //add(0x5f040500, Road~NS & Road~SharedDiagRight) // OxD shared-diag TODO create this
    //add(0x5f040600, Road~SW & Road~SharedDiagRight) // DxD shared-diag TODO create this
    //add(0x5f040700, Road~SharedDiagRight & Road~SharedDiagLeft) // DxD shared-diag TODO create this
    add(0x00000700, Road~SW & Road~ES) // DxD
    add(0x00005700, Road~NS & Road~CE) // OxO T
    add(0x00006300, Road~NS & Road~CSE) // OxD T
    add(0x00000F00, Road~(0,0,2,2)) // 90 curve
    add(0x00000C00, Road~(0,0,1,13)) // curve
    add(0x00004D00, Road~(0,2,0,11)) // curve
    //add(0x00014E00, Road~SharedDiagRight) // shared diag TODO check IID
    add(0x00015600, Road~(1,3,11,3)) // add(0x5f040400, Road~(1,3,1,13)) // shared-diag curve
    add(0x00020900, Road~(2,2,2,11))

    add(0x00000100, Street~(0,0,0,0))
    add(0x05000300, Street~CS) // orth stub
    add(0x5F500300, Street~(0,0,0,1)) // diag stub
    add(0x05020700, Street~(2,2,2,2)) // OxO alt
    add(0x05020700, Street~NS & Street~WE) // OxO
    add(0x5F500700, Street~NS & Street~NE) // OxD
    add(0x5F500600, Street~SE & Street~EN) // DxD
    add(0x05005700, Street~NS & Street~CE) // OxO T
    add(0x5F500A00, Street~NS & Street~CSE) // OxD T (also has (0,2,11,2) alt implementation)
    add(0x5F504000, Street~CS & Street~NE) // DxO T1
    add(0x5F504100, Street~CS & Street~WS) // DxO T2
    add(0x5F504200, Street~SE & Street~CEN) // DxD T1
    add(0x5F504300, Street~WN & Street~CSW) // DxD T2
    add(0x05000F00, Street~(0,0,2,2)) // 90 curve
    add(0x5F500400, Street~(0,0,1,13)) // curve
    add(0x5F500500, Street~(0,2,0,11)) // curve
    add(0x5F500800, Street~(0,2,2,11))
    add(0x5F500900, Street~(0,2,2,13))
    add(0x5F500A00, Street~(0,2,11,2)) // alt OxD T
    add(0x5F500B00, Street~(2,2,2,11))
    add(0x5F500C00, Street~(0,11,2,13))
    add(0x5F500D00, Street~(0,0,2,11))
    add(0x5F500E00, Street~(0,2,11,11))
    add(0x5F500F00, Street~(0,2,13,13))
    add(0x5F501100, Street~(0,11,0,13))
    add(0x5F501000, Street~(0,11,0,11))
    add(0x5F501D00, Street~(0,0,2,13))
    add(0x5F501400, Street~(0,0,11,13))
    add(0x5F501500, Street~(1,3,2,2))
    add(0x5F501600, Street~(1,3,2,11))
    add(0x5F501700, Street~(2,2,11,11))
    add(0x5F501800, Street~(2,11,2,11))
    add(0x5F501900, Street~(2,11,2,13))
    add(0x5F501A00, Street~(2,11,11,11))
    add(0x5F501B00, Street~(11,0,13,2))
    // add(0x5F501100, Street~(0,1,0,3))
    add(0x5F501E00, Street~(1,1,1,1))
    add(0x5F501300, Street~(3,0,3,3))
    add(0x5F501200, Street~(11,0,11,2))
    //smaller wide-radius curves
    //2x2 90
    add(0x5F593800, Street~(0,2,0,131))
    add(0x5F593900, Street~(143,0,0,141))
    add(0x5F593A00, Street~(0,131,133,0))
    add(0x5F593F00, Street~(2,2,0,131)) // T-Intersection off outer tile
    add(0x5F593D00, Street~(133,131,133,131)) // Diverter
    add(0x5F593D00, Street~(133,0,0,131) & Street~(0,131,133,0)) // Diverter (alt)
    add(0x5F093E00, Road~(133,0,0,131) & Street~(0,131,133,0))   // Diverter street & road
    //3x2 S
    add(0x5F594000, Street~(2,0,153,0))
    add(0x5F594100, Street~(153,0,0,161))
    add(0x5F594200, Street~(173,0,0,181))
    add(0x5F594300, Street~(2,2,153,0)) //T-Intersection off outer tile
    // diagonal s-curve
    add(0x5F595C00 , Street~(3,0,0,152))
    add(0x5F595D00, Street~(0,152,154,1))
    add(0x5F595E00, Street~(154,0,0,1))
    //Street Roundabout
    add(0x5F56BB00, Street~(0,0,102,102))                         //Base
    add(0x5F56BA00, Street~(0,0,102,102) & Street~NC)             //Base with Orth Street connection
    add(0x5F56B900, Street~(0,0,102,102) & Street~(0,11,0,0))     //Base with Diag Street connection
    //Street Roundabout Connections
    add(0x5F084800, Street~(0,0,102,102) & Road~NC)               //Base with Orth Road connection
    add(0x5F084900, Street~(0,0,102,102) & Road~(0,13,0,0))       //Base with Diag Road connection
    add(0x5F984800, Street~(0,0,102,102) & Onewayroad~NC)         //Base with Orth Onewayroad connection
    add(0x5F984900, Street~(0,0,102,102) & Onewayroad~(0,13,0,0)) //Base with Diag Onewayroad connection
    //Larger (R2-esque) 45 (4x3)
    //Tile 1F is override of Diagonal
    //Tile 22 is override of 1x1 Stub
    add(0x5F592400, Street~(0,2,0,111))
    add(0x5F592300, Street~(0,111,0,11))
    add(0x5F592200, Street~(15,0,0,14))
    add(0x5F592100, Street~(0,11,113,0))
    add(0x5F592000, Street~(113,0,0,1))
    //Larger 90 (4x4)
    add(0x5F592500, Street~(0,2,0,181))
    add(0x5F592600, Street~(0,181,11,191))
    add(0x5F592700, Street~(11,0,0,82))
    add(0x5F592800, Street~(0,191,194,0))
    add(0x5F592900, Street~(0,82,82,0))
    //T-ints off
    add(0x5F592F00, Street~(2,2,0,181))
    add(0x5F592E00, Street~(2,181,11,191))

    add(0x09000300, Onewayroad~CS) // orth stub
    add(0x09000200, Onewayroad~(0,0,0,1)) // diag stub
    add(0x09020700, Onewayroad~NS & Onewayroad~WE) // OxO
    add(0x09003900, Onewayroad~NS & Onewayroad~NE) // OxD
    add(0x5f940500, Onewayroad~NS & Onewayroad~SharedDiagRight) // OxD shared-diag TODO create this
    add(0x5f940600, Onewayroad~SW & Onewayroad~SharedDiagRight) // DxD shared-diag TODO create this
    add(0x5f940700, Onewayroad~SharedDiagRight & Onewayroad~SharedDiagLeft) // DxD shared-diag TODO create this
    add(0x09000700, Onewayroad~SW & Onewayroad~ES) // DxD
    add(0x90005700, Onewayroad~NS & Onewayroad~CE) // OxO T
    add(0x09006300, Onewayroad~NS & Onewayroad~CSE) // OxD T
    add(0x09000F00, Onewayroad~(0,0,2,2)) // 90 curve
    add(0x09000C00, Onewayroad~(0,0,1,13)) // curve
    add(0x09004D00, Onewayroad~(0,2,0,11)) // curve
    add(0x09014E00, Onewayroad~SharedDiagRight) // shared diag
    add(0x09015600, Onewayroad~(1,3,11,3)) // add(0x5f940400, Onewayroad~(1,3,1,13)) // shared-diag curve

    // shared diag OWR intersections TODO create these
    add(0x5f94a800, Onewayroad~SharedDiagRight & Road~NS)
    add(0x5f94a900, Onewayroad~SharedDiagRight & Road~SW)
    //add(0x5f94aa00, Onewayroad~SharedDiagRight & Road~SharedDiagLeft)
    add(0x5f94ab00, Onewayroad~SharedDiagRight & Street~NS)
    add(0x5f94ac00, Onewayroad~SharedDiagRight & Street~SW)
    //add(0x5f94ad00, Onewayroad~SharedDiagRight & Street~SharedDiagLeft)
    add(0x5f94ae00, Onewayroad~SharedDiagRight & Avenue~NS)
    add(0x5f94af00, Onewayroad~SharedDiagRight & Avenue~SW)
    add(0x5f94b000, Onewayroad~SharedDiagRight & Avenue~SharedDiagLeft)
    add(0x5f232b00, Onewayroad~SharedDiagRight & Highway~SN) // 0x5f94b100
    add(0x5f232900, Onewayroad~SharedDiagLeft & Highway~ES) // 0x5f94b200
    add(0x5f232a00, Onewayroad~SharedDiagLeft & Highway~SharedDiagRight) // 0x5f94b300
    //add(0x5f94b400, Onewayroad~SharedDiagRight & Groundhighway~NS)
    //add(0x5f94b500, Onewayroad~SharedDiagRight & Groundhighway~SW)
    //add(0x5f94b600, Onewayroad~SharedDiagRight & Groundhighway~SharedDiagLeft)
    add(0x5f372300, Onewayroad~SharedDiagRight & Rail~NS) // 0x5f94b700
    add(0x5f371e00, Onewayroad~SharedDiagRight & Rail~SW) // 0x5f94b800
    //add(0x5f3572200, Onewayroad~SharedDiagRight & Rail~SharedDiagLeft) // 0x5f94b900
    add(0x5f832900, Onewayroad~SharedDiagLeft & Lightrail~NS) // 0x5f94ba00
    add(0x5f832a00, Onewayroad~SharedDiagLeft & Lightrail~ES) // 0x5f94bb00 (<-- potentially occupied by a legacy piece)
    //add(0x5f94bc00, Onewayroad~SharedDiagRight & Lightrail~SharedDiagLeft)
    add(0x5fd32900, Onewayroad~SharedDiagLeft & Monorail~NS) // 0x5f94bd00
    add(0x5fd32a00, Onewayroad~SharedDiagLeft & Monorail~ES) // 0x5f94be00
    //add(0x5f94bf00, Onewayroad~SharedDiagRight & Monorail~SharedDiagLeft)

    add(0x04007300, Avenue~CS) // orth stub
    add(0x04006400, Avenue~(-2,0,0,+2)) // 90 curve inside
    add(0x04006500, Avenue~(+2,0,0,-2)) // 90 curve outside
    add(0x04006300, Avenue~(+2,0,-113,0)) // 90 curve extended
    add(0x04007600, Avenue~(0,-2,0,+11)) // curve
    add(0x04007700, Avenue~(0,+2,0,-11)) // curve
    add(0x04007800, Avenue~(0,-11,+3,0)) // curve
    add(0x04007900, Avenue~(-3,+11,-3,+1)) // curve
    add(0x04009000, Avenue~WE & Avenue~NS) // OxO
    add(0x04007400, Avenue~NS & Avenue~NE) // OxD
    add(0x04007500, Avenue~SN & Avenue~NE) // OxD
    add(0x04009300, Avenue~SN & Avenue~SharedDiagLeft) // OxD
    add(0x04006900, Avenue~ES & Avenue~SharedDiagLeft) // DxD
    add(0x04009400, Avenue~SharedDiagRight & Avenue~SharedDiagLeft) // DxD
    add(0x04003600, Avenue~ES & Avenue~NE) // DxD

    // Road intersections
    add(0x03010100, Road~NS & Rail~WE)
    builder.addOne((Road~NS).projectLeft  & Rail~NE, IdTile(0x03010200, R0F0, nonMirroredOnly))
    builder.addOne((Road~NS).projectRight & Rail~NE, IdTile(0x03020500, R0F0, mirroredOnly))
    builder.addOne((Road~WN).projectLeft  & Rail~NS, IdTile(0x03020100, R0F0, nonMirroredOnly))
    builder.addOne((Road~WN).projectRight & Rail~NS, IdTile(0x03020400, R0F0, mirroredOnly))
    builder.addOne((Road~ES).projectLeft  & Rail~NE, IdTile(0x03020200, R0F0, nonMirroredOnly))
    builder.addOne((Road~NW).projectRight & Rail~NE, IdTile(0x03020300, R0F0, nonMirroredOnly))
    add(0x02014000, Road~WE & Highway~NS)
    add(0x02014100, Road~ES & Highway~NS)
    add(0x02014200, Road~WN & Highway~NS)
    add(0x02014300, Road~WE & Highway~ES)
    add(0x02014400, Road~WE & Highway~SharedDiagRight)
    add(0x02014500, Road~NE & Highway~ES)
    add(0x02014600, Road~SW & Highway~SharedDiagRight)
    add(0x00aa0a00, Road~WE & Street~NS)
    add(0x00aa0c00, Road~ES & Street~WE)
    add(0x5f502100, Road~NS & Street~CSE)
    add(0x5f502200, Road~NS & Street~ES)
    add(0x5f502300, Road~CS & Street~NE)
    add(0x5f502400, Road~ES & Street~NE)
    add(0x5f504400, Road~ES & Street~CEN)
    add(0x00aa0800, Road~WE & Street~CS)
    add(0x00aa0400, Road~CE & Street~NS)
    add(0x00aa0100, Road~NC & Street~CS)
    add(0x00aa0200, Road~CE & Street~NC)
    add(0x00aa0d00, Road~CWN & Street~WE)
    add(0x00aa0300, Road~(0,0,2,0) & Street~(2,2,0,0))
    add(0x00aa0500, Road~(0,0,2,0) & Street~(2,2,0,2))
    add(0x00aa0700, Road~(2,2,0,0) & Street~(0,0,0,2))
    add(0x00aa0900, Road~(2,2,0,0) & Street~(0,0,2,2))
    add(0x00aa0b00, Road~(2,2,2,0) & Street~(0,0,0,2))
    // add(0x00aa0d00, Road~(0,1,0,0) & Street~WE) //alt implementation of 0x00aa0d00
    // add(0x00aa0d00, Road~(0,11,0,0) & Street~WE) //alt implementation of 0x00aa0d00
    add(0x00aa0e00, Road~CWN & Street~CE)
    add(0x00aa1100, Road~CWN & Street~WC)
    add(0x5F502000, Road~CES & Street~CSE)
    add(0x5F502500, Road~CES & Street~NE)
    add(0x5F072300, Road~(0,0,1,3) & Street~(2,2,0,0))
    add(0x04008900, Road~WE & Avenue~NS)
    add(0x04001300, Road~ES & Avenue~SN)
    add(0x04001900, Road~WN & Avenue~SN)
    add(0x04005700, Road~NS & Avenue~ES)
    add(0x04006000, Road~NS & Avenue~SharedDiagRight)
    add(0x04020200, Road~ES & Avenue~NE)
    add(0x04023800, Road~ES & Avenue~SharedDiagLeft)
    add(0x08dd0100, Road~ES & Lightrail~NS)
    add(0x08dd1600, Road~NS & Lightrail~ES)
    add(0x08dd1700, Road~SW & Lightrail~ES)
    add(0x08dd0200, Road~EW & Lightrail~NS)
    add(0x0ddd0100, Road~ES & Monorail~NS)
    add(0x0ddd1600, Road~NS & Monorail~ES)
    add(0x0ddd1700, Road~SW & Monorail~ES)
    add(0x0ddd0200, Road~EW & Monorail~NS)
    add(0x09700700, Road~ES & Onewayroad~SW)
    add(0x09703900, Road~NE & Onewayroad~NS)
    add(0x09720700, Road~NS & Onewayroad~EW)
    add(0x09803900, Road~NS & Onewayroad~NE)
    add(0x09720400, Road~(2,2,2,0) & Onewayroad~(0,0,0,2))
    add(0x5f973300, Road~(0,2,2,2) & Onewayroad~(3,0,0,0))
    add(0x5f072a00, Road~(2,0,2,2) & Street~(0,1,0,0)    )
    add(0x5760b400, Road~(2,2,0,2) & Dirtroad~(0,0,2,0)  )
    // Road corner intersections
    add(0x5f972500, Road~(0,0,2,2) & Onewayroad~(0,1,0,0))
    add(0x5f973100, Road~(0,0,2,2) & Onewayroad~(0,3,0,0))
    add(0x09705500, Road~(0,2,2,0) & Onewayroad~CS)
    add(0x09720500, Road~(0,2,2,0) & Onewayroad~(2,0,0,2))
    add(0x5f072800, Road~(0,0,2,2) & Street~(0,1,0,0))
    add(0x5f072900, Road~(0,0,2,2) & Street~(0,3,0,0))
    add(0x5f072600, Road~(0,0,2,2) & Street~(2,1,0,0))
    add(0x5f072700, Road~(0,0,2,2) & Street~(2,3,0,0))
    add(0x5760c700, Road~(0,2,2,0) & Dirtroad~CS)
    add(0x5760b500, Road~(2,2,0,0) & Dirtroad~(0,0,2,2))

    // Road roundabouts
    add(0x5F06BC80, RdRndbt~(0,2,0,-2))                       // surrogate tile "straight roundabout"
    add(0x5F06BC00, RdRndbt~(0,0,-2,2))                       // INRUL
    add(0x5F06BD00, RdRndbt~(0,0,-2,2) & RdRndbt~(2,-2,0,0))  // INRUL
    add(0x5F06BA00, RdRndbt~(0,0,-2,2) & Road~NC)             // INRUL
    add(0x5F06BE00, RdRndbt~(0,0,-2,2) & Road~(0,1,0,0))      // INRUL
    add(0x5F06B800, RdRndbt~(0,0,-2,2) & Road~(0,3,0,0))      // INRUL
    add(0x5F06C100, RdRndbt~(0,0,-2,2) & Road~WC & Road~NC)
    add(0x5F06CB00, RdRndbt~(0,0,-2,2) & Road~WC & Road~CEN)
    add(0x5F06CA00, RdRndbt~(0,0,-2,2) & Road~WC & Road~CWN)
    add(0x5F06C200, RdRndbt~(0,0,-2,2) & Road~WC & Street~NC)
    add(0x5F06CD00, RdRndbt~(0,0,-2,2) & Road~WC & Street~CEN)
    add(0x5F06CC00, RdRndbt~(0,0,-2,2) & Road~WC & Street~CWN)
    add(0x5F06C500, RdRndbt~(0,0,-2,2) & Road~WC & Onewayroad~NC)
    add(0x5F96C000, RdRndbt~(0,0,-2,2) & Road~WC & Onewayroad~CEN)
    add(0x5F96C600, RdRndbt~(0,0,-2,2) & Road~WC & Onewayroad~CWN)
    add(0x5F96C400, RdRndbt~(0,0,-2,2) & Road~WC & Owr1~NC)
    add(0x5F96C500, RdRndbt~(0,0,-2,2) & Road~WC & Owr1~CEN)
    add(0x5760DD00, RdRndbt~(0,0,-2,2) & Road~WC & Dirtroad~NC)
    add(0x5760DE00, RdRndbt~(0,0,-2,2) & Road~WC & Mis~NC)
    add(0x5760DE80, RdRndbt~(0,0,-2,2) & Road~WC & Mis~CN)
    add(0x5F06B100, RdRndbt~(0,0,-2,2) & Street~NC)
    add(0x5F06B300, RdRndbt~(0,0,-2,2) & Street~CEN)
    add(0x5F06B500, RdRndbt~(0,0,-2,2) & Street~CWN)
    add(0x5F06C300, RdRndbt~(0,0,-2,2) & Street~WC & Street~NC)
    add(0x5F06C800, RdRndbt~(0,0,-2,2) & Street~WC & Street~CEN)
    add(0x5F06C700, RdRndbt~(0,0,-2,2) & Street~WC & Street~CWN)
    add(0x5F96BA00, RdRndbt~(0,0,-2,2) & Onewayroad~NC)
    add(0x5F06C400, RdRndbt~(0,0,-2,2) & Onewayroad~(2,2,0,0))
    add(0x5F96BE00, RdRndbt~(0,0,-2,2) & Onewayroad~(0,1,0,0))
    add(0x5F96B800, RdRndbt~(0,0,-2,2) & Onewayroad~(0,3,0,0))
    add(0x5F06C600, RdRndbt~(0,0,-2,2) & Onewayroad~WC & Street~NC)
    add(0x5F96C100, RdRndbt~(0,0,-2,2) & Owr1~NC)
    add(0x5F96C200, RdRndbt~(0,0,-2,2) & Owr1~(0,1,0,0))
    add(0x5F96C300, RdRndbt~(0,0,-2,2) & Owr1~(0,3,0,0))
    add(0x5F96C700, RdRndbt~(0,0,-2,2) & Owr1~(2,2,0,0))
    add(0x5760D800, RdRndbt~(0,0,-2,2) & Dirtroad~NC)
    add(0x5760D880, RdRndbt~(0,0,-2,2) & Dirtroad~(2,2,0,0))
    add(0x5760DC00, RdRndbt~(0,0,-2,2) & Dirtroad~WC & Mis~NC)
    add(0x5760DC80, RdRndbt~(0,0,-2,2) & Dirtroad~WC & Mis~CN)
    add(0x5760D900, RdRndbt~(0,0,-2,2) & Mis~NC)
    add(0x5760D980, RdRndbt~(0,0,-2,2) & Mis~CN)
    add(0x5760DA00, RdRndbt~(0,0,-2,2) & Mis~WC & Mis~NC)
    add(0x5760DA80, RdRndbt~(0,0,-2,2) & Mis~WC & Mis~CN)
    add(0x5760DB80, RdRndbt~(0,0,-2,2) & Mis~CW & Mis~NC)


    // Street intersections
    add(0x05010100, Street~NS & Rail~WE)
    add(0x05010200, Street~NS & Rail~NE)
    builder.addOne((Street~WN).projectLeft  & Rail~NS, IdTile(0x5f502600, R0F0, nonMirroredOnly))
    builder.addOne((Street~WN).projectRight & Rail~NS, IdTile(0x5f502900, R0F0, mirroredOnly))
    builder.addOne((Street~ES).projectLeft  & Rail~NE, IdTile(0x5f502700, R0F0, nonMirroredOnly))
    builder.addOne((Street~NW).projectRight & Rail~NE, IdTile(0x5f502800, R0F0, nonMirroredOnly))
    add(0x02015000, Street~WE & Highway~NS)
    add(0x5F514100, Street~ES & Highway~NS)
    add(0x5F514200, Street~WN & Highway~NS)
    add(0x02015300, Street~WE & Highway~ES)
    add(0x02015400, Street~WE & Highway~SharedDiagRight)
    add(0x5F514500, Street~NE & Highway~ES)
    add(0x5F514600, Street~SW & Highway~SharedDiagRight)
    add(0x5F502B00, Street~ES & Lightrail~NS)
    add(0x08dd1000, Street~NS & Lightrail~ES)
    add(0x5F502C00, Street~SW & Lightrail~ES)
    add(0x08dd0800, Street~EW & Lightrail~NS)
    add(0x5F502D00, Street~ES & Monorail~NS)
    add(0x0ddd1000, Street~NS & Monorail~ES)
    add(0x5F502E00, Street~SW & Monorail~ES)
    add(0x0ddd0800, Street~EW & Monorail~NS)


    // OWR intersections
    add(0x09310100, Onewayroad~NS & Rail~WE)
    add(0x09310200, Onewayroad~NS & Rail~NE)
    add(0x09320100, Onewayroad~WN & Rail~NS)
    builder.addOne((Onewayroad~ES).projectLeft  & Rail~NE, IdTile(0x09320200, R0F0, nonMirroredOnly))
    builder.addOne((Onewayroad~NW).projectRight & Rail~NE, IdTile(0x09320300, R0F0, nonMirroredOnly))
    add(0x09b14000, Onewayroad~WE & Highway~NS)
    add(0x09b14100, Onewayroad~ES & Highway~NS)
    add(0x09b14200, Onewayroad~WN & Highway~NS)
    add(0x09b14300, Onewayroad~WE & Highway~ES)
    add(0x09b14400, Onewayroad~WE & Highway~SharedDiagRight)
    add(0x09b14500, Onewayroad~NE & Highway~ES)
    add(0x09b14600, Onewayroad~SW & Highway~SharedDiagRight)
    add(0x09aa0a00, Onewayroad~WE & Street~NS)
    add(0x09aa0c00, Onewayroad~ES & Street~WE)
    add(0x5f503400, Onewayroad~NS & Street~ES)
    add(0x5f503500, Onewayroad~NS & Street~CSE)
    add(0x5f503600, Onewayroad~ES & Street~NE)
    add(0x5f503700, Onewayroad~CS & Street~NE)
    add(0x5f504600, Onewayroad~ES & Street~CEN)
    add(0x09aa0800, Onewayroad~WE & Street~CS)
    add(0x09aa0400, Onewayroad~CE & Street~NS)
    add(0x09aa0100, Onewayroad~NC & Street~CS)
    add(0x09aa0200, Onewayroad~CE & Street~NC)
    add(0x09aa0d00, Onewayroad~CWN & Street~WE)
    add(0x09aa0300, Onewayroad~(0,0,2,0) & Street~(2,2,0,0))
    add(0x09aa0500, Onewayroad~(0,0,2,0) & Street~(2,2,0,2))
    add(0x09aa0700, Onewayroad~(2,2,0,0) & Street~(0,0,0,2))
    add(0x09aa0900, Onewayroad~(2,2,0,0) & Street~(0,0,2,2))
    add(0x09aa0b00, Onewayroad~(2,2,2,0) & Street~(0,0,0,2))
    add(0x09aa0e00, Onewayroad~CWN & Street~CE)
    add(0x09aa1100, Onewayroad~CWN & Street~WC)
    add(0x5F503800, Onewayroad~CES & Street~NE)
    add(0x5F974300, Onewayroad~ES & Street~(2,2,0,0))
    add(0x091a8900, Onewayroad~WE & Avenue~NS)
    add(0x091a1300, Onewayroad~ES & Avenue~SN)
    add(0x091a1900, Onewayroad~WN & Avenue~SN)
    add(0x091a5700, Onewayroad~NS & Avenue~ES)
    add(0x091a6000, Onewayroad~NS & Avenue~SharedDiagRight)
    add(0x091a0200, Onewayroad~ES & Avenue~NE)
    add(0x091a3800, Onewayroad~ES & Avenue~SharedDiagLeft)
    add(0x091d0100, Onewayroad~ES & Lightrail~NS)
    add(0x091d1600, Onewayroad~NS & Lightrail~ES)
    add(0x091d1700, Onewayroad~SW & Lightrail~ES)
    add(0x091d0200, Onewayroad~EW & Lightrail~NS)
    add(0x092d0100, Onewayroad~ES & Monorail~NS)
    add(0x092d1600, Onewayroad~NS & Monorail~ES)
    add(0x092d1700, Onewayroad~SW & Monorail~ES)
    add(0x092d0200, Onewayroad~EW & Monorail~NS)

    // Avenue + intersections
    add(0x04002100, Avenue~ES & Rail~NE)
    add(0x04004300, Avenue~SharedDiagRight & Rail~SW)
    builder.addOne((Avenue~SN).projectLeft  & Rail~NE, IdTile(0x04001600, R0F0, nonMirroredOnly))
    builder.addOne((Avenue~SN).projectRight & Rail~NE, IdTile(0x5d571600, R0F0, mirroredOnly))
    builder.addOne((Avenue~NS).projectLeft  & Rail~NE, IdTile(0x04001700, R0F0, nonMirroredOnly))
    builder.addOne((Avenue~NS).projectRight & Rail~NE, IdTile(0x5d571700, R0F0, mirroredOnly))
    add(0x04001500, Avenue~SN & Rail~WE)
    add(0x04004700, Avenue~ES & Rail~NS)
    add(0x04004600, Avenue~SharedDiagRight & Rail~NS)
    add(0x04010000, Avenue~NS & Highway~EW)
    add(0x04010700, Avenue~SN & Highway~SW)
    add(0x04010600, Avenue~NS & Highway~SW)
    add(0x04010500, Avenue~NS & Highway~SharedDiagRight)
    add(0x04010300, Avenue~NE & Highway~SharedDiagRight)
    add(0x04010400, Avenue~SharedDiagLeft & Highway~ES)
    add(0x04010100, Avenue~NE & Highway~ES)
    add(0x04010200, Avenue~SharedDiagLeft & Highway~SharedDiagRight)
    add(0x04010800, Avenue~SharedDiagRight & Highway~SN)
    add(0x04010900, Avenue~ES & Highway~NS)
    add(0x04011000, Avenue~ES & Highway~SN)
    add(0x04008300, Avenue~SN & Street~WE)
    add(0x5f577800, Avenue~SharedDiagRight & Street~NS)
    add(0x04004500, Avenue~ES & Street~WC)
    add(0x5f577900, Avenue~ES & Street~WE)
    add(0x5f503000, Avenue~SN & Street~ES)
    add(0x5f503100, Avenue~SN & Street~WN)
    add(0x5f503200, Avenue~NE & Street~ES)
    add(0x5f503300, Avenue~SharedDiagLeft & Street~ES)
    add(0x04004400, Avenue~SN & Street~CE)
    add(0x04008600, Avenue~SN & Street~WC)
    add(0x04005600, Avenue~NC & Street~WE)
    add(0x04009600, Avenue~NC & Street~(2,0,2,2))
    add(0x04009700, Avenue~NC & Street~(0,0,0,2))
    add(0x04008400, Avenue~SC & Street~(0,0,2,0))
    add(0x08dd0500, Avenue~NE & Lightrail~NS)
    add(0x08dd0600, Avenue~SharedDiagLeft & Lightrail~NS)
    add(0x08dd0700, Avenue~EW & Lightrail~NS)
    add(0x08dd1100, Avenue~NS & Lightrail~ES)
    add(0x08dd0900, Avenue~SN & Lightrail~ES)
    add(0x08dd1200, Avenue~SharedDiagLeft & Lightrail~ES)
    add(0x08dd1300, Avenue~NE & Lightrail~ES)
    add(0x0ddd0500, Avenue~NE & Monorail~NS)
    add(0x0ddd0600, Avenue~SharedDiagLeft & Monorail~NS)
    add(0x0ddd0700, Avenue~EW & Monorail~NS)
    add(0x0ddd1100, Avenue~NS & Monorail~ES)
    add(0x0ddd0900, Avenue~SN & Monorail~ES)
    add(0x0ddd1200, Avenue~SharedDiagLeft & Monorail~ES)
    add(0x0ddd1300, Avenue~NE & Monorail~ES)

    add(0x57000800, Dirtroad~(0,0,2,2))
    for (n <- RhwNetworks rangeFrom Mis rangeTo L4Mis) {
      val offset = 0x100000 * n.height
      add(0x57020800 + offset, n~(0,0,-2,+2))    // Mis 90 curve
      add(0x57020e00 + offset, n~(0,0,+2,-2))    //
      add(0x57020880 + offset, n~(+111,0,-2,0))  // Mis 90 curve approach
      add(0x57020e80 + offset, n~(-111,0,+2,0))  //
    }
    for (n <- RhwNetworks rangeFrom Dirtroad rangeTo L4Rhw6s) {  // single-tile RHW networks
      val rangeId = RhwResolver.rhwRangeId(n) & 0x000F0000
      val offset = rangeId + n.height * 0x10 + (if (n.height == 0) 0 else 5)
      add(0x57905000 + offset, n~(+2,0,-123,0))    // R1 curve
      add(0x57905100 + offset, n~(+123,0,0,-111))  // R1 curve
      add(0x57905200 + offset, n~(0,+111,-3,0))    // R1 curve
      add(0x57905F00 + offset, n~(0,+111,-113,0))  // R1 curve 90 degree
      if (!n.isSymm) {
        add(0x57905080 + offset, n~(-2,0,+123,0))    // R1 curve
        add(0x57905180 + offset, n~(-123,0,0,+111))  // R1 curve
        add(0x57905280 + offset, n~(0,-111,+3,0))    // R1 curve
        add(0x57905F80 + offset, n~(0,-111,+113,0))  // R1 curve 90 degree
      }
    }
    add(0x57001000, Dirtroad~NS & Street~WE)
    add(0x57004000, Dirtroad~NS & Street~WS)
    add(0x57007000, Dirtroad~SE & Street~WE)
    add(0x5700a000, Dirtroad~SE & Street~WS)

    add(0x57600000, Dirtroad~NS & Street~CE)
    add(0x57601000, Dirtroad~CE & Street~NS)
    add(0x57600100, Dirtroad~NS & Road~CE)
    add(0x57601100, Dirtroad~CE & Road~NS)
    add(0x57600200, Dirtroad~NS & Onewayroad~CE)
    add(0x57601200, Dirtroad~CE & Onewayroad~NS)

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
        add(RhwResolver.rhwHtRangeId(rhw) + 0x100*(levelDiff-1) + 0x10*height, upper~NC & lower~CS)  // direction North (upper) to South (lower)
      }
    }

    // GLR + intersections
    for ((glr, offset) <- Seq(Glr1, Glr2, Glr3, Glr4).zip(Seq(0, 0x4000, 0x8000, 0xc000))) {
      // O×O
      add(0x5f880300 + offset, glr~NS & Road~WE)
      add(0x5f880d00 + offset, glr~NS & Street~WE)
      add(0x5f880e00 + offset, glr~NS & Onewayroad~WE)
      add(0x5f880f00 + offset, glr~NS & Avenue~WE)
      add(0x5f881000 + offset, glr~NS & Rail~WE)
      // O×D
      add(0x5f881800 + offset, glr~NS & Road~WN)
      add(0x5f882800 + offset, glr~NS & Street~NW)
      add(0x5f881900 + offset, glr~NS & Onewayroad~WN)
      add(0x5f881909 + offset, glr~NS & Onewayroad~SharedDiagRight)  // TODO add placeholder texture
      add(0x5f881a00 + offset, glr~NS & Rail~WN)
      add(0x5f881b00 + offset, glr~NS & Avenue~ES)
      add(0x5f881c00 + offset, glr~NS & Avenue~SharedDiagRight)
      // D×O
      add(0x5f881d00 + offset, glr~NE & Road~NS)
      add(0x5f882900 + offset, glr~NE & Street~NS)
      add(0x5f881e00 + offset, glr~NE & Onewayroad~NS)
      add(0x5f881f00 + offset, glr~NE & Rail~NS)
      add(0x5f882000 + offset, glr~NE & Avenue~SN)
      add(0x5f882100 + offset, glr~NE & Avenue~NS)
      // D×D
      add(0x5f882200 + offset, glr~NE & Road~NW)
      add(0x5f882a00 + offset, glr~NE & Street~NW)
      add(0x5f882300 + offset, glr~NE & Onewayroad~NW)
      add(0x5f882309 + offset, glr~WS & Onewayroad~SharedDiagRight)  // TODO add placeholder texture
      add(0x5f882400 + offset, glr~NE & Rail~NW)
      add(0x5f882500 + offset, glr~NE & Avenue~ES)
      add(0x5f882600 + offset, glr~WS & Avenue~SharedDiagRight)
    }

    // NWM x Street T-intersections
    // Street thru, NWM ends
    // OxO
    add(0x51004000, Street~NS & Tla3~CE) // Tla3 Ends
    add(0x51014000, Street~NS & Ave2~CE) // Ave2 Ends
    add(0x51024000, Street~NS & Ard3~CE) // Ard3 Ends
    add(0x51034000, Street~NS & Owr1~CE) // Owr1 Ends
    add(0x51044000, Street~NS & Owr3~CE) // Owr3 Ends
    add(0x51054000, Street~NS & Nrd4~CE) // Nrd4 Ends
    // OxD
    add(0x5100B500, Street~NS & Tla3~CSE) // Tla3 Ends
    add(0x5101B500, Street~NS & Ave2~CSE) // Ave2 Ends
    add(0x5102B500, Street~NS & Ard3~CSE) // Ard3 Ends
    add(0x5103B500, Street~NS & Owr1~CSE) // Owr1 Ends
    add(0x5104B500, Street~NS & Owr3~CSE) // Owr1 Ends
    add(0x5105B500, Street~NS & Nrd4~CSE) // Nrd4 Ends
    // NWM thru, Street ends
    // OxO short T
    add(0x51003000, Street~CS & Tla3~WE)
    add(0x51013000, Street~CS & Ave2~WE)
    add(0x51023000, Street~CS & Ard3~WE)
    add(0x51023080, Street~CS & Ard3~EW)
    add(0x51033000, Street~CS & Owr1~WE)
    add(0x51043000, Street~CS & Owr3~WE)
    add(0x51053000, Street~CS & Nrd4~WE)
    add(0x51103000, Street~CN & (Tla5~EW).projectLeft)
    add(0x71103000, Street~CN & (Tla5~EW).projectRight)
    add(0x51113000, Street~CN & Owr4~EW)
    add(0x51123000, Street~CN & Owr5~EW)
    add(0x51133000, Street~CN & Rd4~EW)
    add(0x51143000, Street~CN & Rd6~EW)
    add(0x51203000, Street~CN & Ave6~EW)

    builder.result()
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)
}
