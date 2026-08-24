package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, RotFlip._, Flags._
import Implicits.segmentToTile

class HybridRailwayResolver extends IdResolver {

  val tileMap: scala.collection.Map[Tile, IdTile] = {
    val builder = new ResolverBuilder
    import builder.add

    // -- HybridRailway --
    // -- DTR HybridRailway Symmetrical --
    add(0x5da40000, Hrw~NS)
    add(0x5da40100, Hrw~ES)
    add(0x5da40200, Hrw~CS)
    add(0x5da40300, Hrw~CWS)
    add(0x5dac0000, L1Hrw~NS)
    add(0x5dae0000, L2Hrw~NS)
    add(0x5dac0100, L1Hrw~SE)
    add(0x5dae0100, L2Hrw~SE)
    // -- STR HybridRailway Symmetrical --
    //add(0x5da00000, Hrws~NS)
    //add(0x5da00100, Hrws~ES)
    //add(0x5da00200, Hrws~CS)
    //add(0x5da00300, Hrws~CWS)
    //add(0x5da80000, L1Hrws~NS)
    //add(0x5daa0000, L2Hrws~NS)
    //add(0x5da80100, L1Hrws~SE)
    //add(0x5daa0100, L2Hrws~SE)

	// ----- OxO -----
    // -- Street --
    add(0x5dac1000, Street~WE & L1Hrw~NS)
    add(0x5dae1000, Street~WE & L2Hrw~NS)
    // -- Road --
    add(0x5dac1100, Road~WE & L1Hrw~NS)
    add(0x5dae1100, Road~WE & L2Hrw~NS)
    add(0x5dae1105, L1Road~NS & L2Hrw~WE)
    add(0x5dac110a, L2Road~WE & L1Hrw~NS)
    // -- OWR --
    add(0x5dac1200, Onewayroad~WE & L1Hrw~NS)
    add(0x5dae1200, Onewayroad~WE & L2Hrw~NS)
    add(0x5dae1205, L1Onewayroad~NS & L2Hrw~WE)
    add(0x5dac120a, L2Onewayroad~NS & L1Hrw~WE)
    // -- Avenue --
    add(0x5dac1300, Avenue~EW & L1Hrw~NS)
    add(0x5dae1300, Avenue~EW & L2Hrw~NS)
    add(0x5dae1305, L1Avenue~NS & L2Hrw~WE)
    add(0x5dac130a, L2Avenue~EW & L1Hrw~NS)
    // -- Rail --
    add(0x5dac1500, Rail~WE & L1Hrw~NS)
    add(0x5dae1500, Rail~WE & L2Hrw~NS)
    add(0x5dae1510, L2Hrw~WE & L1Hrw~NS)
    // -- STR --
    add(0x5dac1505, Str~WE & L1Hrw~NS)
    add(0x5dae1505, Str~WE & L2Hrw~NS)
    // -- RHW-2
    add(0x5dac1a00, Dirtroad~WE & L1Hrw~NS)
    add(0x5dae1a00, Dirtroad~WE & L2Hrw~NS)
    // -- RHW-3 --
    add(0x5dac1b00, Rhw3~WE & L1Hrw~NS)
    add(0x5dae1b00, Rhw3~WE & L2Hrw~NS)
    // -- MIS --
    add(0x5dac1c00, Mis~WE & L1Hrw~NS)
    add(0x5dae1c00, Mis~WE & L2Hrw~NS)
    // -- RHW-4 --
    add(0x5dac1d00, Rhw4~WE & L1Hrw~NS)
    add(0x5dae1d00, Rhw4~WE & L2Hrw~NS)
    // -- RHW-6S --
    add(0x5dac1e00, Rhw6s~WE & L1Hrw~NS)
    add(0x5dae1e00, Rhw6s~WE & L2Hrw~NS)
    // TO DO - what to do with extra tile 5d771e05, 5d771e10?
    // -- RHW-8S Median
    add(0x5dac1f00, Rhw8sm~WE & L1Hrw~NS)
    add(0x5dae1f00, Rhw8sm~WE & L2Hrw~NS)
    // -- RHW-8S Shoulder --
    add(0x5dac2000, Rhw8s~EW & L1Hrw~NS)
    add(0x5dae2000, Rhw8s~EW & L2Hrw~NS)
    // -- RHW-10S Shoulder --
    add(0x5dac2100, Rhw10s~EW & L1Hrw~NS)
    add(0x5dae2100, Rhw10s~EW & L2Hrw~NS)
    // -- RHW-6C Median --
    add(0x5dac2300, Rhw6cm~WE & L1Hrw~NS)
    add(0x5dae2300, Rhw6cm~WE & L2Hrw~NS)
    // -- RHW-6C Shoulder --
    add(0x5dac2400, Rhw6c~EW & L1Hrw~NS)
    add(0x5dae2400, Rhw6c~EW & L2Hrw~NS)
    // -- RHW-8C Shoulder --
    add(0x5dac2500, Rhw8c~EW & L1Hrw~NS)
    add(0x5dae2500, Rhw8c~EW & L2Hrw~NS)
    // -- TLA-3 --
    add(0x5dac2700, Tla3~WE & L1Hrw~NS)
    add(0x5dae2700, Tla3~WE & L2Hrw~NS)
    // -- AVE-2 --
    add(0x5dac2800, Ave2~WE & L1Hrw~NS)
    add(0x5dae2800, Ave2~WE & L2Hrw~NS)
    // -- ARD-3 --
    add(0x5dac2900, Ard3~WE & L1Hrw~NS)
    add(0x5dae2900, Ard3~WE & L2Hrw~NS)
    // -- OWR-1 --
    add(0x5dac2a00, Owr1~WE & L1Hrw~NS)
    add(0x5dae2a00, Owr1~WE & L2Hrw~NS)
    // -- OWR-3 --
    add(0x5dac2b00, Owr3~WE & L1Hrw~NS)
    add(0x5dae2b00, Owr3~WE & L2Hrw~NS)
    // -- NRD-4 --
    add(0x5dac2c00, Nrd4~WE & L1Hrw~NS)
    add(0x5dae2c00, Nrd4~WE & L2Hrw~NS)
    // -- TLA-5 --
    add(0x5dac2d00, Tla5~EW & L1Hrw~NS)
    add(0x5dae2d00, Tla5~EW & L2Hrw~NS)
    // -- OWR-4 --
    add(0x5dac2e00, Owr4~EW & L1Hrw~NS)
    add(0x5dae2e00, Owr4~EW & L2Hrw~NS)
    add(0x5dac2e05, Owr4m~EW & L1Hrw~NS)
    add(0x5dae2e05, Owr4m~EW & L2Hrw~NS)
    // -- OWR-5 --
    add(0x5dac2f00, Owr5~EW & L1Hrw~NS)
    add(0x5dae2f00, Owr5~EW & L2Hrw~NS)
    // -- RD-4 --
    add(0x5dac3000, Rd4~EW & L1Hrw~NS)
    add(0x5dae3000, Rd4~EW & L2Hrw~NS)
    // -- RD-6 --
    add(0x5dac3100, Rd6~EW & L1Hrw~NS)
    add(0x5dae3100, Rd6~EW & L2Hrw~NS)
    // -- TLA-7 Shoulder / Ave6? -- 3200
    add(0x5dac3200, Ave6~EW & L1Hrw~NS)
    add(0x5dae3200, Ave6~EW & L2Hrw~NS)
    // -- TLA-7 Median --
    add(0x5dac3300, Tla7m~WE & L1Hrw~NS)
    add(0x5dae3300, Tla7m~WE & L2Hrw~NS)
    // -- TLA-9 Shoulder -- 3400
    // -- AVE-6 Median
    add(0x5dac3500, Ave6m~WE & L1Hrw~NS)
    add(0x5dae3500, Ave6m~WE & L2Hrw~NS)

    builder.result()
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)

}
