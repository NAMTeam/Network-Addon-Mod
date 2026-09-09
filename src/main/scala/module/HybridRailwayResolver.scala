package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, RotFlip._, Flags._
import Implicits.segmentToTile

class HybridRailwayResolver extends IdResolver {

  val tileMap: scala.collection.Map[Tile, IdTile] = {
    val builder = new ResolverBuilder
    import builder.add

    // Construct tile
    //add(0x5da40080, Hrw~(0,0,0,0))
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


    // Overhangs
    //add(0x5dac1e80, Hrw6oxoL1~NS)
    //add(0x5dae1e80, Hrw6oxoL2~NS)
    //add(0x5dac4e80, Hrw6oxdL1~NS)
    //add(0x5dae4e80, Hrw6oxdL2~NS)
    //add(0x5dac7e80, Hrw6dxoL1~SE)
    //add(0x5dae7e80, Hrw6dxoL2~SE)
    //add(0x5dacae80, Hrw6dxdL1~SE)
    //add(0x5daeae80, Hrw6dxdL2~SE)

	// ----- OxO -----
    add(0x5da40e00, Rail~CS & Monorail~CN)
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
    add(0x5dac2000, Rhw8s~WE & L1Hrw~NS)
    add(0x5dae2000, Rhw8s~WE & L2Hrw~NS)
    // -- RHW-10S Shoulder --
    add(0x5dac2100, Rhw10s~WE & L1Hrw~NS)
    add(0x5dae2100, Rhw10s~WE & L2Hrw~NS)
    // -- RHW-6C Median --
    add(0x5dac2300, Rhw6cm~WE & L1Hrw~NS)
    add(0x5dae2300, Rhw6cm~WE & L2Hrw~NS)
    // -- RHW-6C Shoulder --
    add(0x5dac2400, Rhw6c~WE & L1Hrw~NS)
    add(0x5dae2400, Rhw6c~WE & L2Hrw~NS)
    // -- RHW-8C Shoulder --
    add(0x5dac2500, Rhw8c~WE & L1Hrw~NS)
    add(0x5dae2500, Rhw8c~WE & L2Hrw~NS)
    // -- TLA-3 --
    add(0x5dac2700, Tla3~EW & L1Hrw~NS)
    add(0x5dae2700, Tla3~EW & L2Hrw~NS)
    // -- AVE-2 --
    add(0x5dac2800, Ave2~EW & L1Hrw~NS)
    add(0x5dae2800, Ave2~EW & L2Hrw~NS)
    // -- ARD-3 --
    add(0x5dac2900, Ard3~EW & L1Hrw~NS)
    add(0x5dae2900, Ard3~EW & L2Hrw~NS)
    // -- OWR-1 --
    add(0x5dac2a00, Owr1~EW & L1Hrw~NS)
    add(0x5dae2a00, Owr1~EW & L2Hrw~NS)
    // -- OWR-3 --
    add(0x5dac2b00, Owr3~EW & L1Hrw~NS)
    add(0x5dae2b00, Owr3~EW & L2Hrw~NS)
    // -- NRD-4 --
    add(0x5dac2c00, Nrd4~EW & L1Hrw~NS)
    add(0x5dae2c00, Nrd4~EW & L2Hrw~NS)
    // -- TLA-5 --
    add(0x5dac2d00, Tla5~WE & L1Hrw~NS)
    add(0x5dae2d00, Tla5~WE & L2Hrw~NS)
    // -- OWR-4 --
    add(0x5dac2e00, Owr4~WE & L1Hrw~NS)
    add(0x5dae2e00, Owr4~WE & L2Hrw~NS)
    add(0x5dac2e05, Owr4m~WE & L1Hrw~NS)
    add(0x5dae2e05, Owr4m~WE & L2Hrw~NS)
    // -- OWR-5 --
    add(0x5dac2f00, Owr5~WE & L1Hrw~NS)
    add(0x5dae2f00, Owr5~WE & L2Hrw~NS)
    // -- RD-4 --
    add(0x5dac3000, Rd4~WE & L1Hrw~NS)
    add(0x5dae3000, Rd4~WE & L2Hrw~NS)
    // -- RD-6 --
    add(0x5dac3100, Rd6~WE & L1Hrw~NS)
    add(0x5dae3100, Rd6~WE & L2Hrw~NS)
    // -- TLA-7 Shoulder / Ave6? -- 3200
    add(0x5dac3200, Ave6~WE & L1Hrw~NS)
    add(0x5dae3200, Ave6~WE & L2Hrw~NS)
    // -- TLA-7 Median --
    add(0x5dac3300, Tla7m~WE & L1Hrw~NS)
    add(0x5dae3300, Tla7m~WE & L2Hrw~NS)
    // -- TLA-9 Shoulder -- 3400
    // -- AVE-6 Median
    add(0x5dac3500, Ave6m~WE & L1Hrw~NS)
    add(0x5dae3500, Ave6m~WE & L2Hrw~NS)


    // HRW L0 Tiles
    // ----- OxD -----
    // -- Street --
    add(0x5dac4000, Street~ES & L1Hrw~NS)
    add(0x5dae4000, Street~ES & L2Hrw~NS)
    // -- Road --
    add(0x5dac4100, Road~ES & L1Hrw~NS)
    add(0x5dae4100, Road~ES & L2Hrw~NS)
    add(0x5dae4110, L1Road~ES & L2Hrw~NS)
    add(0x5dac410a, L2Road~ES & L1Hrw~NS)
    // -- OWR --
    add(0x5dac4200, Onewayroad~ES & L1Hrw~NS)
    add(0x5dae4200, Onewayroad~ES & L2Hrw~NS)
    // -- Avenue --
    add(0x5dac4300, Avenue~SW & L1Hrw~NS)
    add(0x5dac4305, Avenue~SharedDiagLeft & L1Hrw~NS)
    add(0x5dae4300, Avenue~SW & L2Hrw~NS)
    add(0x5dae4305, Avenue~SharedDiagLeft & L2Hrw~NS)
    // -- Rail --
    add(0x5dac4500, Rail~ES & L1Hrw~NS)
    add(0x5dae4500, Rail~ES & L2Hrw~NS)
    // -- STR --
    add(0x5dac4505, Str~ES & L1Hrw~NS)
    add(0x5dae4505, Str~ES & L2Hrw~NS)
    // -- GLR --
    add(0x5dac4700, Glr1~ES & L1Hrw~NS)
    add(0x5dac4705, Glr3~ES & L1Hrw~NS)
    add(0x5dac4800, Glr2~ES & L1Hrw~NS)
    add(0x5dac4805, Glr4~ES & L1Hrw~NS)
    add(0x5dae4700, Glr1~ES & L2Hrw~NS)
    add(0x5dae4705, Glr3~ES & L2Hrw~NS)
    add(0x5dae4800, Glr2~ES & L2Hrw~NS)
    add(0x5dae4805, Glr4~ES & L2Hrw~NS)
    // -- RHW-2 --
    add(0x5dac4a00, Dirtroad~ES & L1Hrw~NS)
    add(0x5dae4a00, Dirtroad~ES & L2Hrw~NS)
    // -- RHW-3 --
    add(0x5dac4b00, Rhw3~ES & L1Hrw~NS)
    add(0x5dac4b05, Rhw3~SE & L1Hrw~NS)
    add(0x5dae4b00, Rhw3~ES & L2Hrw~NS)
    add(0x5dae4b05, Rhw3~SE & L2Hrw~NS)
    // -- MIS --
    add(0x5dac4c00, Mis~ES & L1Hrw~NS)
    add(0x5dac4c05, Mis~SE & L1Hrw~NS)
    add(0x5dae4c00, Mis~ES & L2Hrw~NS)
    add(0x5dae4c05, Mis~SE & L2Hrw~NS)
    // -- RHW-4 --
    add(0x5dac4d00, Rhw4~ES & L1Hrw~NS)
    add(0x5dac4d05, Rhw4~SE & L1Hrw~NS)
    add(0x5dae4d00, Rhw4~ES & L2Hrw~NS)
    add(0x5dae4d05, Rhw4~SE & L2Hrw~NS)
    // -- RHW-6S --
    add(0x5dac4e00, Rhw6s~ES & L1Hrw~NS)
    add(0x5dac4e05, Rhw6s~SE & L1Hrw~NS)
    add(0x5dae4e00, Rhw6s~ES & L2Hrw~NS)
    add(0x5dae4e05, Rhw6s~SE & L2Hrw~NS)
    // -- RHW-8Sm --
    add(0x5dac4f00, Rhw8sm~ES & L1Hrw~NS)
    add(0x5dac4f05, Rhw8sm~SE & L1Hrw~NS)
    add(0x5dae4f00, Rhw8sm~ES & L2Hrw~NS)
    add(0x5dae4f05, Rhw8sm~SE & L2Hrw~NS)
    // -- RHW-8S --
    add(0x5dac5000, Rhw8s~ES & L1Hrw~NS)
    add(0x5dac5005, Rhw8s~SE & L1Hrw~NS)
    add(0x5dae5000, Rhw8s~ES & L2Hrw~NS)
    add(0x5dae5005, Rhw8s~SE & L2Hrw~NS)
    // -- RHW-10s --
    add(0x5dac5100, Rhw10s~ES & L1Hrw~NS)
    add(0x5dac5105, Rhw10s~SE & L1Hrw~NS)
    add(0x5dae5100, Rhw10s~ES & L2Hrw~NS)
    add(0x5dae5105, Rhw10s~SE & L2Hrw~NS)
     // -- RHW-6cm --
    add(0x5dac5300, Rhw6cm~ES & L1Hrw~NS)
    add(0x5dae5300, Rhw6cm~ES & L2Hrw~NS)
    // -- RHW-6c --
    add(0x5dac5400, Rhw6c~ES & L1Hrw~NS)
    add(0x5dac5405, Rhw6c~SE & L1Hrw~NS)
    add(0x5dae5400, Rhw6c~ES & L2Hrw~NS)
    add(0x5dae5405, Rhw6c~SE & L2Hrw~NS)
    // -- RHW-8c --
    add(0x5dac5500, Rhw8c~ES & L1Hrw~NS)
    add(0x5dac5505, Rhw8c~SE & L1Hrw~NS)
    add(0x5dae5500, Rhw8c~ES & L2Hrw~NS)
    add(0x5dae5505, Rhw8c~SE & L2Hrw~NS)
    // ...
    // -- TLA-3 --
    add(0x5dac5700, Tla3~ES & L1Hrw~NS)
    add(0x5dae5700, Tla3~ES & L2Hrw~NS)
    // -- AVE-2 --
    add(0x5dac5800, Ave2~ES & L1Hrw~NS)
    add(0x5dae5800, Ave2~ES & L2Hrw~NS)
    // -- ARD-3 --
    add(0x5dac5900, Ard3~ES & L1Hrw~NS)
    add(0x5dac5905, Ard3~SE & L1Hrw~NS)
    add(0x5dae5900, Ard3~ES & L2Hrw~NS)
    add(0x5dae5905, Ard3~SE & L2Hrw~NS)
    // -- OWR-1 --
    add(0x5dac5a00, Owr1~ES & L1Hrw~NS)
    add(0x5dae5a00, Owr1~ES & L2Hrw~NS)
    // -- OWR-3 --
    add(0x5dac5b00, Owr3~ES & L1Hrw~NS)
    add(0x5dae5b00, Owr3~ES & L2Hrw~NS)
    // -- NRD-4 --
    add(0x5dac5c00, Nrd4~ES & L1Hrw~NS)
    add(0x5dae5c00, Nrd4~ES & L2Hrw~NS)
    // -- TLA-5 --
    add(0x5dac5d00, Tla5~ES & L1Hrw~NS)
    add(0x5dac5d05, Tla5~SE & L1Hrw~NS)
    add(0x5dae5d00, Tla5~ES & L2Hrw~NS)
    add(0x5dae5d05, Tla5~SE & L2Hrw~NS)
    // -- OWR-4 --
    add(0x5dac5e00, Owr4~ES & L1Hrw~NS)
    add(0x5dac5e05, Owr4~SE & L1Hrw~NS)
    add(0x5dae5e00, Owr4~ES & L2Hrw~NS)
    add(0x5dae5e05, Owr4~SE & L2Hrw~NS)
    // -- OWR-5 --
    add(0x5dac5f00, Owr5~ES & L1Hrw~NS)
    add(0x5dac5f05, Owr5~SE & L1Hrw~NS)
    add(0x5dae5f00, Owr5~ES & L2Hrw~NS)
    add(0x5dae5f05, Owr5~SE & L2Hrw~NS)
    // -- RD-4 --
    add(0x5dac6000, Rd4~ES & L1Hrw~NS)
    add(0x5dac6005, Rd4~SE & L1Hrw~NS)
    add(0x5dae6000, Rd4~ES & L2Hrw~NS)
    add(0x5dae6005, Rd4~SE & L2Hrw~NS)
    // -- RD-6 --
    add(0x5dac6100, Rd6~ES & L1Hrw~NS)
    add(0x5dac6105, Rd6~SE & L1Hrw~NS)
    add(0x5dae6100, Rd6~ES & L2Hrw~NS)
    add(0x5dae6105, Rd6~SE & L2Hrw~NS)
    // ...
    // -- SAM-2 --
    add(0x5dac6a00, Sam2~ES & L1Hrw~NS)
    add(0x5dae6a00, Sam2~ES & L2Hrw~NS)
    // -- SAM-3 --
    add(0x5dac6a05, Sam3~ES & L1Hrw~NS)
    add(0x5dae6a05, Sam3~ES & L2Hrw~NS)
    // -- SAM-4 --
    add(0x5dac6a0a, Sam4~ES & L1Hrw~NS)
    add(0x5dae6a0a, Sam4~ES & L2Hrw~NS)
    // -- SAM-5 --
    add(0x5dac6b00, Sam5~ES & L1Hrw~NS)
    add(0x5dae6b00, Sam5~ES & L2Hrw~NS)
    // -- SAM-6 --
    add(0x5dac6b05, Sam6~ES & L1Hrw~NS)
    add(0x5dae6b05, Sam6~ES & L2Hrw~NS)
    // -- SAM-7 --
    add(0x5dac6b0a, Sam7~ES & L1Hrw~NS)
    add(0x5dae6b0a, Sam7~ES & L2Hrw~NS)
    // -- SAM-8 --
    add(0x5dac6c00, Sam8~ES & L1Hrw~NS)
    add(0x5dae6c00, Sam8~ES & L2Hrw~NS)
    // -- SAM-9 --
    add(0x5dac6c05, Sam9~ES & L1Hrw~NS)
    add(0x5dae6c05, Sam9~ES & L2Hrw~NS)
    // -- SAM-10 --
    add(0x5dac6c0a, Sam10~ES & L1Hrw~NS)
    add(0x5dae6c0a, Sam10~ES & L2Hrw~NS)
    // -- SAM-11 --
    add(0x5dac6d00, Sam11~ES & L1Hrw~NS)
    add(0x5dae6d00, Sam11~ES & L2Hrw~NS)

        // ----- DxO -----
   // -- Street --
    // add(???, Street~EW & Rail~ES)
    add(0x5dac7000, Street~EW & L1Hrw~ES)
    add(0x5dae7000, Street~EW & L2Hrw~ES)
    // -- Road --
    add(0x5dac7100, Road~EW & L1Hrw~ES)
    add(0x5dae7100, Road~EW & L2Hrw~ES)
    // -- OWR --
    add(0x5dac7200, Onewayroad~EW & L1Hrw~ES)
    add(0x5dae7200, Onewayroad~EW & L2Hrw~ES)
    // -- Avenue --
    add(0x5dac7300, Avenue~WE & L1Hrw~ES)
    add(0x5dae7300, Avenue~WE & L2Hrw~ES)
    add(0x5dac7305, Avenue~EW & L1Hrw~ES)
    add(0x5dae7305, Avenue~EW & L2Hrw~ES)
    // -- Rail --
    add(0x5dac7500, Rail~EW & L1Hrw~ES)
    add(0x5dae7500, Rail~EW & L2Hrw~ES)
    // -- STR --
    add(0x5d510100, Str~EW & Rail~NE)
    add(0x5dac7505, Str~EW & L1Hrw~ES)
    add(0x5dae7505, Str~EW & L2Hrw~ES)
    // -- GLR --
    add(0x5dac7700, Glr1~EW & L1Hrw~ES)
    add(0x5dac7705, Glr3~EW & L1Hrw~ES)
    add(0x5dac7800, Glr2~EW & L1Hrw~ES)
    add(0x5dac7805, Glr4~EW & L1Hrw~ES)
    add(0x5dae7700, Glr1~EW & L2Hrw~ES)
    add(0x5dae7705, Glr3~EW & L2Hrw~ES)
    add(0x5dae7800, Glr2~EW & L2Hrw~ES)
    add(0x5dae7805, Glr4~EW & L2Hrw~ES)
    // -- RHW-2 --
    add(0x5dac7a00, Dirtroad~WE & L1Hrw~ES)
    add(0x5dae7a00, Dirtroad~WE & L2Hrw~ES)
    // -- RHW-3 --
    add(0x5dac7b00, Rhw3~WE & L1Hrw~ES)
    add(0x5dac7b05, Rhw3~EW & L1Hrw~ES)
    add(0x5dae7b00, Rhw3~WE & L2Hrw~ES)
    add(0x5dae7b05, Rhw3~EW & L2Hrw~ES)
    // -- MIS --
    add(0x5dac7c00, Mis~WE & L1Hrw~ES)
    add(0x5dac7c05, Mis~EW & L1Hrw~ES)
    add(0x5dae7c00, Mis~WE & L2Hrw~ES)
    add(0x5dae7c05, Mis~EW & L2Hrw~ES)
    // -- RHW-4 --
    add(0x5dac7d00, Rhw4~WE & L1Hrw~ES)
    add(0x5dac7d05, Rhw4~EW & L1Hrw~ES)
    add(0x5dae7d00, Rhw4~WE & L2Hrw~ES)
    add(0x5dae7d05, Rhw4~EW & L2Hrw~ES)
    // -- RHW-6S --
    add(0x5dac7e00, Rhw6s~WE & L1Hrw~ES)
    add(0x5dac7e05, Rhw6s~EW & L1Hrw~ES)
    add(0x5dae7e00, Rhw6s~WE & L2Hrw~ES)
    add(0x5dae7e05, Rhw6s~EW & L2Hrw~ES)
    // -- RHW-8Sm --
    add(0x5dac7f00, Rhw8sm~WE & L1Hrw~ES)
    add(0x5dac7f05, Rhw8sm~EW & L1Hrw~ES)
    add(0x5dae7f00, Rhw8sm~WE & L2Hrw~ES)
    add(0x5dae7f05, Rhw8sm~EW & L2Hrw~ES)
    // -- RHW-8S --
    add(0x5dac8000, Rhw8s~WE & L1Hrw~ES)
    add(0x5dac8005, Rhw8s~EW & L1Hrw~ES)
    add(0x5dae8000, Rhw8s~WE & L2Hrw~ES)
    add(0x5dae8005, Rhw8s~EW & L2Hrw~ES)
    // -- RHW-10S --
    add(0x5dac8100, Rhw10s~WE & L1Hrw~ES)
    add(0x5dac8105, Rhw10s~EW & L1Hrw~ES)
    add(0x5dae8100, Rhw10s~WE & L2Hrw~ES)
    add(0x5dae8105, Rhw10s~EW & L2Hrw~ES)
    // -- RHW-6Cm --
    add(0x5dac8300, Rhw6cm~WE & L1Hrw~ES)
    add(0x5dae8300, Rhw6cm~WE & L2Hrw~ES)
    // -- RHW-6C --
    add(0x5dac8400, Rhw6c~WE & L1Hrw~ES)
    add(0x5dac8405, Rhw6c~EW & L1Hrw~ES)
    add(0x5dae8400, Rhw6c~WE & L2Hrw~ES)
    add(0x5dae8405, Rhw6c~EW & L2Hrw~ES)
    // -- RHW-8C --
    add(0x5dac8500, Rhw8c~WE & L1Hrw~ES)
    add(0x5dac8505, Rhw8c~EW & L1Hrw~ES)
    add(0x5dae8500, Rhw8c~WE & L2Hrw~ES)
    add(0x5dae8505, Rhw8c~EW & L2Hrw~ES)
    // -- RHW-3 --
    // ...
    // -- TLA-3 --
    add(0x5dac8700, Tla3~EW & L1Hrw~ES)
    add(0x5dae8700, Tla3~EW & L2Hrw~ES)
    // -- AVE-2 --
    add(0x5dac8800, Ave2~EW & L1Hrw~ES)
    add(0x5dae8800, Ave2~EW & L2Hrw~ES)
    // -- ARD-3 --
    add(0x5dac8900, Ard3~EW & L1Hrw~ES)
    add(0x5dac8905, Ard3~WE & L1Hrw~ES)
    add(0x5dae8900, Ard3~EW & L2Hrw~ES)
    add(0x5dae8905, Ard3~WE & L2Hrw~ES)
    // -- OWR-1 --
    add(0x5dac8a00, Owr1~EW & L1Hrw~ES)
    add(0x5dae8a00, Owr1~EW & L2Hrw~ES)
    // -- OWR-3 --
    add(0x5dac8b00, Owr3~EW & L1Hrw~ES)
    add(0x5dae8b00, Owr3~EW & L2Hrw~ES)
    // -- NRD-4 --
    add(0x5dac8c00, Nrd4~EW & L1Hrw~ES)
    add(0x5dae8c00, Nrd4~EW & L2Hrw~ES)
    // -- TLA-5 --
    add(0x5dac8d00, Tla5~EW & L1Hrw~ES)
    add(0x5dac8d05, Tla5~WE & L1Hrw~ES)
    add(0x5dae8d00, Tla5~EW & L2Hrw~ES)
    add(0x5dae8d05, Tla5~WE & L2Hrw~ES)
    // -- OWR-4 --
    add(0x5dac8e00, Owr4~EW & L1Hrw~ES)
    add(0x5dac8e05, Owr4~WE & L1Hrw~ES)
    add(0x5dae8e00, Owr4~EW & L2Hrw~ES)
    add(0x5dae8e05, Owr4~WE & L2Hrw~ES)
    // -- OWR-5 --
    add(0x5dac8f00, Owr5~EW & L1Hrw~ES)
    add(0x5dac8f05, Owr5~WE & L1Hrw~ES)
    add(0x5dae8f00, Owr5~EW & L2Hrw~ES)
    add(0x5dae8f05, Owr5~WE & L2Hrw~ES)
    // -- Rd-4 --
    add(0x5dac9000, Rd4~EW & L1Hrw~ES)
    add(0x5dac9005, Rd4~WE & L1Hrw~ES)
    add(0x5dae9000, Rd4~EW & L2Hrw~ES)
    add(0x5dae9005, Rd4~WE & L2Hrw~ES)
    // -- RD-6 --
    add(0x5dac9100, Rd6~EW & L1Hrw~ES)
    add(0x5dac9105, Rd6~WE & L1Hrw~ES)
    add(0x5dae9100, Rd6~EW & L2Hrw~ES)
    add(0x5dae9105, Rd6~WE & L2Hrw~ES)
    // ...
    // -- SAM-2 --
    add(0x5dac9a00, Sam2~EW & L1Hrw~ES)
    add(0x5dae9a00, Sam2~EW & L2Hrw~ES)
    // -- SAM-3 --
    add(0x5dac9a05, Sam3~EW & L1Hrw~ES)
    add(0x5dae9a05, Sam3~EW & L2Hrw~ES)
    // -- SAM-4 --
    add(0x5dac9a0a, Sam4~EW & L1Hrw~ES)
    add(0x5dae9a0a, Sam4~EW & L2Hrw~ES)
    // -- SAM-5 --
    add(0x5dac9b00, Sam5~EW & L1Hrw~ES)
    add(0x5dae9b00, Sam5~EW & L2Hrw~ES)
    // -- SAM-6 --
    add(0x5dac9b05, Sam6~EW & L1Hrw~ES)
    add(0x5dae9b05, Sam6~EW & L2Hrw~ES)
    // -- SAM-7 --
    add(0x5dac9b0a, Sam7~EW & L1Hrw~ES)
    add(0x5dae9b0a, Sam7~EW & L2Hrw~ES)
    // -- SAM-8 --
    add(0x5dac9c00, Sam8~EW & L1Hrw~ES)
    add(0x5dae9c00, Sam8~EW & L2Hrw~ES)
    // -- SAM-9 --
    add(0x5dac9c05, Sam9~EW & L1Hrw~ES)
    add(0x5dae9c05, Sam9~EW & L2Hrw~ES)
    // -- SAM-10 --
    add(0x5dac9c0a, Sam10~EW & L1Hrw~ES)
    add(0x5dae9c0a, Sam10~EW & L2Hrw~ES)
    // -- SAM-11 --
    add(0x5dac9d00, Sam11~EW & L1Hrw~ES)
    add(0x5dae9d00, Sam11~EW & L2Hrw~ES)

        // ----- DxD -----
    // -- Street --
    add(0x5daca000, Street~WS & L1Hrw~ES)
    add(0x5daea000, Street~WS & L2Hrw~ES)
    // -- Road --
    add(0x5daca100, Road~WS & L1Hrw~ES)
    add(0x5daea100, Road~WS & L2Hrw~ES)
    // -- OWR --
    add(0x5daca200, Onewayroad~WS & L1Hrw~ES)
    add(0x5daea200, Onewayroad~WS & L2Hrw~ES)
    // -- Avenue --
    add(0x5daca300, Avenue~SW & L1Hrw~ES)
    add(0x5daea300, Avenue~SW & L2Hrw~ES)
    add(0x5daca305, Avenue~SharedDiagLeft & L1Hrw~ES)
    add(0x5daea305, Avenue~SharedDiagLeft & L2Hrw~ES)
    // -- Rail --
    add(0x5daca500, Rail~WS & L1Hrw~ES)
    add(0x5daea500, Rail~WS & L2Hrw~ES)
    // -- STR --
    add(0x5d510300, Str~NE & Rail~ES)
    add(0x5daca505, Str~WS & L1Hrw~ES)
    add(0x5daea505, Str~WS & L2Hrw~ES)
    // -- GLR --
    add(0x5daca700, Glr1~WS & L1Hrw~ES)
    add(0x5daca705, Glr3~WS & L1Hrw~ES)
    add(0x5daca800, Glr2~WS & L1Hrw~ES)
    add(0x5daca805, Glr4~WS & L1Hrw~ES)
    add(0x5daea700, Glr1~WS & L2Hrw~ES)
    add(0x5daea705, Glr3~WS & L2Hrw~ES)
    add(0x5daea800, Glr2~WS & L2Hrw~ES)
    add(0x5daea805, Glr4~WS & L2Hrw~ES)
    // -- RHW-2 ---
    add(0x5dacaa00, Dirtroad~WS & L1Hrw~ES)
    add(0x5daeaa00, Dirtroad~WS & L2Hrw~ES)
    // -- RHW-3 ---
    add(0x5dacab00, Rhw3~WS & L1Hrw~ES)
    add(0x5dacab05, Rhw3~SW & L1Hrw~ES)
    add(0x5daeab00, Rhw3~WS & L2Hrw~ES)
    add(0x5daeab05, Rhw3~SW & L2Hrw~ES)
    // -- MIS ---
    add(0x5dacac00, Mis~WS & L1Hrw~ES)
    add(0x5dacac05, Mis~SW & L1Hrw~ES)
    add(0x5daeac00, Mis~WS & L2Hrw~ES)
    add(0x5daeac05, Mis~SW & L2Hrw~ES)
    // -- RHW-4 ---
    add(0x5dacad00, Rhw4~WS & L1Hrw~ES)
    add(0x5dacad05, Rhw4~SW & L1Hrw~ES)
    add(0x5daead00, Rhw4~WS & L2Hrw~ES)
    add(0x5daead05, Rhw4~SW & L2Hrw~ES)
    // -- RHW-6S --
    add(0x5dacae00, Rhw6s~WS & L1Hrw~ES)
    add(0x5dacae05, Rhw6s~SW & L1Hrw~ES)
    add(0x5daeae00, Rhw6s~WS & L2Hrw~ES)
    add(0x5daeae05, Rhw6s~SW & L2Hrw~ES)
    // -- RHW-8Sm --
    add(0x5dacaf00, Rhw8sm~WS & L1Hrw~ES)
    add(0x5dacaf05, Rhw8sm~SW & L1Hrw~ES)
    add(0x5daeaf00, Rhw8sm~WS & L2Hrw~ES)
    add(0x5daeaf05, Rhw8sm~SW & L2Hrw~ES)
    // -- RHW-8S --
    add(0x5dacb000, Rhw8s~WS & L1Hrw~ES)
    add(0x5dacb005, Rhw8s~SW & L1Hrw~ES)
    add(0x5daeb000, Rhw8s~WS & L2Hrw~ES)
    add(0x5daeb005, Rhw8s~SW & L2Hrw~ES)
    // -- RHW-10S --
    add(0x5dacb100, Rhw10s~WS & L1Hrw~ES)
    add(0x5dacb105, Rhw10s~SW & L1Hrw~ES)
    add(0x5daeb100, Rhw10s~WS & L2Hrw~ES)
    add(0x5daeb105, Rhw10s~SW & L2Hrw~ES)
    // -- RHW-6Cm ---
    add(0x5dacb300, Rhw6cm~WS & L1Hrw~ES)
    add(0x5daeb300, Rhw6cm~WS & L2Hrw~ES)
    // -- RHW-6C --
    add(0x5dacb400, Rhw6c~WS & L1Hrw~ES)
    add(0x5dacb405, Rhw6c~SW & L1Hrw~ES)
    add(0x5daeb400, Rhw6c~WS & L2Hrw~ES)
    add(0x5daeb405, Rhw6c~SW & L2Hrw~ES)
    // -- RHW-8C --
    add(0x5dacb500, Rhw8c~WS & L1Hrw~ES)
    add(0x5dacb505, Rhw8c~SW & L1Hrw~ES)
    add(0x5daeb500, Rhw8c~WS & L2Hrw~ES)
    add(0x5daeb505, Rhw8c~SW & L2Hrw~ES)
    // ...
    // -- TLA-3 --
    add(0x5dacb700, Tla3~WS & L1Hrw~ES)
    add(0x5daeb700, Tla3~WS & L2Hrw~ES)
    // -- AVE-2 --
    add(0x5dacb800, Ave2~WS & L1Hrw~ES)
    add(0x5daeb800, Ave2~WS & L2Hrw~ES)
    // -- ARD-3 --
    add(0x5dacb900, Ard3~WS & L1Hrw~ES)
    add(0x5dacb905, Ard3~SW & L1Hrw~ES)
    add(0x5daeb900, Ard3~WS & L2Hrw~ES)
    add(0x5daeb905, Ard3~SW & L2Hrw~ES)
    // -- OWR-1 --
    add(0x5dacba00, Owr1~WS & L1Hrw~ES)
    add(0x5daeba00, Owr1~WS & L2Hrw~ES)
    // -- OWR-3 --
    add(0x5dacbb00, Owr3~WS & L1Hrw~ES)
    add(0x5daebb00, Owr3~WS & L2Hrw~ES)
    // -- NRD-4 --
    add(0x5dacbc00, Nrd4~WS & L1Hrw~ES)
    add(0x5daebc00, Nrd4~WS & L2Hrw~ES)
    // -- TLA-5 --
    add(0x5dacbd00, Tla5~WS & L1Hrw~ES)
    add(0x5dacbd05, Tla5~SW & L1Hrw~ES)
    add(0x5daebd00, Tla5~WS & L2Hrw~ES)
    add(0x5daebd05, Tla5~SW & L2Hrw~ES)
    // -- OWR-4 --
    add(0x5dacbe00, Owr4~WS & L1Hrw~ES)
    add(0x5dacbe05, Owr4~SW & L1Hrw~ES)
    add(0x5daebe00, Owr4~WS & L2Hrw~ES)
    add(0x5daebe05, Owr4~SW & L2Hrw~ES)
    // -- OWR-5 --
    add(0x5dacbf00, Owr5~WS & L1Hrw~ES)
    add(0x5dacbf05, Owr5~SW & L1Hrw~ES)
    add(0x5daebf00, Owr5~WS & L2Hrw~ES)
    add(0x5daebf05, Owr5~SW & L2Hrw~ES)
    // -- RD-4 --
    add(0x5dacc000, Rd4~WS & L1Hrw~ES)
    add(0x5dacc005, Rd4~SW & L1Hrw~ES)
    add(0x5daec000, Rd4~WS & L2Hrw~ES)
    add(0x5daec005, Rd4~SW & L2Hrw~ES)
    // -- RD-6 --
    add(0x5dacc100, Rd6~WS & L1Hrw~ES)
    add(0x5dacc105, Rd6~SW & L1Hrw~ES)
    add(0x5daec100, Rd6~WS & L2Hrw~ES)
    add(0x5daec105, Rd6~SW & L2Hrw~ES)
    // ...
    // -- SAM-2 --
    add(0x5dacca00, Sam2~WS & L1Hrw~ES)
    add(0x5daeca00, Sam2~WS & L2Hrw~ES)
    // -- SAM-3 --
    add(0x5dacca05, Sam3~WS & L1Hrw~ES)
    add(0x5daeca05, Sam3~WS & L2Hrw~ES)
    // -- SAM-4 --
    add(0x5dacca0a, Sam4~WS & L1Hrw~ES)
    add(0x5daeca0a, Sam4~WS & L2Hrw~ES)
    // -- SAM-5 --
    add(0x5daccb00, Sam5~WS & L1Hrw~ES)
    add(0x5daecb00, Sam5~WS & L2Hrw~ES)
    // -- SAM-6 --
    add(0x5daccb05, Sam6~WS & L1Hrw~ES)
    add(0x5daecb05, Sam6~WS & L2Hrw~ES)
    // -- SAM-7 --
    add(0x5daccb0a, Sam7~WS & L1Hrw~ES)
    add(0x5daecb0a, Sam7~WS & L2Hrw~ES)
    // -- SAM-8 --
    add(0x5daccc00, Sam8~WS & L1Hrw~ES)
    add(0x5daecc00, Sam8~WS & L2Hrw~ES)
    // -- SAM-9 --
    add(0x5daccc05, Sam9~WS & L1Hrw~ES)
    add(0x5daecc05, Sam9~WS & L2Hrw~ES)
    // -- SAM-10 --
    add(0x5daccc0a, Sam10~WS & L1Hrw~ES)
    add(0x5daecc0a, Sam10~WS & L2Hrw~ES)
    // -- SAM-11 --
    add(0x5daccd00, Sam11~WS & L1Hrw~ES)
    add(0x5daecd00, Sam11~WS & L2Hrw~ES) 

    builder.result()
  }

  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)

  def apply(tile: Tile): IdTile = tileMap(tile)

}
