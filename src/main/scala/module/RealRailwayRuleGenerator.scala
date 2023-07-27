package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, Flags._, RotFlip._, Implicits._
import scala.collection.mutable.Buffer
import NetworkProperties._


class RealRailwayRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Adjacencies {

  def start(): Unit = {
    /*
    Generate OxO rules by iteration over list of supported crossings
    */
    val RrwNetworks = List(L1Dtr, L2Dtr)
    /*
    val CrossNetworks = List(Road, L1Road, L2Road, Avenue, L1Avenue, L2Avenue, Onewayroad, L1Onewayroad, L2Onewayroad,
    Rail, L1Dtr, L2Dtr, Glr1, Glr2, Glr3, Glr4, Hsr, Dirtroad, Rhw3, Mis, Rhw4, Rhw6s, Rhw8sm, Rhw8s, Rhw10s, Rhw6cm,
    Rhw6c, Rhw8c, Tla3, Ave2, Ard3, Owr1, Owr3, Nrd4, Tla5, Owr4, Owr5, Rd4, Rd6, Ave6, Tla7m, Ave6m,
    Sam2, Sam3, Sam4, Sam5, Sam6, Sam7, Sam8, Sam9, Sam10, Sam11)
    val CrossNetworks = List(Road, L1Road, L2Road, Avenue, L1Avenue, L2Avenue, Onewayroad, L1Onewayroad, L2Onewayroad,
    Rail, L1Dtr, L2Dtr, Dirtroad, Rhw3, Mis, Rhw4, Rhw6s, Rhw8sm, Rhw8s, Rhw10s, Rhw6cm,
    Rhw6c, Rhw8c)
    */
    /*
    val CrossNetworks = List(Street, Road, Avenue, Onewayroad, Rail, 
    Dirtroad, Rhw3, Mis, Rhw4, Rhw6s, Rhw8sm, Rhw8s, Rhw10s, Rhw12s, Rhw6cm, Rhw6c, Rhw8c,
    Tla3, Ave2, Ard3, Owr1, Owr3, Nrd4, Tla5, Owr4, Owr5, Rd4, Rd6, Ave6, Tla7m, Ave6m,
    Sam2, Sam3, Sam4, Sam5, Sam6, Sam7, Sam8, Sam9, Sam10, Sam11)
    */

    val CrossNetworks = List(Street, Road, Avenue, Onewayroad, Rail)


    for (main <- RrwNetworks; base <- main.base) {

      Rules += main~WE | (base ~> main)~WE      // ortho
      Rules += main~WE | base~CW | % | main~WE  // overrides end stub to orth ERRW
      Rules += main~WE | base~CE | % | main~WE  // overrides end stub to orth ERRW
      Rules += main~WE | base~(0,0,0,0) | % | main~WE // overides stub to orth ERRW

      Rules += Rail~CW & main~CE | (base ~> main)~WE // Orth OST Adj
      Rules += Rail~CW & main~CE | base~CW | % | main~WE
      Rules += Rail~CW & main~CE | base~CE | % | main~WE 
      Rules += Rail~CW & main~CE | base~(0,0,0,0) | % | main~WE 
      Rules += Rail~CW & main~WE | (base ~> main)~WE // Orth Ramp HT
      Rules += Rail~CW & main~WE | base~CW | % | main~WE
      Rules += Rail~CW & main~WE | base~CE | % | main~WE 
      Rules += Rail~CW & main~WE | base~(0,0,0,0) | % | main~WE

      /*
      for (minor <- CrossNetworks) {
        createAdjacentIntersections(main, base, minor)
      }
      */

      for (minor <- CrossNetworks if minor.height != main.height) {
        /*
        Cases:
        1.) 1-tile
        2.) Avenue-like
        3.) Dual-tile Asymmetrical e.g. RHW-8S
        4.) Triple-tile
        */

        if (isSingleTile(minor)) {
          // OxO
          Rules += main~WE | (base ~> main)~WE & minor~NS~SN          // OxO
          Rules += main~WE | minor~NS~SN | % | main~WE & minor~NS~SN  // OxO jump
          Rules += main~WE & minor~NS~SN | (base ~> main)~WE          // OxO continue
          Rules += main~WE & minor~NS~SN | base~CW | % | main~WE      // OxO continue stub conversion (jump)
          Rules += main~WE & minor~NS~SN | base~CE | % | main~WE      // OxO continue stub conversion
          // OxD (to do: consider asymmetrical)
          Rules += main~WE | (base ~> main)~WE & minor~ES       // OxD
          Rules += main~WE | minor~ES | % | main~WE & minor~ES  // OxD jump
          Rules += main~WE & minor~WN | (base ~> main)~WE       // OxD continue
          Rules += main~WE & minor~WN | base~CW | % | main~WE   // OxD continue stub conversion (jump)
          Rules += main~WE & minor~WN | base~CE | % | main~WE   // OxD continue stub conversion
          // DxO
          Rules += main~ES | (base ~> main)~NW & minor~NS       // DxO
          Rules += main~ES | minor~NS | % | main~NW & minor~NS  // DxO jump
          Rules += main~ES & minor~NS | (base ~> main)~NW       // DxO continue
          // DxD
          Rules += main~ES | (base ~> main)~NW & minor~EN       // DxD
          Rules += main~SE | minor~WS | % | main~NW & minor~EN  // DxD jump
          Rules += main~SE & minor~WS | (base ~> main)~NW       // DxD continue
        }

        /*
        if (hasRightShoulder(minor)) {
          Rules += main~WE | (base ~> main)~WE & minor~NS       // OxO
          Rules += main~WE | minor~NS | % | main~WE & minor~NS  // OxO jump
          // orth continue
          Rules += main~WE & minor~SN | (base ~> main)~WE       // OxO continue
          Rules += main~WE & minor~SN | base~CW | % | main~WE   // OxO continue stub conversion 
          Rules += main~WE & minor~SN | base~CE | % | main~WE   // OxO continue stub conversion
          // orth OST/HT
          Rules += Rail~CW & main~CE | (base ~> main)~WE & minor~NS       // Orth OST Adj
          Rules += Rail~CW & main~CE | minor~NS | % | main~WE & minor~NS  // Orth OST no-int
          Rules += Rail~CW & main~WE | (base ~> main)~WE & minor~NS       // Orth Ramp HT
          Rules += Rail~CW & main~WE | minor~NS | % | main~WE & minor~NS  // Orth Ramp no-int
        }
        if (hasLeftShoulder(minor)) {
          Rules += main~WE | (base ~> main)~WE & minor~SN       // OxO
          Rules += main~WE | minor~SN | % | main~WE & minor~SN  // OxO jump
          // orth continue
          Rules += main~WE & minor~NS | (base ~> main)~WE       // OxO continue
          Rules += main~WE & minor~NS | base~CW | % | main~WE   // OxO continue stub conversion 
          Rules += main~WE & minor~NS | base~CE | % | main~WE   // OxO continue stub conversion
          // orth OST/HT
          Rules += Rail~CW & main~CE | (base ~> main)~WE & minor~SN // Orth OST Adj
          Rules += Rail~CW & main~CE | minor~SN | % | main~WE & minor~SN // Orth OST no-int
          Rules += Rail~CW & main~WE | (base ~> main)~WE & minor~SN // Orth Ramp HT
          Rules += Rail~CW & main~WE | minor~SN | % | main~WE & minor~SN // Orth Ramp no-int
        }
        if(minor.typ == AvenueLike) {
          Rules += main~WE & minor~NS | (base ~> main)~WE & minor~SN // OxO double
          Rules += main~WE & minor~NS | minor~SN | % | main~WE & minor~SN // OxO double no-int
        }
        //Rules += main~WE & minor~SN | (base ~> main)~WE       // OxO continue
        //Rules += main~WE & minor~SN | base~CW | % | main~WE   // OxO continue stub conversion 
        //Rules += main~WE & minor~SN | base~CE | % | main~WE   // OxO continue stub conversion
        
        for(minor2 <- CrossNetworks if minor2.height != main.height && minor2 != main) {
          if (hasRightShoulder(minor2)) {
          Rules += main~WE & minor~SN | (base ~> main)~WE & minor2~NS       // OxO | OxO adj
          Rules += main~WE & minor~SN | minor2~NS | % | main~WE & minor2~NS // OxO | OxO adj no-int
          }
        }


        Rules += main~WE | (base ~> main)~WE & minor~SE // OxD
        Rules += main~WE | minor~SE | % | main~WE & minor~SE // OxD no-int
        Rules += main~EW & minor~SW | (base ~> main)~WE // OxD continue
        for(minor2 <- CrossNetworks if minor2.height != main.height) {
          Rules += main~WE & minor~WN | (base ~> main)~WE & minor2~NS // OxD | OxO adjacencies
        }
        // Height transition OxD adjacency
        Rules += Rail~CW & main~CE | (base ~> main)~WE & minor~SE // Orth OST Adj
        Rules += Rail~CW & main~CE | minor~SE | % | main~WE & minor~SE// Orth OST no-int
        Rules += Rail~CW & main~WE | (base ~> main)~WE & minor~SE // Orth Ramp HT
        Rules += Rail~CW & main~WE | minor~SE | % | main~WE & minor~SE // Orth Ramp no-int

       //Rules += main~NE | (base ~> main)~NE & minor~WE // DxO
       */
      }
    }
    createRules()
  }
}