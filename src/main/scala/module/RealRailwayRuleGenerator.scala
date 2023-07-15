package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, Flags._, RotFlip._, Implicits._, group.SymGroup._
import scala.collection.mutable.Buffer
import NetworkProperties._


class RealRailwayRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Adjacencies with Stability {

  private def overhangsRight(n: Network): Boolean = {
    /*
    Determines whether a network should require an extra tile
    on the right side of crossings due overhang.
    */
    Set[Network](Rhw6s).contains(n)
  } 

  def start(): Unit = {
    /*
    Generate OxO rules by iteration over list of supported crossings
    */
    val RrwNetworks = List(L1Dtr, L2Dtr)

    val CrossNetworks = List(
      Street, Road, Onewayroad, Avenue,
      Rail, Str, 
      Glr1, Glr2, Glr3, Glr4, 
      Dirtroad, Rhw3, Mis, Rhw4, Rhw6s,
      Tla3, Ave2, Ard3, Owr1, Owr3, Nrd4,
      Sam2, Sam3, Sam4, Sam5, Sam6, Sam7, Sam8, Sam9, Sam10, Sam11,
      )

    for (main <- RrwNetworks; base <- main.base) {

      Rules += main~WE | (base ~> main)~WE            // ortho continue
      Rules += main~WE | base~CW | % | main~WE        // ortho continue stub convert
      // Rules += main~WE | base~CE | % | main~WE        // ortho continue stub convert (jump)
      // Rules += main~WE | base~(0,0,0,0) | % | main~WE // ortho continue blank stub convert

      Rules += main~ES | (base ~> main)~NW      // diag continue
      Rules += main~ES | base~CNW | % | main~NW // diag stub convert
      // Rules += main~ES | base~NWC | % | main~NW // diag stub convert (jump)

      Rules += Rail~CW & main~CE | (base ~> main)~WE // Orth OST Adj
      Rules += Rail~CW & main~CE | base~CW | % | main~WE
      Rules += Rail~CW & main~CE | base~CE | % | main~WE 
      // Rules += Rail~CW & main~CE | base~(0,0,0,0) | % | main~WE
      Rules += Rail~CE & main~CW | base~CE | % | base~WE

      Rules += Rail~CW & main~WE | (base ~> main)~WE // Orth Ramp HT
      Rules += Rail~CW & main~WE | base~CW | % | main~WE
      // Rules += Rail~CW & main~WE | base~CE | % | main~WE 
      // Rules += Rail~CW & main~WE | base~(0,0,0,0) | % | main~WE

      Rules += Rail~(0,0,0,13) & main~(0,0,1,13) | (base ~> main)~NW      // diag OST
      Rules += Rail~(0,0,0,13) & main~(0,0,1,13) | base~CNW | % | main~NW // diag OST stub conversion
      Rules += Rail~(0,0,0,13) & main~(0,0,1,13) | base~NWC | % | main~NW // diag OST stub conversion (jump)

      Rules += Rail~(0,0,0,3) & main~(0,0,1,3) | (base ~> main)~NW        // diag Ramp HT
      Rules += Rail~(0,0,0,3) & main~(0,0,1,3) | base~CNW | % | main~NW   // diag Ramp HT stub conversion
      // Rules += Rail~(0,0,0,3) & main~(0,0,1,3) | base~NWC | % | main~NW   // diag Ramp HT stub conversion (jump)
      
      // helper/overhang tiles rules
      Rules += main~(42,0,2,0) | (base ~> main)~WE                // OxO helper continue
      Rules += main~(42,0,2,0) | base~CE | % | main~WE            // OxO helper continue stub
      Rules += main~(42,0,2,0) | base~CW | % | main~WE            // OxO helper continue stub (jump)
      Rules += main~(72,0,2,8) | (base ~> main)~WE                // OxD helper continue
      Rules += main~(72,0,2,8) | base~CE | % | main~WE            // OxD helper continue stub
      Rules += main~(72,0,2,8) | base~CW | % | main~WE            // OxD helper continue stub (jump)
      Rules += main~(0,41,43,0) | main~WS | % | main~(43,0,0,1)   // DxO helper tile two
      
      // defines adjacent ortho height transitions for the main network in WE orientation
      val adjOrthHTs = main match {
        case L1Dtr => List(Rail~CW & L1Dtr~CE, Rail~CW & L1Dtr~WE)
        case L2Dtr => List(Rail~CW & L2Dtr~CE, L1Dtr~CW & L2Dtr~CE, Rail~CW & L2Dtr~WE, L1Dtr~CW & L2Dtr~WE)
      }
      // defines adjacent diag height transitions for the main network in ES orientation
      val adjDiagHTs = main match {
        case L1Dtr => List(Rail~(0,0,0,13) & L1Dtr~(0,0,1,13), Rail~(0,0,0,3) & L1Dtr~(0,0,1,3))
        case L2Dtr => List(Rail~(0,0,0,13) & L2Dtr~(0,0,1,13), Rail~(0,0,0,3) & L2Dtr~(0,0,1,3))
      }
      // defines valid left side start tiles for orth & diag rules
      val orthStarts = Tile(main~WE) +: adjOrthHTs
      val diagStarts = Tile(main~ES) +: adjDiagHTs

      for (minor <- CrossNetworks if minor.height != main.height) {
        /*
        Cases:
        1.) 1-tile
        2.) Avenue-like
        3.) Dual-tile Asymmetrical e.g. RHW-8S
        4.) Triple-tile
        */

        if (isSingleTile(minor)) {

          // OxO Rules

          if (overhangsRight(minor)) {
            // if minor overhangs, then on the overhanging side of the rule:
            //    only the base ortho ERRW is a valid start, and
            //    the start rule is a double-swap with the overhang helper
            Rules ++= stabilize(main~WE | base~WE & minor~NS | main~(2,0,42,0) | main~WE & minor~NS) // OxO start overhang double-swap
            Rules ++= stabilize(main~WE | minor~NS | main~(2,0,42,0) | main~WE & minor~NS )          // OxO start overhang double-swap (jump)

            for (orthStart <- orthStarts) {
              Rules += orthStart | (base ~> main)~WE & minor~SN         // OxO start
              Rules += orthStart | minor~SN | % | main~WE & minor~SN    // OxO start (jump)
            }

            // the continue rules differ on overhang side
            Rules += main~WE & minor~NS | (base ~> main)~WE          // OxO continue
            Rules += main~WE & minor~NS | base~CE | % | main~WE      // OxO continue stub conversion
            Rules += main~WE & minor~NS | base~CW | % | main~WE      // OxO continue stub conversion (jump)
            Rules += main~WE & minor~SN | base~WE | % | main~(42,0,2,0)   // OxO continue to helper
            Rules += main~WE & minor~SN | base~CE | % | main~(42,0,2,0)   // OxO continue to helper stub conversion
            Rules += main~WE & minor~SN | base~CW | % | main~(42,0,2,0)   // OxO continue to helper stub conversion (jump)

          } else {
              for (orthStart <- orthStarts) {
                Rules += orthStart | (base ~> main)~WE & minor~NS~SN          // OxO start
                Rules += orthStart | minor~NS~SN | % | main~WE & minor~NS~SN  // OxO start (jump)
                }
                Rules += main~WE & minor~NS~SN | (base ~> main)~WE          // OxO continue
                Rules += main~WE & minor~NS~SN | base~CE | % | main~WE      // OxO continue stub conversion
                Rules += main~WE & minor~NS~SN | base~CW | % | main~WE      // OxO continue stub conversion (jump)
          }

          // OxD Rules

          if (overhangsRight(minor)) {

            Rules ++= stabilize(main~WE | base~WE & minor~ES | main~(2,8,72,0) | main~WE & minor~ES)  // OxD start overhang double-swap
            Rules ++= stabilize(main~WE | minor~ES | main~(2,8,72,0) | main~WE & minor~ES)            // OxD start overhang double-swap (jump)

            for (orthStart <- orthStarts) {
              Rules += orthStart | (base ~> main)~WE & minor~SE       // OxD start
              Rules += orthStart | minor~SE | % | main~WE & minor~SE  // OxD start (jump)
            }

            Rules += main~WE & minor~WN | base~WE | % | main~(72,0,2,8) // OxD continue to helper
            Rules += main~WE & minor~WN | base~CE | % | main~(72,0,2,8) // OxD continue to helper stub conversion
            Rules += main~WE & minor~WN | base~CW | % | main~(72,0,2,8) // OxD continue to helper stub conversion (jump)
            Rules += main~WE & minor~NW | (base ~> main)~WE       // OxD continue
            Rules += main~WE & minor~NW | base~CE | % | main~WE   // OxD continue stub conversion
            Rules += main~WE & minor~NW | base~CW | % | main~WE   // OxD continue stub conversion (jump)

          } else {

            for (orthStart <- orthStarts) {
              Rules += orthStart | (base ~> main)~WE & minor~ES       // OxD start
              Rules += orthStart | minor~ES | % | main~WE & minor~ES  // OxD start (jump)
              Rules += orthStart | (base ~> main)~WE & minor~SE       // OxD start
              Rules += orthStart | minor~SE | % | main~WE & minor~SE  // OxD start (jump)
            }

            Rules += main~WE & minor~WN | (base ~> main)~WE       // OxD continue
            Rules += main~WE & minor~WN | base~CE | % | main~WE   // OxD continue stub conversion
            Rules += main~WE & minor~WN | base~CW | % | main~WE   // OxD continue stub conversion (jump)
            Rules += main~WE & minor~NW | (base ~> main)~WE       // OxD continue
            Rules += main~WE & minor~NW | base~CE | % | main~WE   // OxD continue stub conversion
            Rules += main~WE & minor~NW | base~CW | % | main~WE   // OxD continue stub conversion (jump)
          }

          // OxD across rules needed in all cases
          Rules += main~WE & minor~ES | (base ~> main)~WE & minor~NW        // OxD across
          Rules += main~WE & minor~ES | minor~NW | % | main~WE & minor~NW   // OxD across (jump)
          Rules += main~WE & minor~SE | (base ~> main)~WE & minor~WN        // OxD across
          Rules += main~WE & minor~SE | minor~WN | % | main~WE & minor~WN   // OxD across (jump)

          if (minor == Street || minor == Road) {
            val crossbucks = minor match {
              case Street => IdTile(0x5f502a00, R2F0, noSymmetries)
              case Road   => IdTile(0x03020600, R2F0, noSymmetries)
            }
            Rules += main~NS & minor~ES | crossbucks | % | minor~NW // remove crossbucks tile
          }

          // DxO Rules

          if (overhangsRight(minor)) {
              // e.g. RHW-6S
              // will require two extra tiles on the overhang side
              // can only initiate with main~ES
              Rules ++= stabilize(main~ES | base~NW & minor~NS | main~(41,43,0,0) | main~NW & minor~NS)  // DxO start overhang double-swap
              Rules ++= stabilize(main~ES | minor~NS | main~(41,43,0,0) | main~NW & minor~NS)            // DxO start overhang double-swap (jump)
              // non-overhanging side starts are standard, use all starts
              for (diagStart <- diagStarts) {
                Rules += diagStart | (base ~> main)~NW & minor~SN       // DxO start
                Rules += diagStart | minor~SN | % | main~NW & minor~SN  // DxO start (jump)
              } 
              // continue rules on non-overhang side
              Rules += main~ES & minor~NS | (base ~> main)~NW       // DxO continue
              Rules += main~ES & minor~NS | base~WNC | % | main~NW  // DxO continue stub conversion
              Rules += main~ES & minor~NS | base~NWC | % | main~NW  // DxO continue stub conversion (jump)
          } else {
              for (diagStart <- diagStarts) {
                Rules += diagStart | (base ~> main)~NW & minor~NS       // DxO start
                Rules += diagStart | minor~NS | % | main~NW & minor~NS  // DxO start (jump)
              }   
              Rules += main~ES & minor~NS | (base ~> main)~NW       // DxO continue
              Rules += main~ES & minor~NS | base~WNC | % | main~NW  // DxO continue stub conversion
              Rules += main~ES & minor~NS | base~NWC | % | main~NW  // DxO continue stub conversion (jump)
          }
          // DxO across rules needed in all cases
          Rules += main~EN & minor~WE | (base ~> main)~SW & minor~WE        // DxO across
          Rules += main~EN & minor~WE | minor~WE | % | main~SW & minor~WE   // DxO across (jump) 

          // DxD Rules

          for (diagStart <- diagStarts) {
            Rules += diagStart | (base ~> main)~NW & minor~EN       // DxD start
            Rules += diagStart | minor~EN | % | main~NW & minor~EN  // DxD start (jump)
          }
          Rules += main~EN & minor~ES | (base ~> main)~SW & minor~NW        // DxD across
          Rules += main~EN & minor~ES | minor~NW | % | main~SW & minor~NW   // DxD across (jump)
          Rules += main~SE & minor~WS | (base ~> main)~NW       // DxD continue
          Rules += main~SE & minor~WS | base~WNC | % | main~NW  // DxD continue stub conversion
          Rules += main~SE & minor~WS | base~NWC | % | main~NW  // DxD continue stub conversion (jump)
        }

        if (minor.typ == AvenueLike) {

          // OxO
          for (orthStart <- orthStarts) {
            Rules += orthStart | (base ~> main)~WE & minor~NS             // OxO start
            Rules += orthStart | minor~NS | % | main~WE & minor~NS        // OxO start (jump)
          }
          Rules += main~WE & minor~NS | (base ~> main)~WE & minor~SN      // OxO across
          Rules += main~WE & minor~NS | minor~SN | % | main~WE & minor~SN // OxO across (jump)
          Rules += main~WE & minor~SN | (base ~> main)~WE             // OxO continue
          Rules += main~WE & minor~SN | base~CE | % | main~WE         // OxO continue stub conversion
          Rules += main~WE & minor~SN | base~CW | % | main~WE         // OxO continue stub conversion (jump)

          // OxD
          for (orthStart <- orthStarts) {
            Rules += orthStart | (base ~> main)~WE & minor~ES             // OxD start
            Rules += orthStart | minor~ES | % | main~WE & minor~ES        // OxD start (jump)
          }
          Rules += main~WE & minor~ES | (base ~> main)~WE & minor~SharedDiagRight                   // OxD middle
          Rules += main~WE & minor~ES | minor~SharedDiagRight | % | main~WE & minor~SharedDiagRight // OxD middle (jump)
          Rules += main~WE & minor~SharedDiagRight | (base ~> main)~WE & minor~WN       // OxD end
          Rules += main~WE & minor~SharedDiagRight | minor~WN | % | main~WE & minor~WN  // OxD end (jump)
          Rules += main~WE & minor~WN | (base ~> main)~WE             // OxD continue
          Rules += main~WE & minor~WN | base~CE | % | main~WE         // OxD continue stub conversion
          Rules += main~WE & minor~WN | base~CW | % | main~WE         // OxD continue stub conversion (jump)

          // DxO
          for (diagStart <- diagStarts) {
            Rules += diagStart | (base ~> main)~NW & minor~NS                 // DxO start
            Rules += diagStart | minor~NS | % | main~NW & minor~NS            // DxO start (jump)
          }
          Rules += main~EN & minor~EW | (base ~> main)~SW & minor~EW      // DxO middle 1
          Rules += main~EN & minor~EW | minor~EW | % | main~SW & minor~EW // DxO middle 1 (jump)
          Rules += main~ES & minor~NS | (base ~> main)~NW & minor~SN      // DxO middle 2
          Rules += main~ES & minor~NS | minor~SN | % | main~NW & minor~SN // DxO middle 2 (jump)
          Rules += main~EN & minor~WE | (base ~> main)~SW & minor~WE      // DxO end
          Rules += main~EN & minor~WE | minor~WE | % | main~SW & minor~WE // DxO end (jump)
          Rules += main~ES & minor~SN | (base ~> main)~NW                 // DxO continue
          Rules += main~ES & minor~SN | base~WNC | % | main~NW            // DxO continue stub conversion
          Rules += main~ES & minor~SN | base~NWC | % | main~NW            // DxO continue stub conversion (jump)

          // DxD
          for (diagStart <- diagStarts) {
            Rules += diagStart | (base ~> main)~NW & minor~NE         // DxD start
            Rules += diagStart | minor~NE | % | main~NW & minor~NE    // DxD start (jump)
          }
          Rules += main~EN & minor~ES | (base ~> main)~SW & minor~SharedDiagRight                   // DxD middle
          Rules += main~EN & minor~ES | minor~SharedDiagRight | % | main~SW & minor~SharedDiagRight // DxD middle (jump)
          Rules += main~ES & minor~SharedDiagLeft | (base ~> main)~NW & minor~SW                    // DxD end
          Rules += main~ES & minor~SharedDiagLeft | minor~SW | % | main~NW & minor~SW               // DxD end (jump)
          Rules += main~ES & minor~SW | (base ~> main)~NW           // DxD continue
          Rules += main~ES & minor~SW | base~WNC | % | main~NW      // DxD continue stub conversion
          Rules += main~ES & minor~SW | base~NWC | % | main~NW      // DxD continue stub conversion (jump)
        }
      }
    }
    createRules()
  }
}