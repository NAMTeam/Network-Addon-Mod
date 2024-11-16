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

      // base
      Rules += main~WE | (base ~> main)~WE        // ortho continue
      Rules += main~WE | base~CW | % | main~WE    // ortho continue stub convert

      Rules += main~ES | (base ~> main)~NW        // diag continue
      Rules += main~ES | base~CNW | % | main~NW   // diag stub convert

      // height transitions
      Rules += Rail~CW & main~CE | (base ~> main)~WE      // orth OST continue to viaduct
      Rules += Rail~CW & main~CE | base~CW | % | main~WE  // orth OST continue to viaduct stub convert
      Rules += Rail~CW & main~CE | base~CE | % | main~WE  // orth OST continue to viaduct stub convert (jump)
      Rules += Rail~CE & main~CW | base~CE | % | base~WE  // orth OST continue to ground stub convert (jump)
    
      Rules += Rail~CW & main~WE | (base ~> main)~WE      // orth ramp HT continue
      Rules += Rail~CW & main~WE | base~CW | % | main~WE  // orth ramp HT continue stub convert

      Rules += Rail~(0,0,0,993) & main~(0,0,1,993) | (base ~> main)~NW  // diag OST continue
      Rules += Rail~(0,0,0,983) & main~(0,0,1,983) | (base ~> main)~NW  // diag ramp HT continue
      
      // helper/overhang tiles
      Rules += main~(42,0,2,0) | (base ~> main)~WE               // OxO helper continue
      Rules += main~(42,0,2,0) | base~CW | % | main~WE           // OxO helper continue stub convert
      Rules += main~(72,0,2,8) | (base ~> main)~WE               // OxD helper continue
      Rules += main~(72,0,2,8) | base~CW | % | main~WE           // OxD helper continue stub convert
      Rules += main~(0,41,43,0) | main~WS | % | main~(43,0,0,1)  // DxO helper tile two
      
      // defines adjacent ortho height transitions for the main network in WE orientation
      val adjOrthHTs = main match {
        case L1Dtr => List(Rail~CW & L1Dtr~CE, Rail~CW & L1Dtr~WE)
        case L2Dtr => List(Rail~CW & L2Dtr~CE, L1Dtr~CW & L2Dtr~CE, Rail~CW & L2Dtr~WE, L1Dtr~CW & L2Dtr~WE)
      }
      // defines adjacent diag height transitions for the main network in ES orientation
      val adjDiagHTs = main match {
        case L1Dtr => List(Rail~(0,0,0,993) & L1Dtr~(0,0,1,993), Rail~(0,0,0,983) & L1Dtr~(0,0,1,983))
        case L2Dtr => List(Rail~(0,0,0,993) & L2Dtr~(0,0,1,993), Rail~(0,0,0,983) & L2Dtr~(0,0,1,983))
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

            for (orthStart <- orthStarts) {
              Rules += orthStart | (base ~> main)~WE & minor~SN       // OxO start
              // TODO: check if the orthStart is OST before creating the following rule
              Rules += orthStart | minor~SN | % | main~WE & minor~SN  // OxO start (jump)
            }

            // the continue rules differ on overhang side
            Rules += main~WE & minor~NS | (base ~> main)~WE              // OxO continue
            Rules += main~WE & minor~NS | base~CW | % | main~WE          // OxO continue stub conversion
            Rules += main~WE & minor~SN | base~WE | % | main~(42,0,2,0)  // OxO continue to helper
            Rules += main~WE & minor~SN | base~CW | % | main~(42,0,2,0)  // OxO continue to helper stub conversion

          } else {
              for (orthStart <- orthStarts) {
                Rules += orthStart | (base ~> main)~WE & minor~NS~SN          // OxO start
                // TODO: check if the orthStart is the OST before creating the following rule
                // this is only necessary for single tile networks flanked by OSTs
                Rules += orthStart | minor~NS~SN | % | main~WE & minor~NS~SN  // OxO start (jump)
                }
                Rules += main~WE & minor~NS~SN | (base ~> main)~WE      // OxO continue
                Rules += main~WE & minor~NS~SN | base~CW | % | main~WE  // OxO continue stub conversion
          }

          // OxD Rules

          if (overhangsRight(minor)) {

            Rules ++= stabilize(main~WE | base~WE & minor~ES | main~(2,8,72,0) | main~WE & minor~ES)  // OxD start overhang double-swap

            for (orthStart <- orthStarts) {
              Rules += orthStart | (base ~> main)~WE & minor~SE  // OxD start
            }

            Rules += main~WE & minor~WN | base~WE | % | main~(72,0,2,8)  // OxD continue to helper
            Rules += main~WE & minor~WN | base~CW | % | main~(72,0,2,8)  // OxD continue to helper stub conversion
            Rules += main~WE & minor~NW | (base ~> main)~WE              // OxD continue
            Rules += main~WE & minor~NW | base~CW | % | main~WE          // OxD continue stub conversion

          } else {

            for (orthStart <- orthStarts) {
              Rules += orthStart | (base ~> main)~WE & minor~ES  // OxD start
              Rules += orthStart | (base ~> main)~WE & minor~SE  // OxD start
            }

            Rules += main~WE & minor~WN | (base ~> main)~WE       // OxD continue
            Rules += main~WE & minor~WN | base~CW | % | main~WE   // OxD continue stub conversion
            Rules += main~WE & minor~NW | (base ~> main)~WE       // OxD continue
            Rules += main~WE & minor~NW | base~CW | % | main~WE   // OxD continue stub conversion
          }

          // OxD across rules needed in all cases
          Rules += main~WE & minor~ES | (base ~> main)~WE & minor~NW  // OxD across
          Rules += main~WE & minor~SE | (base ~> main)~WE & minor~WN  // OxD across

          if (minor == Street || minor == Road) {
            val crossbucks = minor match {
              case Street => IdTile(0x5f502a00, R2F0, noSymmetries)
              case Road   => IdTile(0x03020600, R2F0, noSymmetries)
            }
            Rules += main~NS & minor~ES | crossbucks | % | minor~NW  // remove crossbucks tile
          }

          // DxO Rules
          /*
          'jump' rules required to handle case of single tile network flanked by two OSTs
          */

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
              Rules += main~ES & minor~NS | (base ~> main)~NW  // DxO continue

          } else {
              for (diagStart <- diagStarts) {
                Rules += diagStart | (base ~> main)~NW & minor~NS       // DxO start
                Rules += diagStart | minor~NS | % | main~NW & minor~NS  // DxO start (jump)
              }   
              Rules += main~ES & minor~NS | (base ~> main)~NW  // DxO continue
          }
          // DxO across rules needed in all cases
          Rules += main~EN & minor~WE | (base ~> main)~SW & minor~WE       // DxO across
          Rules += main~EN & minor~WE | minor~WE | % | main~SW & minor~WE  // DxO across (jump) 

          // DxD Rules

          for (diagStart <- diagStarts) {
            Rules += diagStart | (base ~> main)~NW & minor~EN  // DxD start
          }
          Rules += main~EN & minor~ES | (base ~> main)~SW & minor~NW  // DxD across
          Rules += main~SE & minor~WS | (base ~> main)~NW             // DxD continue
        }

        if (minor.typ == AvenueLike) {

          // OxO
          for (orthStart <- orthStarts) {
            Rules += orthStart | (base ~> main)~WE & minor~NS  // OxO start
          }
          Rules += main~WE & minor~NS | (base ~> main)~WE & minor~SN  // OxO across
          Rules += main~WE & minor~SN | (base ~> main)~WE             // OxO continue
          Rules += main~WE & minor~SN | base~CW | % | main~WE         // OxO continue stub conversion

          // OxD
          for (orthStart <- orthStarts) {
            Rules += orthStart | (base ~> main)~WE & minor~ES  // OxD start
          }
          Rules += main~WE & minor~ES | (base ~> main)~WE & minor~SharedDiagRight  // OxD middle
          Rules += main~WE & minor~SharedDiagRight | (base ~> main)~WE & minor~WN  // OxD end
          Rules += main~WE & minor~WN | (base ~> main)~WE                          // OxD continue
          Rules += main~WE & minor~WN | base~CW | % | main~WE                      // OxD continue stub conversion

          // DxO
          for (diagStart <- diagStarts) {
            Rules += diagStart | (base ~> main)~NW & minor~NS  // DxO start
          }
          Rules += main~EN & minor~EW | (base ~> main)~SW & minor~EW  // DxO middle 1
          Rules += main~ES & minor~NS | (base ~> main)~NW & minor~SN  // DxO middle 2
          Rules += main~EN & minor~WE | (base ~> main)~SW & minor~WE  // DxO end
          Rules += main~ES & minor~SN | (base ~> main)~NW             // DxO continue

          // DxD
          for (diagStart <- diagStarts) {
            Rules += diagStart | (base ~> main)~NW & minor~NE  // DxD start
          }
          Rules += main~EN & minor~ES | (base ~> main)~SW & minor~SharedDiagRight // DxD middle
          Rules += main~ES & minor~SharedDiagLeft | (base ~> main)~NW & minor~SW  // DxD end
          Rules += main~ES & minor~SW | (base ~> main)~NW                         // DxD continue
        }
      }
    }
    createRules()
  }
}