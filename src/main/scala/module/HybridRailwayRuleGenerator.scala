package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, Flags._, RotFlip._, Implicits._, group.SymGroup._
import scala.collection.mutable.Buffer
import NetworkProperties._

class HybridRailwayRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Adjacencies with Stability {

  def start(): Unit = {

      val HrwNetworks = List(L1Hrw, L2Hrw) 

      val BaseNetworks = List(Street, Road, Onewayroad, Dirtroad, Rhw3, Mis, Rhw4) 

      val CrossNetworks1 = List(
      Street, Road, Onewayroad,
      Dirtroad, Rhw3, Mis, Rhw4, Rhw6s, Rhw6c, Rhw6cm, Rhw8c, Rhw8s, Rhw8sm, Rhw10s,
      Tla3, Ave2, Ard3, Owr1, Owr3, Nrd4,
      ) 
      val CrossNetworks2 = List(
      Street, Road, Onewayroad, Avenue,
      Rhw3, Mis, Rhw4, Rhw6s, Rhw8s, Rhw8sm, Rhw10s, Rhw6c, Rhw6cm, Rhw8c,
      Tla3, Ave2, Ard3, Owr1, Owr3, Nrd4, Tla5, Rd4, Rd6, Ave6, Tla7m, Ave6m,
      )
      val Override = List(
      Rhw3, Mis, Rhw4, Rhw6s, Rhw8s, Rhw8sm, Rhw10s, Rhw6c, Rhw6cm, Rhw8c,
      Tla3, Ave2, Ard3, Owr1, Owr3, Nrd4,
      ) 
      val RoadNetworks = List( Road, Tla3, Ave2, Ard3, Nrd4, Tla5, Rd4, Rd6, Ave6, Tla7m, Ave6m)
      val OwrNetworks = List ( Onewayroad,  Owr1, Owr3)
      val RhwNetworks = List ( Dirtroad, Rhw3, Mis, Rhw4, Rhw6s, Rhw8s, Rhw8sm, Rhw10s, Rhw6c, Rhw6cm, Rhw8c)

      val DiagshareNetworks = List (Avenue)
      val ViaductDiagshare = List (L1Avenue, L2Avenue)

      //val Overhang = List(
      //Hrw6oxoL1, Hrw6oxoL2, Hrw6oxdL1, Hrw6oxdL2,
      //Hrw6dxoL1, Hrw6dxoL2, Hrw6dxdL1, Hrw6dxdL2,
      //)
      for ( base <- BaseNetworks ) {
      for ( main <- HrwNetworks ) { 
      for ( over <- Override){
      // L0 Hrw
      Rules += Hrw~WE | (Rail ~> Hrw)~WE
      Rules += Hrw~WE | (Rail ~> Hrw)~EW         // orth continue
      //Rules += Hrw~WE | Rail~CW | % | Hrw~WE
      //Rules += Hrw~WE | Rail~WC | % | Hrw~WE
      Rules += Hrw~WE | (Rail~EW & Road~NS) | % | Hrw~WE
      Rules += Hrw~EW | (Rail~EW & Avenue~NS) | % | Hrw~WE    // orth stub convert
      //Rules += Hrw~ES | (Rail ~> Hrw)~NW        // diag continue
      //Rules += Hrw~ES | Rail~CNW | % | Hrw~NW   // diag stub convert
      // L1 Hrw
      Rules += L1Hrw~WE | (Hrw ~> L1Hrw)~WE
      Rules += L1Hrw~WE | (Hrw ~> L1Hrw)~EW         // orth continue
      Rules += L1Hrw~WE | Hrw~CW | % | L1Hrw~WE
      Rules += L1Hrw~WE | Hrw~WC | % | L1Hrw~WE
      Rules += L1Hrw~WE | (Hrw~EW & Road~NS) | % | L1Hrw~WE
      Rules += L1Hrw~EW | (Hrw~EW & Avenue~NS) | % | L1Hrw~WE     // orth stub convert
      Rules += L1Hrw~ES | (Hrw ~> L1Hrw)~NW        // diag continue
      Rules += L1Hrw~ES | Hrw~CNW | % | L1Hrw~NW   // diag stub convert
      // L2 Hrw
      Rules += L2Hrw~WE | (Hrw ~> L2Hrw)~WE  
      Rules += L2Hrw~WE | (Hrw ~> L2Hrw)~EW       // orth continue
      Rules += L2Hrw~WE | Hrw~CW | % | L2Hrw~WE  
      Rules += L2Hrw~WE | Hrw~WC | % | L2Hrw~WE    // orth stub convert
      Rules += L2Hrw~WE | (Hrw~EW & Road~NS) | % | L2Hrw~WE
      Rules += L2Hrw~EW | (Hrw~EW & Avenue~NS) | % | L2Hrw~WE  
      Rules += L2Hrw~ES | (Hrw ~> L2Hrw)~NW        // diag continue
      Rules += L2Hrw~ES | Hrw~CNW | % | L2Hrw~NW   // diag stub convert
      //for ( over <- Overhang ) {
      Rules += L1Hrw~WE | Rail~CW & Monorail~CE | % | L1Hrw~WE 
      Rules += L2Hrw~WE | Rail~CW & Monorail~CE | % | L2Hrw~WE
      Rules += L1Hrw~WE | Rail~WC & Monorail~EC | % | L1Hrw~WE        
      Rules += L2Hrw~WE | Rail~WC & Monorail~EC | % | L2Hrw~WE
      Rules += L1Hrw~WE | Rail~WC & Monorail~CE | % | L1Hrw~WE 
      Rules += L2Hrw~WE | Rail~WC & Monorail~CE | % | L2Hrw~WE
      Rules += L1Hrw~WE | Rail~EC & Monorail~WC | % | L1Hrw~WE        
      Rules += L2Hrw~WE | Rail~EC & Monorail~WC | % | L2Hrw~WE

      for (minor <- BaseNetworks ) {
      //OxO Crossing overrides 
      Rules ++= stabilize (Hrw~WE | minor~SN & Rail~WE  | % | Hrw~WE)
      //OxD Crossing overrides Edge
      Rules ++= stabilize (Hrw~WE | minor~SE & Rail~WE  | % | Hrw~WE)
      //OxD Crossing overrides Middle
      Rules ++= stabilize (Hrw~WE | minor~NW & Rail~WE  | % | Hrw~WE)
      //Flip Edge
      Rules ++= stabilize (Hrw~WE | minor~ES & Rail~WE  | % | Hrw~WE)
      //Flip Middle
      Rules ++= stabilize (Hrw~WE | minor~WN & Rail~WE  | % | Hrw~WE)
      //DxO Crossing overrides Edge
      Rules ++= stabilize (Hrw~SE | minor~NS & Rail~NW  | % | Hrw~NW)
      //DxO Crossing overrides Middle
      Rules ++= stabilize (Hrw~SE | minor~WE & Rail~NW  | % | Hrw~NW)
      //Flip Edge
      Rules ++= stabilize (Hrw~SE | minor~SN & Rail~NW  | % | Hrw~NW)
      //Flip Middle
      Rules ++= stabilize (Hrw~SE | minor~EW & Rail~NW  | % | Hrw~NW)
      //DxD Crossing overrides Edge
      Rules ++= stabilize (Hrw~SE | minor~NE & Rail~NW  | % | Hrw~NW)
      //Flip Edge
      Rules ++= stabilize (Hrw~SE | minor~EN & Rail~NW  | % | Hrw~NW)
      //DxD Crossing overrides Middle
      Rules ++= stabilize (Hrw~SE | minor~SW & Rail~NW  | % | Hrw~NW)
      //Flip Middle
      Rules ++= stabilize (Hrw~SE | minor~WS & Rail~NW  | % | Hrw~NW)
      //Rules ++= stabilize(Rhw6s~WE & L1Hrw~NS | Hrw~EW | % | over)
      //Rules ++= stabilize(Rhw6s~WE & L1Hrw~NS | L1Hrw~EW | % | over)
      //}
      }
      for ( Base <- HrwNetworks ) 
      for ( minor <- BaseNetworks ) {
       //OxO Crossing overrides 
      Rules ++= stabilize (Base~WE | minor~NS & Rail~WE  | % | Base~WE)
      //OxD Crossing overrides Edge
      Rules ++= stabilize (Base~WE | minor~SE & Rail~WE  | % | Base~WE)
      //OxD Crossing overrides Middle
      Rules ++= stabilize (Base~WE | minor~NW & Rail~WE  | % | Base~WE)
      //Flip Edge
      Rules ++= stabilize (Base~WE | minor~ES & Rail~WE  | % | Base~WE)
      //Flip Middle
      Rules ++= stabilize (Base~WE | minor~WN & Rail~WE  | % | Base~WE)
      //DxO Crossing overrides Edge
      Rules ++= stabilize (Base~SE | minor~NS & Rail~NW  | % | Base~NW)
      //DxO Crossing overrides Middle
      Rules ++= stabilize (Base~SE | minor~WE & Rail~NW  | % | Base~NW)
      //Flip Edge
      Rules ++= stabilize (Base~SE | minor~SN & Rail~NW  | % | Base~NW)
      //Flip Middle
      Rules ++= stabilize (Base~SE | minor~EW & Rail~NW  | % | Base~NW)
      //DxD Crossing overrides Edge
      Rules ++= stabilize (Base~SE | minor~NE & Rail~NW  | % | Base~NW)
      //Flip Edge
      Rules ++= stabilize (Base~SE | minor~EN & Rail~NW  | % | Base~NW)
      //DxD Crossing overrides Middle
      Rules ++= stabilize (Base~SE | minor~SW & Rail~NW  | % | Base~NW)
      //Flip Middle
      Rules ++= stabilize (Base~SE | minor~WS & Rail~NW  | % | Base~NW)
      }
        for ( minor <- CrossNetworks2  ) {
        Rules ++= stabilize(main~EW & minor~NS | Hrw~EW | % | main~WE) //OxO
        Rules ++= stabilize(main~WE & minor~SN | Hrw~EW | % | main~WE) //OxO Rotate
        Rules ++= stabilize(main~EW & minor~SN | Hrw~EW | % | main~WE) //OxO Flip
        }
        for ( minor <- CrossNetworks1  ) {
        Rules ++= stabilize(main~SE & minor~NS | Hrw~NW | % | main~NW)            //OxD
        Rules ++= stabilize(main~SE & minor~EW | Hrw~NW | % | main~WN & minor~EW) 
        Rules ++= stabilize(main~EW & minor~NE | Hrw~EW | % | main~EW & minor~WS) //DxO
        Rules ++= stabilize(main~NE & minor~NW | Hrw~SW | % | main~SW)            //DxD

        Rules ++= stabilize(main~ES & minor~SN | Hrw~NW | % | main~NW)            //OxD Rotate
        Rules ++= stabilize(main~ES & minor~WE | Hrw~NW | % | main~WN & minor~WE) 
        Rules ++= stabilize(main~WE & minor~EN | Hrw~EW | % | main~EW & minor~SW) //DxO Rotate
        Rules ++= stabilize(main~EN & minor~WN | Hrw~SW | % | main~SW)            //DxD Rotate

        Rules ++= stabilize(main~SE & minor~SN | Hrw~NW | % | main~NW)             //OxD Flip
        Rules ++= stabilize(main~SE & minor~WE | Hrw~NW | % | main~WN & minor~WE )             
        Rules ++= stabilize(main~EW & minor~EN | Hrw~EW | % | main~EW & minor~WS ) //DxO Flip
        Rules ++= stabilize(main~NE & minor~WN | Hrw~SW | % | main~SW)             //DxD Flip
        /////
        for ( minor <- RoadNetworks ) 
        Rules ++= stabilize(main~NS & minor~EW | Street~(0,0,0,0) | % | minor~WE) //OxO stub
        Rules ++= stabilize(main~NS & minor~WS | Street~(0,0,0,0) | % | minor~WN) //OxD stub
        Rules ++= stabilize(main~SW & minor~EW | Street~(0,0,0,0) | % | minor~WE) //DxO stub
        Rules ++= stabilize(main~NW & minor~SW | Street~(0,0,0,0) | % | minor~SW) //DxD stub

        for ( minor <- OwrNetworks ) 
        Rules ++= stabilize(main~NS & minor~EW | Street~(0,0,0,0) | % | minor~WE) //OxO stub
        Rules ++= stabilize(main~NS & minor~WS | Street~(0,0,0,0) | % | minor~WN) //OxD stub
        Rules ++= stabilize(main~SW & minor~EW | Street~(0,0,0,0) | % | minor~WE) //DxO stub
        Rules ++= stabilize(main~NW & minor~SW | Street~(0,0,0,0) | % | minor~SW) //DxD stub

        for ( minor <- RhwNetworks ) 
        Rules ++= stabilize(main~NS & minor~EW | Dirtroad~(0,0,0,0) | % | minor~WE) //OxO stub
        Rules ++= stabilize(main~NS & minor~WS | Dirtroad~(0,0,0,0) | % | minor~WN) //OxD stub
        Rules ++= stabilize(main~SW & minor~EW | Dirtroad~(0,0,0,0) | % | minor~WE) //DxO stub
        Rules ++= stabilize(main~NW & minor~SW | Dirtroad~(0,0,0,0) | % | minor~SW) //DxD stub

        }
        for ( minor <- CrossNetworks2 ) {
        Rules ++= stabilize(main~NS | minor~EC | main~NS & minor~WE | minor~EW) //OxO
        Rules ++= stabilize(main~NS | minor~EW | main~NS & minor~WE | minor~EW) //OxO
        }
        //for ( minor <- DiagshareNetworks ) {
        // // Rules += main~WE & minor~ES | (base ~> main)~WE & minor~SharedDiagRight
        //Rules ++= stabilize( main~NS | minor~SharedDiagRight | minor~SharedDiagLeft & main~NS | minor~SharedDiagRight ) //DxO
        //Rules ++= stabilize( main~NW | minor~SharedDiagRight | minor~SharedDiagLeft & main~NW | minor~SharedDiagRight ) //DxD
        //}
        //for ( minor <- ViaductDiagshare ) {
        //// Rules += main~WE & minor~ES | (base ~> main)~WE & minor~SharedDiagRight
        //Rules ++= stabilize( Hrw~NS | minor~SharedDiagRight | minor~SharedDiagLeft & Hrw~NS | minor~SharedDiagRight ) //DxO
        //Rules ++= stabilize( Hrw~NW | minor~SharedDiagRight | minor~SharedDiagLeft & Hrw~NW | minor~SharedDiagRight ) //DxD
        //
        //}
        for ( minor <- CrossNetworks1 ) { // if (minor.typ == Symmetrical) 
        Rules ++= stabilize( main~NS | minor~(0,0,0,1) | minor~WN & main~NS | minor~WS )  //DxO
        Rules ++= stabilize( main~SW | minor~EC | minor~WE & main~SW | minor~EW )         //OxD
        Rules ++= stabilize( main~NW | minor~(0,0,0,-1) | minor~NE & main~NW | minor~SW ) //DxD

        Rules ++= stabilize( main~NS | minor~(0,0,0,-1) | minor~NW & main~NS | minor~SW ) //DxO
        Rules ++= stabilize( main~NW | minor~EC | minor~WE & main~NW | minor~EW )         //OxD
        Rules ++= stabilize( main~NW | minor~(0,0,0,1) | minor~EN & main~NW | minor~WS )  //DxD
        }
      }
    }
  }
  createRules()
 }
}