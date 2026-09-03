package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, Flags._, RotFlip._, Implicits._, group.SymGroup._
import scala.collection.mutable.Buffer
import NetworkProperties._

class HybridRailwayRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Adjacencies with Stability {

  def start(): Unit = {

      val HrwNetworks = List(L1Hrw, L2Hrw) 

      val BaseNetworks = List(Street, Road, Onewayroad, Dirtroad) 

      val CrossNetworks1 = List(
      Street, Road, Onewayroad,
      Dirtroad, Rhw3, Mis, Rhw4, Rhw6s,
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

      val ViaductNetworks = List (L1Road, L1Onewayroad, L2Road, L2Onewayroad, L1Mis, L1Rhw4, L1Rhw6s, L1Rhw8s,
      L1Rhw8sm, L1Rhw10s, L1Rhw12s, L1Rhw6c, L1Rhw6cm, L1Rhw8c, L2Mis, L2Rhw4, L2Rhw6s, L2Rhw8s,
      L2Rhw8sm, L2Rhw10s, L2Rhw12s, L2Rhw6c, L2Rhw6cm, L2Rhw8c, L3Mis, L3Rhw4, L3Rhw6s, L4Mis, L4Rhw4, L4Rhw6s,
      )
      val DiagshareNetworks = List (Avenue)
      val ViaductDiagshare = List (L1Avenue, L2Avenue)
      //val Overhang = List(
      //Hrw6oxoL1, Hrw6oxoL2, Hrw6oxdL1, Hrw6oxdL2,
      //Hrw6dxoL1, Hrw6dxoL2, Hrw6dxdL1, Hrw6dxdL2,
      //)
      for ( base <- BaseNetworks ) {
      for ( main <- HrwNetworks ) { 
      for ( over <- Override){
      // L1 Hrw
      Rules += L1Hrw~WE | (Hrw ~> L1Hrw)~WE        // orth continue
      Rules += L1Hrw~WE | Hrw~CW | % | L1Hrw~WE    // orth stub convert
      Rules += L1Hrw~ES | (Hrw ~> L1Hrw)~NW        // diag continue
      Rules += L1Hrw~ES | Hrw~CNW | % | L1Hrw~NW   // diag stub convert
      // L2 Hrw
      Rules += L2Hrw~WE | (Hrw ~> L2Hrw)~WE        // orth continue
      Rules += L2Hrw~WE | Hrw~CW | % | L2Hrw~WE    // orth stub convert
      Rules += L2Hrw~ES | (Hrw ~> L2Hrw)~NW        // diag continue
      Rules += L2Hrw~ES | Hrw~CNW | % | L2Hrw~NW   // diag stub convert
      //for ( over <- Overhang ) {
      //Rules ++= stabilize(Rhw6s~WE & L1Hrw~NS | Hrw~EW | % | over)
      //Rules ++= stabilize(Rhw6s~WE & L1Hrw~NS | L1Hrw~EW | % | over)
      //}
        for ( minor <- CrossNetworks2  ) {
        Rules ++= stabilize(main~EW & minor~NS | Hrw~EW | % | main~WE) //OxO
        Rules ++= stabilize(main~WE & minor~SN | Hrw~EW | % | main~WE) //OxO Rotate
        Rules ++= stabilize(main~EW & minor~SN | Hrw~EW | % | main~WE) //OxO Flip
        }
        for ( minor <- CrossNetworks1  ) {
        Rules ++= stabilize(main~SE & minor~NS | Hrw~NW | % | main~NW) //OxD
        Rules ++= stabilize(main~EW & minor~SE | Hrw~EW | % | main~EW) //DxO
        Rules ++= stabilize(main~NE & minor~SE | Hrw~SW | % | main~SW) //DxD

        Rules ++= stabilize(main~ES & minor~SN | Hrw~NW | % | main~NW) //OxD Rotate
        Rules ++= stabilize(main~WE & minor~ES | Hrw~EW | % | main~EW) //DxO Rotate
        Rules ++= stabilize(main~EN & minor~ES | Hrw~SW | % | main~SW) //DxD Rotate

        Rules ++= stabilize(main~SE & minor~SN | Hrw~NW | % | main~NW) //OxD Flip
        Rules ++= stabilize(main~EW & minor~ES | Hrw~EW | % | main~EW) //DxO Flip
        Rules ++= stabilize(main~NE & minor~ES | Hrw~SW | % | main~SW) //DxD Flip
        /////
        for ( minor <- RoadNetworks ) 
        Rules ++= stabilize(main~NS & minor~EW | Street~(0,0,0,0) | % | minor~WE) //OxO stub
        Rules ++= stabilize(main~NS & minor~SW | Street~(0,0,0,0) | % | minor~NW) //OxD stub
        Rules ++= stabilize(main~SE & minor~EW | Street~(0,0,0,0) | % | minor~WE) //DxO stub
        Rules ++= stabilize(main~SE & minor~NE | Street~(0,0,0,0) | % | minor~SW) //DxD stub

        for ( minor <- OwrNetworks ) 
        Rules ++= stabilize(main~NS & minor~EW | Street~(0,0,0,0) | % | minor~WE) //OxO stub
        Rules ++= stabilize(main~NS & minor~SW | Street~(0,0,0,0) | % | minor~NW) //OxD stub
        Rules ++= stabilize(main~SE & minor~EW | Street~(0,0,0,0) | % | minor~WE) //DxO stub
        Rules ++= stabilize(main~SE & minor~NE | Street~(0,0,0,0) | % | minor~SW) //DxD stub

        for ( minor <- RhwNetworks ) 
        Rules ++= stabilize(main~NS & minor~EW | Dirtroad~(0,0,0,0) | % | minor~WE) //OxO stub
        Rules ++= stabilize(main~NS & minor~SW | Dirtroad~(0,0,0,0) | % | minor~NW) //OxD stub
        Rules ++= stabilize(main~SE & minor~EW | Dirtroad~(0,0,0,0) | % | minor~WE) //DxO stub
        Rules ++= stabilize(main~SE & minor~NE | Dirtroad~(0,0,0,0) | % | minor~SW) //DxD stub

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
        Rules ++= stabilize( main~NS | minor~(0,0,0,1) | minor~WN & main~NS | minor~WS ) //DxO
        Rules ++= stabilize( main~SW | minor~EC | minor~WE & main~SW | minor~EW )         //OxD
        Rules ++= stabilize( main~NW | minor~(0,0,0,-1) | minor~NE & main~NW | minor~SW ) //DxD

        Rules ++= stabilize( main~NS | minor~(0,0,0,-1) | minor~NW & main~NS | minor~SW ) //DxO
        Rules ++= stabilize( main~NW | minor~EC | minor~WE & main~NW | minor~EW )         //OxD
        Rules ++= stabilize( main~NW | minor~(0,0,0,1) | minor~EN & main~NW | minor~WS ) //DxD

          for ( main <- BaseNetworks) {
          Rules ++= stabilize(main~CW | Hrw~NS | main~EW  | Hrw~NS ) //Viaduct OxO Stub
          Rules ++= stabilize(main~CW | Hrw~NE | main~EW  | Hrw~NE ) //Viaduct OxD Stub
          Rules ++= stabilize(main~(0,0,0,3) | Hrw~NS | main~ES  | Hrw~NS ) //Viaduct DxO Stub
          Rules ++= stabilize(main~(0,0,0,3) | Hrw~NE | main~ES  | Hrw~NE ) //Viaduct DxD Stub  
          }      
          for ( main <- ViaductNetworks){
          Rules ++= stabilize(main~EW | Hrw~NS | % | main~EW & Hrw~NS ) //Viaduct OxO
          Rules ++= stabilize(main~EW | Hrw~NE | % | main~EW & Hrw~NE ) //Viaduct OxD
          Rules ++= stabilize(main~ES | Hrw~NS | % | main~NW & Hrw~NS ) //Viaduct DxO
          Rules ++= stabilize(main~ES | Hrw~NE | % | main~NW & Hrw~NE ) //Viaduct DxD
                  
          Rules ++= stabilize(main~WE | Hrw~SN | % | main~WE & Hrw~SN ) //Viaduct OxO
          Rules ++= stabilize(main~WE | Hrw~EN | % | main~WE & Hrw~EN ) //Viaduct OxD
          Rules ++= stabilize(main~SE | Hrw~SN | % | main~WN & Hrw~SN ) //Viaduct DxO
          Rules ++= stabilize(main~SE | Hrw~EN | % | main~WN & Hrw~EN ) //Viaduct DxD
          }
        }
      }
    }
  }
  createRules()
  }
}