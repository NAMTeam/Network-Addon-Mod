package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, Flags._, RotFlip._, Implicits._, group.SymGroup._
import scala.collection.mutable.Buffer
import NetworkProperties._

class HybridRailwayViaductRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Adjacencies with Stability {

  def start(): Unit = {

      val Rd = List(Road)
      val Ord = List(Onewayroad)
      val Ave = List(Avenue)
      val Drd = List(Dirtroad)

      val HrwNetworks = List(Hrw) 

      val BaseNetworks = List(Road, Onewayroad, Dirtroad) 

      val ErdNetworks = List (L1Road,L2Road)
      
      val EordNetworks = List (L1Onewayroad, L2Onewayroad)
      
      val EaveNetworks = List (L1Avenue, L2Avenue)

      val ErhwNetworks = List (L1Mis, L1Rhw4, L1Rhw6s, L1Rhw8s,
      L1Rhw8sm, L1Rhw10s, L1Rhw12s, L1Rhw6c, L1Rhw6cm, L1Rhw8c, L2Mis, L2Rhw4, L2Rhw6s, L2Rhw8s,
      L2Rhw8sm, L2Rhw10s, L2Rhw12s, L2Rhw6c, L2Rhw6cm, L2Rhw8c, L3Mis, L3Rhw4, L3Rhw6s, L4Mis, L4Rhw4, L4Rhw6s,
      )

      val ViaductNetworks1 = List (L1Road, L1Onewayroad, L2Road, L2Onewayroad, L1Avenue, L2Avenue, L1Mis, L1Rhw4, L1Rhw6s, L1Rhw8s,
      L1Rhw8sm, L1Rhw10s, L1Rhw12s, L1Rhw6c, L1Rhw6cm, L1Rhw8c, L2Mis, L2Rhw4, L2Rhw6s, L2Rhw8s,
      L2Rhw8sm, L2Rhw10s, L2Rhw12s, L2Rhw6c, L2Rhw6cm, L2Rhw8c, L3Mis, L3Rhw4, L3Rhw6s, L4Mis, L4Rhw4, L4Rhw6s,
      )
      val ViaductNetworks2 = List (L1Road, L1Onewayroad, L2Road, L2Onewayroad, L1Mis, L1Rhw4, L1Rhw6s, L1Rhw8s,
      L1Rhw8sm, L1Rhw10s, L1Rhw12s, L1Rhw6c, L1Rhw6cm, L1Rhw8c, L2Mis, L2Rhw4, L2Rhw6s, L2Rhw8s,
      L2Rhw8sm, L2Rhw10s, L2Rhw12s, L2Rhw6c, L2Rhw6cm, L2Rhw8c, L3Mis, L3Rhw4, L3Rhw6s, L4Mis, L4Rhw4, L4Rhw6s,
      )
          for ( rail <- HrwNetworks) 
          for ( minor <- Rd) {
          for ( main <- ErdNetworks){
          Rules ++= stabilize(main~EW & rail~NS | minor~EW | % | main~EW ) //Viaduct OxO
          Rules ++= stabilize(main~EW & rail~SW | minor~EW | % | main~EW ) //Viaduct OxD
          Rules ++= stabilize(main~NE & rail~NS | minor~SW | % | main~SW ) //Viaduct DxO
          Rules ++= stabilize(main~SE & rail~SW | minor~NW | % | main~NW ) //Viaduct DxD
            }
          }
          for ( minor <- Ord) {
          for ( main <- EordNetworks){
          Rules ++= stabilize(main~EW & rail~NS | minor~EW | % | main~EW ) //Viaduct OxO
          Rules ++= stabilize(main~EW & rail~SW | minor~EW | % | main~EW ) //Viaduct OxD
          Rules ++= stabilize(main~NE & rail~NS | minor~SW | % | main~SW ) //Viaduct DxO
          Rules ++= stabilize(main~SE & rail~SW | minor~NW | % | main~NW ) //Viaduct DxD
            }
          }
          for ( minor <- Ave) {
          for ( main <- EaveNetworks){
          Rules ++= stabilize(main~EW & rail~NS | minor~EW | % | main~EW ) //Viaduct OxO
            }
          }
          for ( minor <- Drd) {
          for ( main <- ErhwNetworks){
          Rules ++= stabilize(main~EW & rail~NS | minor~EW | % | main~EW ) //Viaduct OxO
          }
          for ( main <- BaseNetworks) {
          Rules ++= stabilize(main~CW | rail~NS | main~EW  | rail~NS ) //Viaduct OxO Stub
          Rules ++= stabilize(main~CW | rail~NE | main~EW  | rail~NE ) //Viaduct OxD Stub
          Rules ++= stabilize(main~(0,0,0,3) | rail~NS | main~ES  | rail~NS ) //Viaduct DxO Stub
          Rules ++= stabilize(main~(0,0,0,3) | rail~NE | main~ES  | rail~NE ) //Viaduct DxD Stub  
          }      
          for ( main <- ViaductNetworks1){
          Rules ++= stabilize(main~EW | rail~NS | % | main~EW & rail~NS ) //Viaduct OxO
          Rules ++= stabilize(main~WE | rail~SN | % | main~WE & rail~SN ) //Viaduct OxO
          }
          for ( main <- ViaductNetworks2){
          Rules ++= stabilize(main~EW | rail~NE | % | main~EW & rail~NE ) //Viaduct OxD
          Rules ++= stabilize(main~ES | rail~NS | % | main~NW & rail~NS ) //Viaduct DxO
          Rules ++= stabilize(main~ES | rail~NE | % | main~NW & rail~NE ) //Viaduct DxD
                  
          Rules ++= stabilize(main~WE | rail~EN | % | main~WE & rail~EN ) //Viaduct OxD
          Rules ++= stabilize(main~SE | rail~SN | % | main~WN & rail~SN ) //Viaduct DxO
          Rules ++= stabilize(main~SE | rail~EN | % | main~WN & rail~EN ) //Viaduct DxD
          
          Rules ++= stabilize( main~EW & rail~NS | Hrw~NS | % | main~EW & rail~NS ) //Viaduct OxO     
          Rules ++= stabilize( main~WE & rail~SN | Hrw~SN | % | main~WE & rail~SN ) //Viaduct OxO
          }
      }
  createRules()
 }
}