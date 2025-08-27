package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._
import Network._, Flags._, Flag._, RotFlip._, Implicits._, group.SymGroup._
import NetworkProperties._

class ViaductRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Curve45Generator with CrossingGenerator {

  def start(): Unit = {
    for (main <- Viaducts; base <- main.base) {
      Rules += main~WE    | (base ~> main)~WE      // ortho
      Rules += main~WE    | (base ~> main)~WC      // ortho stub
      withSharedDiagonals {
        Rules += main~SE~ES | (base ~> main)~WN~NW   // diagonal
      }
      // curves
      createCurve45Rules(main)
      createCurve90Rules(main)

      // crossings (O×O, O×D, D×O, D×D)
      for (minor <- CrossingGenerator.crossingNetworksOf(main)) {
        createCrossingRules(main, minor)
      }
      // at-grades
      Rules += L1Road~WE | Road~WE & L1Road~NS | % | L1Road~WE & L1Road~NS
      Rules += L1Road~WE | Road~WE & L1Onewayroad~NS | % | L1Road~WE & L1Onewayroad~NS
      Rules += L1Road~WE | Road~WE & L1Avenue~NS | % | L1Road~WE & L1Avenue~NS
      Rules += L1Road~WE & L1Avenue~NS | Road~WE & L1Avenue~SN | % | L1Road~WE & L1Avenue~SN
      
      Rules += L1Onewayroad~WE | Onewayroad~WE & L1Road~NS | % | L1Onewayroad~WE & L1Road~NS
      Rules += L1Onewayroad~WE | Onewayroad~WE & L1Onewayroad~NS | % | L1Onewayroad~WE & L1Onewayroad~NS
      Rules += L1Onewayroad~WE | Onewayroad~WE & L1Avenue~NS | % | L1Onewayroad~WE & L1Avenue~NS
      Rules += L1Onewayroad~WE & L1Avenue~NS | Onewayroad~WE & L1Avenue~SN | % | L1Onewayroad~WE & L1Avenue~SN
      
      Rules += L1Avenue~EW | Avenue~EW & L1Road~NS | % | L1Avenue~EW & L1Road~NS
      Rules += L1Avenue~EW | Avenue~EW & L1Onewayroad~NS | % | L1Avenue~EW & L1Onewayroad~NS
      Rules += L1Avenue~EW | Avenue~EW & L1Avenue~NS | % | L1Avenue~EW & L1Avenue~NS
      Rules += L1Avenue~EW & L1Avenue~NS | Avenue~EW & L1Avenue~SN | % | L1Avenue~WE & L1Avenue~SN
      
      Rules += L1Road~WE | Road~WE & Road~NC | % | L1Road~WE & L1Road~NC
      Rules += L1Road~WE | Road~WE & Onewayroad~NC | % | L1Road~WE & L1Onewayroad~NC
      Rules += L1Road~WE | Road~WE & Avenue~NC | % | L1Road~WE & L1Avenue~NC
      Rules += L1Road~WE | Road~WC & Avenue~NS | % | L1Road~WC & L1Avenue~NS
      Rules += L1Road~WE & L1Avenue~NC | Road~WE & Avenue~CN | % | L1Road~WE & L1Avenue~CN
      Rules += L1Road~WE & L1Avenue~NS | Road~WC & Avenue~SN | % | L1Road~WC & L1Avenue~SN
      Rules += L1Road~WE & Avenue~NS | Road~WC & Avenue~SN | L1Road~WE & L1Avenue~NS | L1Road~WC & L1Avenue~SN
      
      Rules += L1Onewayroad~WE | Onewayroad~WE & Road~NC | % | L1Onewayroad~WE & L1Road~NC
      Rules += L1Onewayroad~WE | Onewayroad~WE & Onewayroad~NC | % | L1Onewayroad~WE & L1Onewayroad~NC
      Rules += L1Onewayroad~WE | Onewayroad~WE & Avenue~NC | % | L1Onewayroad~WE & L1Avenue~NC
      Rules += L1Onewayroad~WE | Onewayroad~WC & Avenue~NS | % | L1Onewayroad~WC & L1Avenue~NS
      Rules += L1Onewayroad~WE & L1Avenue~NC | Onewayroad~WE & Avenue~CN | % | L1Onewayroad~WE & L1Avenue~CN
      Rules += L1Onewayroad~WE & L1Avenue~NS | Onewayroad~WC & Avenue~SN | % | L1Onewayroad~WC & L1Avenue~SN
      Rules += L1Onewayroad~WE & Avenue~NS | Onewayroad~WC & Avenue~SN | L1Onewayroad~WE & L1Avenue~NS | L1Onewayroad~WC & L1Avenue~SN
     
      Rules += L1Avenue~EW | Avenue~EW & Road~NC | % | L1Avenue~EW & L1Road~NC
      Rules += L1Avenue~WE | Avenue~WE & Road~NC | % | L1Avenue~WE & L1Road~NC
      Rules += L1Avenue~EW | Avenue~EW & Onewayroad~NC | % | L1Avenue~EW & L1Onewayroad~NC
      Rules += L1Avenue~WE | Avenue~WE & Onewayroad~NC | % | L1Avenue~WE & L1Onewayroad~NC
      Rules += L1Avenue~WE | Avenue~WE & Avenue~NC | % | L1Avenue~WE & L1Avenue~NC
      Rules += L1Avenue~WE & L1Avenue~NC | Avenue~WE & Avenue~CN | % | L1Avenue~WE & L1Avenue~CN
      Rules += L1Avenue~NS & L1Road~WC | Avenue~SN | % | L1Avenue~SN
      Rules += L1Avenue~NS & L1Onewayroad~WC | Avenue~SN | % | L1Avenue~SN

      Rules += L2Road~WE | Road~WE & L2Road~NS | % | L2Road~WE & L2Road~NS
      Rules += L2Road~WE | Road~WE & L2Onewayroad~NS | % | L2Road~WE & L2Onewayroad~NS
      Rules += L2Road~WE | Road~WE & L2Avenue~NS | % | L2Road~WE & L2Avenue~NS
      Rules += L2Road~WE & L2Avenue~NS | Road~WE & L2Avenue~SN | % | L2Road~WE & L2Avenue~SN
      
      Rules += L2Onewayroad~WE | Onewayroad~WE & L2Road~NS | % | L2Onewayroad~WE & L2Road~NS
      Rules += L2Onewayroad~WE | Onewayroad~WE & L2Onewayroad~NS | % | L2Onewayroad~WE & L2Onewayroad~NS
      Rules += L2Onewayroad~WE | Onewayroad~WE & L2Avenue~NS | % | L2Onewayroad~WE & L2Avenue~NS
      Rules += L2Onewayroad~WE & L2Avenue~NS | Onewayroad~WE & L2Avenue~SN | % | L2Onewayroad~WE & L2Avenue~SN
      
      Rules += L2Avenue~EW | Avenue~EW & L2Road~NS | % | L2Avenue~EW & L2Road~NS
      Rules += L2Avenue~EW | Avenue~EW & L2Onewayroad~NS | % | L2Avenue~EW & L2Onewayroad~NS
      Rules += L2Avenue~EW | Avenue~EW & L2Avenue~NS | % | L2Avenue~EW & L2Avenue~NS
      Rules += L2Avenue~EW & L2Avenue~NS | Avenue~EW & L2Avenue~SN | % | L2Avenue~WE & L2Avenue~SN
      
      Rules += L2Road~WE | Road~WE & Road~NC | % | L2Road~WE & L2Road~NC
      Rules += L2Road~WE | Road~WE & Onewayroad~NC | % | L2Road~WE & L2Onewayroad~NC
      Rules += L2Road~WE | Road~WE & Avenue~NC | % | L2Road~WE & L2Avenue~NC
      Rules += L2Road~WE | Road~WC & Avenue~NS | % | L2Road~WC & L2Avenue~NS
      Rules += L2Road~WE & L2Avenue~NC | Road~WE & Avenue~CN | % | L2Road~WE & L2Avenue~CN
      Rules += L2Road~WE & L2Avenue~NS | Road~WC & Avenue~SN | % | L2Road~WC & L2Avenue~SN
      Rules += L2Road~WE & Avenue~NS | Road~WC & Avenue~SN | L2Road~WE & L2Avenue~NS | L2Road~WC & L2Avenue~SN
      
      Rules += L2Onewayroad~WE | Onewayroad~WE & Road~NC | % | L2Onewayroad~WE & L2Road~NC
      Rules += L2Onewayroad~WE | Onewayroad~WE & Onewayroad~NC | % | L2Onewayroad~WE & L2Onewayroad~NC
      Rules += L2Onewayroad~WE | Onewayroad~WE & Avenue~NC | % | L2Onewayroad~WE & L2Avenue~NC
      Rules += L2Onewayroad~WE | Onewayroad~WC & Avenue~NS | % | L2Onewayroad~WC & L2Avenue~NS
      Rules += L2Onewayroad~WE & L2Avenue~NC | Onewayroad~WE & Avenue~CN | % | L2Onewayroad~WE & L2Avenue~CN
      Rules += L2Onewayroad~WE & L2Avenue~NS | Onewayroad~WC & Avenue~SN | % | L2Onewayroad~WC & L2Avenue~SN
      Rules += L2Onewayroad~WE & Avenue~NS | Onewayroad~WC & Avenue~SN | L2Onewayroad~WE & L2Avenue~NS | L2Onewayroad~WC & L2Avenue~SN
      
      Rules += L2Avenue~EW | Avenue~EW & Road~NC | % | L2Avenue~EW & L2Road~NC
      Rules += L2Avenue~WE | Avenue~WE & Road~NC | % | L2Avenue~WE & L2Road~NC
      Rules += L2Avenue~EW | Avenue~EW & Onewayroad~NC | % | L2Avenue~EW & L2Onewayroad~NC
      Rules += L2Avenue~WE | Avenue~WE & Onewayroad~NC | % | L2Avenue~WE & L2Onewayroad~NC
      Rules += L2Avenue~WE | Avenue~WE & Avenue~NC | % | L2Avenue~WE & L2Avenue~NC
      Rules += L2Avenue~WE & L2Avenue~NC | Avenue~WE & Avenue~CN | % | L2Avenue~WE & L2Avenue~CN
      Rules += L2Avenue~NS & L2Road~WC | Avenue~SN | % | L2Avenue~SN
      Rules += L2Avenue~NS & L2Onewayroad~WC | Avenue~SN | % | L2Avenue~SN
	  
	  // at-grade exit
      
      Rules += L1Road~WE & L1Road~NS | Road~WE | % | L1Road~WE
      Rules += L1Road~WE & L1Road~NC | Road~WE | % | L1Road~WE
      Rules += L1Road~WE & L1Onewayroad~NS | Road~WE | % | L1Road~WE
      Rules += L1Road~WE & L1Onewayroad~NC | Road~WE | % | L1Road~WE
      Rules += L1Road~WE & L1Avenue~SN | Road~WE | % | L1Road~WE
      Rules += L1Road~WE & L1Avenue~CN | Road~WE | % | L1Road~WE
      Rules += L1Road~CE & L1Road~NS | Road~WE | % | L1Road~WE
      Rules += L1Road~CE & L1Onewayroad~NS | Road~WE | % | L1Road~WE
      Rules += L1Road~CE & L1Avenue~NS | Road~WE | % | L1Road~WE
	  
      Rules += L1Onewayroad~WE & L1Road~NS | Onewayroad~WE | % | L1Onewayroad~WE
      Rules += L1Onewayroad~WE & L1Road~NC | Onewayroad~WE | % | L1Onewayroad~WE
      Rules += L1Onewayroad~WE & L1Onewayroad~NS | Onewayroad~WE | % | L1Onewayroad~WE
      Rules += L1Onewayroad~WE & L1Onewayroad~NC | Onewayroad~WE | % | L1Onewayroad~WE
      Rules += L1Onewayroad~WE & L1Avenue~SN | Onewayroad~WE | % | L1Onewayroad~WE
      Rules += L1Onewayroad~WE & L1Avenue~CN | Onewayroad~WE | % | L1Onewayroad~WE
      Rules += L1Onewayroad~CE & L1Road~NS | Onewayroad~WE | % | L1Onewayroad~WE
      Rules += L1Onewayroad~CE & L1Onewayroad~NS | Onewayroad~WE | % | L1Onewayroad~WE
      Rules += L1Onewayroad~CE & L1Avenue~NS | Onewayroad~WE | % | L1Onewayroad~WE

      Rules += L1Avenue~EW & L1Road~NS | Avenue~EW | % | L1Avenue~EW
      Rules += L1Avenue~EW & L1Road~NC | Avenue~EW | % | L1Avenue~EW
      Rules += L1Avenue~WE & L1Road~NC | Avenue~WE | % | L1Avenue~WE
      Rules += L1Avenue~EW & L1Onewayroad~NS | Avenue~EW | % | L1Avenue~EW
      Rules += L1Avenue~EW & L1Onewayroad~NC | Avenue~EW | % | L1Avenue~EW
      Rules += L1Avenue~WE & L1Onewayroad~NC | Avenue~WE | % | L1Avenue~WE
      Rules += L1Avenue~EW & L1Avenue~SN | Avenue~EW | % | L1Avenue~EW
      Rules += L1Avenue~WE & L1Avenue~CN | Avenue~WE | % | L1Avenue~WE
      Rules += L1Avenue~EC & L1Road~NS | Avenue~EW | % | L1Avenue~EW
      Rules += L1Avenue~CE & L1Road~NS | Avenue~WE | % | L1Avenue~WE
      Rules += L1Avenue~EC & L1Onewayroad~NS | Avenue~EW | % | L1Avenue~EW
      Rules += L1Avenue~CE & L1Onewayroad~NS | Avenue~WE | % | L1Avenue~WE

      Rules += L2Road~WE & L2Road~NS | Road~WE | % | L2Road~WE
      Rules += L2Road~WE & L2Road~NC | Road~WE | % | L2Road~WE
      Rules += L2Road~WE & L2Onewayroad~NS | Road~WE | % | L2Road~WE
      Rules += L2Road~WE & L2Onewayroad~NC | Road~WE | % | L2Road~WE
      Rules += L2Road~WE & L2Avenue~SN | Road~WE | % | L2Road~WE
      Rules += L2Road~WE & L2Avenue~CN | Road~WE | % | L2Road~WE
      Rules += L2Road~CE & L2Road~NS | Road~WE | % | L2Road~WE
      Rules += L2Road~CE & L2Onewayroad~NS | Road~WE | % | L2Road~WE
      Rules += L2Road~CE & L2Avenue~NS | Road~WE | % | L2Road~WE
	  
      Rules += L2Onewayroad~WE & L2Road~NS | Onewayroad~WE | % | L2Onewayroad~WE
      Rules += L2Onewayroad~WE & L2Road~NC | Onewayroad~WE | % | L2Onewayroad~WE
      Rules += L2Onewayroad~WE & L2Onewayroad~NS | Onewayroad~WE | % | L2Onewayroad~WE
      Rules += L2Onewayroad~WE & L2Onewayroad~NC | Onewayroad~WE | % | L2Onewayroad~WE
      Rules += L2Onewayroad~WE & L2Avenue~SN | Onewayroad~WE | % | L2Onewayroad~WE
      Rules += L2Onewayroad~WE & L2Avenue~CN | Onewayroad~WE | % | L2Onewayroad~WE
      Rules += L2Onewayroad~CE & L2Road~NS | Onewayroad~WE | % | L2Onewayroad~WE
      Rules += L2Onewayroad~CE & L2Onewayroad~NS | Onewayroad~WE | % | L2Onewayroad~WE
      Rules += L2Onewayroad~CE & L2Avenue~NS | Onewayroad~WE | % | L2Onewayroad~WE

      Rules += L2Avenue~EW & L2Road~NS | Avenue~EW | % | L2Avenue~EW
      Rules += L2Avenue~EW & L2Road~NC | Avenue~EW | % | L2Avenue~EW
      Rules += L2Avenue~WE & L2Road~NC | Avenue~WE | % | L2Avenue~WE
      Rules += L2Avenue~EW & L2Onewayroad~NS | Avenue~EW | % | L2Avenue~EW
      Rules += L2Avenue~EW & L2Onewayroad~NC | Avenue~EW | % | L2Avenue~EW
      Rules += L2Avenue~WE & L2Onewayroad~NC | Avenue~WE | % | L2Avenue~WE
      Rules += L2Avenue~EW & L2Avenue~SN | Avenue~EW | % | L2Avenue~EW
      Rules += L2Avenue~WE & L2Avenue~CN | Avenue~WE | % | L2Avenue~WE
      Rules += L2Avenue~EC & L2Road~NS | Avenue~EW | % | L2Avenue~EW
      Rules += L2Avenue~CE & L2Road~NS | Avenue~WE | % | L2Avenue~WE
      Rules += L2Avenue~EC & L2Onewayroad~NS | Avenue~EW | % | L2Avenue~EW
      Rules += L2Avenue~CE & L2Onewayroad~NS | Avenue~WE | % | L2Avenue~WE
    }
  }
}

// Compile individually with `sbt "runMain com.sc4nam.module.CompileViaductCode"`.
object CompileViaductCode extends AbstractMain {
  lazy val resolve: IdResolver = new MiscResolver orElse new RealRailwayResolver orElse new RhwResolver orElse new NwmResolver orElse new ViaductResolver
  val generator = new ViaductRuleGenerator(_)
//  lazy val file = new java.io.File("target/ViaductMetaGenerated_MANAGED.txt")
  lazy val file = new java.io.File("Controller/RUL2/06_Road_Viaducts/ViaductMetaGenerated_MANAGED.txt")
}
