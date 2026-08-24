package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, Flags._, RotFlip._, Implicits._, group.SymGroup._
import scala.collection.mutable.Buffer
import NetworkProperties._


class HybridRailwayRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Adjacencies with Stability {

  def start(): Unit = {
    /*
    Generate OxO rules by iteration over list of supported crossings
    */
      val HrwNetworks = List(L1Hrw, L2Hrw)

    val CrossNetworks = List(Street, Road, Onewayroad
      )

    for (main <- HrwNetworks; base <- main.base) {
      // base
      Rules += main~WE | (base ~> main)~WE        // ortho continue
      Rules += main~WE | base~CW | % | main~WE    // ortho continue stub convert
    
      Rules += main~ES | (base ~> main)~NW        // diag continue
      Rules += main~ES | base~CNW | % | main~NW   // diag stub convert
       
      //Crossings OxO
      for (minor <- CrossNetworks if minor.height != main.height) {
      Rules += main~WE & minor~NS | (base ~> main)~WE              // OxO continue
      }
    }
    createRules()
  }
}