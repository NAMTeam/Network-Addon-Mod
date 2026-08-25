package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, Flags._, RotFlip._, Implicits._, group.SymGroup._
import scala.collection.mutable.Buffer
import NetworkProperties._

class HybridRailwayRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Adjacencies with Stability {

  val HrwNetworks = List(L1Hrw, L2Hrw)

  def start(): Unit = {
    for (main <- HrwNetworks; base <- main.base) {
      Rules += main~WE    | (base ~> main)~WE      // ortho
      Rules += main~WE    | (base ~> main)~WC      // ortho stub
      
      // crossings (O×O, O×D, D×O, D×D)
      for (minor <- CrossingGenerator.crossingNetworksOf(main)) {
      }
    }
  }
  createRules()
}