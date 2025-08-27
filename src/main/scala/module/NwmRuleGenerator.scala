package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._
import Network._, Flags._, Flag._, RotFlip._, Implicits._, group.SymGroup._
import NetworkProperties._

class NwmRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Curve45Generator with CrossingGenerator {

  def start(): Unit = {
    for (main <- NwmNetworks; base <- main.base) {
      Rules += main~WE    | (base ~> main)~WE      // ortho
      Rules += main~WE    | (base ~> main)~WC      // ortho stub
      withSharedDiagonals {
        Rules += main~SE~ES | (base ~> main)~WN~NW   // diagonal
      }
      if (main != Owr4 && main != Owr4m && main != Rd4) {
        Rules += main~SE~ES | (base ~> main)~CNW~WNC   // diagonal stub
      }
      // curves
      createCurve45Rules(main)
      createCurve90Rules(main)

      // crossings (O×O, O×D, D×O, D×D)
      for (minor <- CrossingGenerator.crossingNetworksOf(main)) {
        createCrossingRules(main, minor)
      }
    }
  }
}

// Compile individually with `sbt "runMain com.sc4nam.module.CompileNwmCode"`.
object CompileNwmCode extends AbstractMain {
  lazy val resolve: IdResolver = new MiscResolver orElse new RealRailwayResolver orElse new RhwResolver orElse new NwmResolver orElse new ViaductResolver
  val generator = new NwmRuleGenerator(_)
//  lazy val file = new java.io.File("target/NwmMetaGenerated_MANAGED.txt")
  lazy val file = new java.io.File("Controller/RUL2/08_NWM/NwmMetaGenerated_MANAGED.txt")
}
