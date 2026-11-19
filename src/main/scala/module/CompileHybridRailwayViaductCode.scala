package com.sc4nam.module

import java.io.File
import syntax.{RuleGenerator, IdResolver, RuleTransducer}

/** Outputs HRW RUL2 code to 'target/Sec13s_HRWVIA_MANAGED.txt'
  */
object CompileHybridRailwayViaductCode extends AbstractMain {

  lazy val resolve: IdResolver = new HybridRailwayResolver orElse new SamResolver orElse new RhwResolver orElse new MiscResolver orElse new NwmResolver orElse new ViaductResolver
  val generator = new HybridRailwayViaductRuleGenerator(_)
  lazy val file = new File("target/Sec13s_HRWVIA_MANAGED.txt")

}