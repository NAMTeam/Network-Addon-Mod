package com.sc4nam.module

import java.io.File
import syntax.{RuleGenerator, IdResolver, RuleTransducer}

/** Outputs RRW RUL2 code to 'target/Sec13r_HRW_MANAGED.txt'
  */
object CompileHybridRailwayCode extends AbstractMain {

  lazy val resolve: IdResolver = new HybridRailwayResolver orElse new SamResolver orElse new RhwResolver orElse new MiscResolver orElse new NwmResolver orElse new ViaductResolver
  val generator = new HybridRailwayRuleGenerator(_)
  lazy val file = new File("target/Sec13r_HRW_MANAGED.txt")

}