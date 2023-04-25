package metarules
package module

import java.io.File
import syntax.{RuleGenerator, IdResolver, RuleTransducer}

/** Outputs RRW RUL2 code to 'target/Sec11r_RRW_MANAGED.txt'
  */
object CompileRealRailwayCode extends AbstractMain {

  lazy val resolve: IdResolver = new RealRailwayResolver orElse new RhwResolver orElse new MiscResolver orElse new NwmResolver
  val generator = new RealRailwayRuleGenerator(_)
  lazy val file = new File("target/Sec11r_RRW_MANAGED.txt")

}
