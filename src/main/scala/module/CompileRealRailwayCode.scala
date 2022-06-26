package metarules
package module

import java.io.File
import meta.{RuleGenerator, IdResolver}

/** Outputs RRW RUL2 code to 'Controller/RUL2/11_Rail/Sec11r_RRW_MANAGED.txt'
  */
object CompileRealRailwayCode extends AbstractMain {

  lazy val resolve: IdResolver = new RealRailwayResolver
  lazy val generator: RuleGenerator = new RealRailwayRuleGenerator(resolve)
  lazy val file = new File("Controller/RUL2/11_Rail/Sec11r_RRW_MANAGED.txt")

}
