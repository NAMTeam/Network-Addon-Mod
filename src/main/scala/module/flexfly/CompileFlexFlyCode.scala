package metarules
package module.flexfly

import java.io.File
import meta.{RuleGenerator, IdResolver}

/** Outputs FlexFly RUL2 code to 'target/FlexFlyRUL2.txt'
  */
object CompileFlexFlyCode extends module.AbstractMain {

  lazy val resolve: IdResolver = new FlexFlyResolver
  lazy val generator: RuleGenerator = new FlexFlyRuleGenerator(resolve)
  lazy val file = new File("Controller/RUL2/07_RHW/Sec7j_FLEXFly/Sec7j_FLEXFly_MANAGED.txt")

}
