package metarules
package module.flexfly

import java.io.File
import meta.{RuleGenerator, IdResolver, RuleTransducer}

/** Outputs FlexFly RUL2 code to 'target/Sec7j_FLEXFly_MANAGED.txt'
  */
object CompileFlexFlyCode extends module.AbstractMain {

  lazy val resolve: IdResolver = new FlexFlyResolver
  lazy val generator: RuleGenerator = new FlexFlyRuleGenerator(RuleTransducer.Context(resolve))
  lazy val file = new File("target/Sec7j_FLEXFly_MANAGED.txt")

}
