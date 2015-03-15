package metarules.module

import java.io.{File, PrintWriter}
import metarules.meta.{RuleGenerator, IdResolver, EquivRule}

/** Usage: Replace (in source code) `resolve` and `generator` by custom
  * implementation, optionally replace `file`, too.
  * Then call `sbt run` to execute.
  */
object Main extends AbstractMain {

  lazy val resolve: IdResolver = new RhwResolver orElse new MiscResolver orElse new NwmResolver
  lazy val generator: RuleGenerator = new RhwRuleGenerator(resolve)
  lazy val file = new File("target/output.txt")
}

abstract class AbstractMain {

  def resolve: IdResolver
  def generator: RuleGenerator
  def file: File

  def main(args: Array[String]): Unit = start()

  def start(file: File = file, generator: RuleGenerator = generator): Unit = {
    generator.start()
    // TODO to be revised, later, in order to make more efficient
    val printer = new PrintWriter(file)
    try {
      val seen = collection.mutable.Set.empty[EquivRule] // remember seen rules to avoid duplicates
      for (rule <- generator.queue if seen.add(new EquivRule(rule))) {
        printer.println(rule(0) + "," + rule(1) + "=" + rule(2) + "," + rule(3))
      }
    } finally {
      printer.close()
    }
  }
}
