package metarules.module

import java.io.{File, PrintWriter}
import metarules.meta.{RuleGenerator, IdResolver, EquivRule, RotFlip, RuleTransducer}

/** Usage: Replace (in source code) `resolve` and `generator` by custom
  * implementation, optionally replace `file`, too.
  * Then call `sbt run` to execute.
  */
object Main extends AbstractMain {

  lazy val resolve: IdResolver = new RealRailwayResolver orElse new MiscResolver orElse new RhwResolver orElse new NwmResolver
  lazy val generator: RuleGenerator = new RhwRuleGenerator(RuleTransducer.Context(resolve))
  lazy val file = new File("./Controller/RUL2/07_RHW/RhwMetaGenerated_MANAGED.txt")
}

abstract class AbstractMain {

  def resolve: IdResolver
  def generator: RuleGenerator
  def file: File

  def main(args: Array[String]): Unit = start()

  def start(file: File = file, generator: RuleGenerator = generator, tileOrientationCache: collection.mutable.Map[Int, Set[RotFlip]] = null): Unit = {
    if (tileOrientationCache == null) {
      for (cache <- RegenerateTileOrientationCache.withCache()) {
        start(file, generator, cache)
      }
    } else {
      generator.context = generator.context.copy(tileOrientationCache = tileOrientationCache, preprocess = MirrorVariants.preprocessor)
      generator.start()
      // TODO to be revised, later, in order to make more efficient
      for (printer <- resource.managed(new PrintWriter(file))) {
        printer.println(";This file was generated automatically. DO NOT EDIT!")
        val seen = collection.mutable.Set.empty[EquivRule] // remember seen rules to avoid duplicates
        for (rule <- generator.queue if seen.add(new EquivRule(rule))) {
          printer.println(rule(0) + "," + rule(1) + "=" + rule(2) + "," + rule(3))
        }
      }
    }
  }
}
