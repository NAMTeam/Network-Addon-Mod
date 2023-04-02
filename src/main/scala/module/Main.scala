package metarules.module

import java.io.{File, PrintWriter}
import metarules.meta.{RuleGenerator, IdResolver, EquivRule}

/** Usage: Replace (in source code) `resolve` and `generator` by custom
  * implementation, optionally replace `file`, too.
  * Then call `sbt run` to execute.
  */
object Main extends AbstractMain {

  lazy val resolve: IdResolver = new RealRailwayResolver orElse new MiscResolver orElse new RhwResolver orElse new NwmResolver
  lazy val generator: RuleGenerator = new RhwRuleGenerator(resolve)
  lazy val file = new File("./Controller/RUL2/07_RHW/RhwMetaGenerated_MANAGED.txt")
}

abstract class AbstractMain {

  def resolve: IdResolver
  def generator: RuleGenerator
  def file: File

  def main(args: Array[String]): Unit = start()

  def start(file: File = file, generator: RuleGenerator = generator,
      tileOrientationCache: collection.mutable.Map[Int, Set[metarules.meta.RotFlip]] = RegenerateTileOrientationCache.loadCache()): Unit = {
    generator.tileOrientationCache = tileOrientationCache
    generator.start()
    // TODO to be revised, later, in order to make more efficient
    val printer = new PrintWriter(file)
    try {
      printer.println(";This file was generated automatically. DO NOT EDIT!")
      val seen = collection.mutable.Set.empty[EquivRule] // remember seen rules to avoid duplicates
      for (rule <- generator.queue if seen.add(new EquivRule(rule))) {
        printer.println(rule(0) + "," + rule(1) + "=" + rule(2) + "," + rule(3))
      }
    } finally {
      printer.close()
    }
  }
}
