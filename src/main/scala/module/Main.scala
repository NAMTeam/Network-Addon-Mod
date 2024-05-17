package com.sc4nam.module

import java.io.{File, PrintWriter}
import io.github.memo33.metarules.meta.{RotFlip, EquivRule, IdTile}
import syntax.{RuleGenerator, IdResolver, RuleTransducer, Tile}

/** Usage: Replace (in source code) `resolve` and `generator` by custom
  * implementation, optionally replace `file`, too.
  * Then call `sbt run` to execute.
  */
object Main extends AbstractMain {

  lazy val resolve: IdResolver = new RealRailwayResolver orElse new SamResolver orElse new MiscResolver orElse new RhwResolver orElse new NwmResolver
  val generator = new RhwRuleGenerator(_)
  lazy val file = new File("./Controller/RUL2/07_RHW/RhwMetaGenerated_MANAGED.txt")
}

abstract class AbstractMain {

  def resolve: IdResolver
  def generator: RuleTransducer.Context => RuleGenerator
  def file: File

  /** Wraps the resolver for more informative error messages. */
  lazy val resolveSafely: IdResolver = new PartialFunction[Tile, IdTile] {
    def isDefinedAt(tile: Tile) = resolve.isDefinedAt(tile)
    def apply(tile: Tile) = try resolve.apply(tile) catch {
      case e @ (_: java.util.NoSuchElementException | _: MatchError) =>
        throw new IllegalArgumentException(s"ID resolution failed for tile $tile", e)
    }
  }

  def main(args: Array[String]): Unit = start()

  /** Creates a generator with a new context, runs its start method and outputs the resulting RUL2 code to file. */
  def start(file: File = file, tileOrientationCache: collection.mutable.Map[Int, Set[RotFlip]] = null): Unit = {
    if (tileOrientationCache == null) {
      for (cache <- RegenerateTileOrientationCache.withCache()) {
        start(file, cache)
      }
    } else {
      val context = RuleTransducer.Context(resolveSafely, tileOrientationCache, MirrorVariants.preprocessor)
      val gen = generator(context)
      gen.start()
      // TODO to be revised, later, in order to make more efficient
      for (printer <- resource.managed(new PrintWriter(file))) {
        printer.println(";This file was generated automatically. DO NOT EDIT!")
        val seen = collection.mutable.Set.empty[EquivRule] // remember seen rules to avoid duplicates
        for (rule <- gen.queue if seen.add(new EquivRule(rule))) {
          printer.println(s"${rule(0)},${rule(1)}=${rule(2)},${rule(3)}")
        }
      }
    }
  }
}
