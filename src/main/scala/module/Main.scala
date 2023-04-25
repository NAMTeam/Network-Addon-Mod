package com.sc4nam.module

import java.io.{File, PrintWriter}
import io.github.memo33.metarules.meta.{RotFlip, EquivRule, IdTile}
import syntax.{RuleGenerator, IdResolver, RuleTransducer, Tile}

/** The following is the complete mini-example from the metarules readme file:
  * https://github.com/memo33/metarules
  *
  * It defines a custom IdResolver named `resolve` as well as
  * a custom rule `generator`. Call
  * {{{
  * sbt "runMain com.sc4nam.module.MiniExample"
  * }}}
  * to execute. The output is written to the file. Add additional rules to the
  * generator to try out the metarule syntax and observe how the output changes.
  *
  * (Refer to MiscResolver.scala for a more compact way to define an IdResolver:
  * instead of listing all the rotations of an ID, it requires just one rotation.)
  */
object MiniExample extends AbstractMain {

  import io.github.memo33.metarules.meta._, syntax._, Network._, RotFlip._, Flags._, Implicits._

  val resolve = Map[Tile, IdTile](
    (Dirtroad~NS, IdTile(0x57000000, R0F0)),
    (Dirtroad~EW, IdTile(0x57000000, R1F0)),
    (Mis~NS,      IdTile(0x57020000, R0F0)),
    (Mis~EW,      IdTile(0x57020000, R1F0)),
    (Mis~SN,      IdTile(0x57020000, R2F0)),
    (Mis~WE,      IdTile(0x57020000, R3F0)),
    (L1Rhw2~NS,   IdTile(0x57100000, R0F0)),
    (L1Rhw2~EW,   IdTile(0x57100000, R1F0)))

  val generator = (ctx: RuleTransducer.Context) => new RuleGenerator {
    var context = ctx
    def start(): Unit = {
      Rules += Mis~WE    | (Dirtroad ~> Mis)~WE
      Rules += L1Rhw2~WE | (Dirtroad ~> L1Rhw2)~WE
      createRules()
    }
  }

  lazy val file = new File("target/miniexample.txt")
}

/** This class takes care of executing a rule generator and writing the
  * generated RUL2 code to a file.
  * Subclasses need to implement `resolve`, `generator` and `file`.
  */
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
