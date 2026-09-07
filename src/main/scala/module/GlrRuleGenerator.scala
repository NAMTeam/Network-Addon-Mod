package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._
import Network._, Flags._, Flag._, RotFlip._, Implicits._, group.SymGroup._
import NetworkProperties._

class GlrRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Curve45Generator with CrossingGenerator {

  def start(): Unit = {
    for (glr <- GlrNetworks; base <- glr.base) {
      Rules += glr~WE | (base ~> glr)~WE        // ortho
      Rules += glr~SE | (base ~> glr)~WN        // diagonal
      Rules += glr~WE | base~WC  | % | glr~WE   // ortho stub (TODO implement proper GLR end stubs)
      Rules += glr~SE | base~WNC | % | glr~WN   // diagonal stub
      createCurve45Rules(glr)
      createCurve90Rules(glr)
      Rules ++= reflections((base ~> glr)~WE          | (base ~> glr)~(2,0,62,0))   // Y-wye
      Rules ++= reflections((base ~> glr)~(2,0,62,0)  | (base ~> glr)~(62,3,0,1))   // Y-wye
      Rules ++= reflections((base ~> glr)~(3,0,1,62)  | (base ~> glr)~WN)           // Y-wye
      Rules ++= reflections((base ~> glr)~WE          | (base ~> glr)~(2,0,72,0))   // Ψ-wye
      Rules ++= reflections((base ~> glr)~(2,0,72,0)  | (base ~> glr)~(72,3,2,1))   // Ψ-wye
      Rules ++= reflections((base ~> glr)~(3,2,1,72)  | (base ~> glr)~WN)           // Ψ-wye
      Rules ++= reflections((base ~> glr)~(72,3,2,1)  | (base ~> glr)~WE)           // Ψ-wye
      Rules ++= reflections((base ~> glr)~WE          | (base ~> glr)~(2,0,33,0))   // orth switch
      Rules ++= reflections((base ~> glr)~(2,0,33,0)  | (base ~> glr)~(33,0,2,1))   // orth switch
      Rules ++= reflections((base ~> glr)~(0,2,1,33)  | (base ~> glr)~WN)           // orth switch
      Rules ++= reflections((base ~> glr)~(33,0,2,1)  | (base ~> glr)~WE)           // orth switch
      Rules ++= reflections((base ~> glr)~WE          | (base ~> glr)~(2,0,2,2))    // orth T
      Rules ++= reflections((base ~> glr)~WE          | (base ~> glr)~(2,2,0,2))    // orth T
      Rules ++= reflections((base ~> glr)~NE          | (base ~> glr)~(3,2,0,21))   // diag switch
      Rules ++= reflections((base ~> glr)~WE          | (base ~> glr)~(2,0,21,3))   // diag switch
      Rules ++= reflections((base ~> glr)~(2,0,21,3)  | (base ~> glr)~(21,3,0,0))   // diag switch
      Rules ++= reflections((base ~> glr)~(0,21,3,0)  | (base ~> glr)~WS)           // diag switch
      Rules ++= reflections((base ~> glr)~(0,0,21,3)  | (base ~> glr)~(21,13,2,0))  // diag y
      Rules ++= reflections((base ~> glr)~(0,21,13,2) | (base ~> glr)~(13,0,2,0))   // diag y
      Rules ++= reflections((base ~> glr)~(0,21,13,2) | (base ~> glr)~(13,0,13,0))  // diag y
      Rules ++= reflections((base ~> glr)~(0,21,13,2) | (base ~> glr)~(13,0,11,0))  // diag y
      Rules ++= reflections((base ~> glr)~(21,13,2,0) | (base ~> glr)~WE)           // diag y
      Rules ++= reflections((base ~> glr)~(0,0,11,3)  | (base ~> glr)~(11,0,11,0))  // diag S
      Rules ++= reflections((base ~> glr)~(0,0,11,3)  | (base ~> glr)~(11,0,13,0))  // boomerang
      Rules ++= reflections((base ~> glr)~(0,0,21,3)  | (base ~> glr)~(21,23,2,0))  // diag Ψ
      Rules ++= reflections((base ~> glr)~(0,21,23,2) | (base ~> glr)~(23,0,2,1))   // diag Ψ
      Rules ++= reflections((base ~> glr)~(21,23,2,0) | (base ~> glr)~WE)           // diag Ψ
      Rules += glr~(0,2,21,23) | (base ~> glr)~(21,23,2,0)                          // diag Ψ adjacency
      Rules ++= reflections((base ~> glr)~(0,0,21,3)  | (base ~> glr)~(21,3,13,0))  // diag boomerang switch
      Rules ++= reflections((base ~> glr)~(0,2,21,23) | (base ~> glr)~(21,3,13,0))  // diag boomerang switch adjacency
      Rules ++= reflections((base ~> glr)~(21,3,13,0) | (base ~> glr)~(13,0,0,1))   // diag boomerang switch
      Rules ++= reflections((base ~> glr)~(0,21,3,13) | (base ~> glr)~WS)           // diag boomerang switch
      Rules ++= stabilize(glr~SE         | base~WN & base~NE | glr~(0,0,21,3)  | glr~(21,62,23,0))  // diag X (in) (Note that curved switch has been removed from Lightrail D×D but not from GLR)
      Rules ++= stabilize(glr~(0,2,1,23) | base~WN & base~NE | glr~(0,2,21,23) | glr~(21,62,23,0))  // diag X (in) adjacency
      Rules += glr~(0,21,62,23) | base~WS & base~WN | % | glr~(62,23,0,21)                          // diag X (inner)
      Rules += glr~(21,62,23,0) | base~WS           | % | glr~(23,0,0,1)                            // diag X (out)

      // crossings (O×O, O×D, D×O, D×D)
      Rules ++= reflections((base ~> glr)~WE | (base ~> glr)~(2,2,2,2))             // O×O (TODO implement Lightrail × GLR)
      Rules ++= reflections((base ~> glr)~WE | (base ~> glr)~WE & (base ~> glr)~NE) // O×D (TODO implement Lightrail × GLR)
      Rules ++= reflections((base ~> glr)~NE | (base ~> glr)~WS & (base ~> glr)~NS) // D×O
      Rules += glr~WE & glr~NE | (base ~> glr)~WE & (base ~> glr)~WS                // O×D (inner)
      for (minor <- CrossingGenerator.crossingNetworksOf(glr)) {
        createCrossingRules(glr, minor)
      }
    }
  }

}

// Compile individually with `sbt "runMain com.sc4nam.module.CompileGlrCode"`.
object CompileGlrCode extends AbstractMain {
  lazy val resolve: IdResolver = new MiscResolver orElse new RealRailwayResolver orElse new SamResolver orElse new RhwResolver orElse new NwmResolver orElse new ViaductResolver
  val generator = new GlrRuleGenerator(_)
  lazy val file = new java.io.File("target/Sec10e_GlrMetaGenerated_MANAGED.txt")
}
