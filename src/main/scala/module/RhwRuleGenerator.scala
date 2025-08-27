package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._
import Network._, Flags._, Flag._, RotFlip._, Implicits._, group.SymGroup._
import NetworkProperties._


object RhwRuleGenerator {

  implicit class HeightLevel(val level: Int) extends AnyVal {
    def ~ (n: Network): Network = if (n == Dirtroad) {
      if (level > 0 && level <= 2) {
        Network(L1Rhw2.id + level - 1)
      } else {
        require(level == 0)
        Dirtroad
      }
    } else if (level == 0 && (n == L1Rhw2 || n == L2Rhw2)) {
      Dirtroad
    } else {
      require(n.height == 0)
      val m = Network(n.id + (level - n.height))
      assert(m.height == level)
      m
    }
  }
}

class RhwRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Curve45Generator with CrossingGenerator {
  import RhwRuleGenerator._

  def createMultiTileStarters(): Unit = {
    val g = 0 // ground level
    val heights = 0 to 2
    for (h <- heights if h != g) {
      // C networks
      val cMultis = Iterable(Rhw6c, Rhw8c, Rhw10c)
      for (m <- cMultis) {
        Rules += h~Rhw6cm~SN | g~m~SN | % | h~m~SN
        Rules += h~m~WE~EW | g~m~WE~EW | % | h~m~WE~EW  // stability against starter-induced auto-L0 issues
        Rules += h~m~WE~EW | g~m~WC~CW | % | h~m~WC~CW  // stability
      }
      // S networks
      val sMultis = Iterable(Rhw8s, Rhw10s, Rhw12s)
      for (m <- sMultis) {
        Rules += h~Rhw8sm~SN | g~m~SN | % | h~m~SN
        Rules += h~m~WE~EW | g~m~WE~EW | % | h~m~WE~EW  // stability against starter-induced auto-L0 issues
        Rules += h~m~WE~EW | g~m~WC~CW | % | h~m~WC~CW  // stability
      }
      createRules()
    }
  }

  def start(): Unit = {
    createMultiTileStarters()

    for (main <- RhwNetworks; base <- main.base) {
      Rules += main~WE    | (base ~> main)~WE      // ortho
      Rules += main~WE    | (base ~> main)~WC      // ortho stub
      withSharedDiagonals {
        Rules += main~SE~ES | (base ~> main)~WN~NW   // diagonal
      }
      // curves
      createCurve45Rules(main)
      createCurve90Rules(main)

      // crossings (O×O, O×D, D×O, D×D)
      for (minor <- CrossingGenerator.crossingNetworksOf(main)) {
        createCrossingRules(main, minor)
      }
    }
  }
}

// Compile individually with `sbt "runMain com.sc4nam.module.CompileRhwCode"`.
object CompileRhwCode extends AbstractMain {
  lazy val resolve: IdResolver = new MiscResolver orElse new RealRailwayResolver orElse new RhwResolver orElse new NwmResolver orElse new ViaductResolver
  val generator = new RhwRuleGenerator(_)
  // lazy val file = new java.io.File("target/RhwMetaGenerated_MANAGED.txt")
  lazy val file = new java.io.File("Controller/RUL2/07_RHW/Sec7c_RHWxRHW/RhwMetaGenerated_MANAGED.txt")
}
