package com.sc4nam.module
/* The metarule code in this file is mostly self-contained and currently
 * covers orthogonal onslope transitions for RHW networks.
 */

import io.github.memo33.metarules.meta._
import syntax._, Network._, Flags._, RotFlip._, Implicits._, group.SymGroup._
import NetworkProperties._
import RhwRuleGenerator.HeightLevel

trait Onslope { this: RuleGenerator with Curve45Generator =>

  def createOnslopeTransition(): Unit = {
    val rhw2SlopeL1 = L1Rhw2~EC & Dirtroad~CW  // IdTile(0x57700000,1,0, (Dirtroad~EC).symmetries)  // direction East (upper) to West (lower)
    val rhw2SlopeL2 = L2Rhw2~EC & Dirtroad~CW  // IdTile(0x57700100,1,0, (Dirtroad~EC).symmetries)  // direction East (upper) to West (lower)

    for (main <- RhwNetworks - Rhw10c if main.height == 0) {
      val maxHeight = if ((Mis + Rhw4 + Rhw6s).contains(main)) 4 else 2
      val minHeight = if (main == Dirtroad) 1 else 0  // avoiding auto-L1Rhw2 and auto-L2Rhw2
      for {
        (levelDiff, rhw2Slope) <- Seq((1, rhw2SlopeL1), (2, rhw2SlopeL2))  // L1 vs L2 onslopes
        height <- minHeight to (maxHeight-levelDiff)
      } /*do*/ {
        val lower: Network = height~main
        val upper: Network = (height+levelDiff)~main
        val rhw2slope = (levelDiff~Dirtroad)~EC & Dirtroad~CW  // direction East (upper) to West (lower)
        val onslope = upper~EC & lower~CW  // direction East (upper) to West (lower)
        Rules += lower~EW | rhw2Slope | % | onslope   // lower > OST
        Rules += onslope | (Dirtroad ~> upper)~EW     // OST > upper
        Rules += rhw2Slope | upper~EW | onslope | %   // OST < upper
        Rules += (Dirtroad ~> lower)~EW | onslope     // lower < OST
      }
    }

    createRules()
  }
}

class OnslopeGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Curve45Generator with Onslope {
  def start(): Unit = {
    createOnslopeTransition()
  }
}

// Compile individually with `sbt "runMain com.sc4nam.module.CompileOnslopeCode"`.
object CompileOnslopeCode extends AbstractMain {
  lazy val resolve: IdResolver = new MiscResolver orElse new flexfly.FlexFlyResolver orElse new NwmResolver
  val generator = new OnslopeGenerator(_)
  lazy val file = new java.io.File("target/Sec7h0_OnslopeMetaGenerated_MANAGED.txt")
}
