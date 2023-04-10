package metarules.module
/* The metarule code in this file is mostly self-contained and currently
 * covers orthogonal onslope transitions for RHW networks.
 */

import metarules.meta._
import syntax._, Network._, Flags._, RotFlip._, Implicits._, group.SymGroup._
import NetworkProperties._
import RhwRuleGenerator.HeightLevel

trait Onslope { this: RuleGenerator with Curve45Generator =>

  def createOnslopeTransition(): Unit = {
    val rhw2SlopeL1 = IdTile(0x57700000,1,0, (Dirtroad~EC).symmetries)  // direction East (upper) to West (lower)
    val rhw2SlopeL2 = IdTile(0x57700100,1,0, (Dirtroad~EC).symmetries)  // direction East (upper) to West (lower)

    for (main <- RhwNetworks - Dirtroad - Rhw10c if main.height == 0) {
      val maxHeight = if ((Mis + Rhw4 + Rhw6s).contains(main)) 4 else 2
      val rangeId = (RhwResolver.rhwRangeId(main) & 0xFFFFF) + ((RhwResolver.rhwRangeId(main) >>> 4) & 0xF000)  // e.g. 0x88080 for Rhw6cm
      for {
        (levelDiff, rhw2Slope) <- Seq((1, rhw2SlopeL1), (2, rhw2SlopeL2))  // L1 vs L2 onslopes
        height <- 0 to (maxHeight-levelDiff)
      } /*do*/ {
        val lower: Network = height~main
        val upper: Network = (height+levelDiff)~main
        // The following tile is the onslope transition connecting lower and upper network using its explicit IID
        val onslope = IdTile(0x57700000 + rangeId + 0x100*(levelDiff-1) + 0x10*height, R1F0, (main~EC).symmetries)  // direction East (upper) to West (lower)
        Rules += lower~EW | rhw2Slope | % | onslope   // lower > OST
        Rules += onslope | (Dirtroad ~> upper)~EW     // OST > upper
        Rules += rhw2Slope | upper~EW | onslope | %   // OST < upper
        Rules += (Dirtroad ~> lower)~EW | onslope     // lower < OST

        // adjacencies
        for (minor <- RhwNetworks ++ (BaseNetworks - Subway) ++ NwmNetworks) {  // crossing network
          if (intersectionAllowed(upper, minor)) {
            if (hasRightShoulder(minor)) {
              Rules += onslope | (Dirtroad ~> upper)~EW & minor~NS    // OST > upper crossing minor
              Rules += rhw2Slope | upper~EW & minor~NS | onslope | %  // OST < upper crossing minor
            }
            if (hasLeftShoulder(minor)) {
              Rules += onslope | (Dirtroad ~> upper)~EW & minor~SN    // OST > upper crossing minor
              Rules += rhw2Slope | upper~EW & minor~SN | onslope | %  // OST < upper crossing minor
            }
          }
          if (intersectionAllowed(lower, minor)) {
            if (hasRightShoulder(minor)) {
              Rules += (Dirtroad ~> lower)~EW & minor~SN | onslope    // lower crossing minor < OST
              Rules += lower~EW & minor~SN | rhw2Slope | % | onslope  // lower crossing minor > OST
            }
            if (hasLeftShoulder(minor)) {
              Rules += (Dirtroad ~> lower)~EW & minor~NS | onslope    // lower crossing minor < OST
              Rules += lower~EW & minor~NS | rhw2Slope | % | onslope  // lower crossing minor > OST
            }
          }
        }

        // OST adjacent to 45 degree curves
        if (isSingleTile(main)) {  // curves adjacent to OSTs only seem useful for tight setups with single-tilers
          if (hasSharpCurveBase(upper, inside=false) && hasSharpCurve(upper, inside=false)) {
            Rules += onslope | (Dirtroad ~> upper)~(+2,0,-13,0)    // OST > R0 upper
            Rules += rhw2Slope | upper~(+2,0,-13,0) | onslope | %  // OST < R0 upper
            Rules += lower~(+11,0,-2,0) | rhw2Slope | % | onslope  // R0 lower > OST
            Rules += (Dirtroad ~> lower)~(+11,0,-2,0) | onslope    // R0 lower < OST
          }
          if (hasSharpCurveBase(upper, inside=true) && hasSharpCurve(upper, inside=true)) {
            Rules += onslope | (Dirtroad ~> upper)~(+2,0,-11,0)    // OST > R0 upper
            Rules += rhw2Slope | upper~(+2,0,-11,0) | onslope | %  // OST < R0 upper
            Rules += lower~(+13,0,-2,0) | rhw2Slope | % | onslope  // R0 lower > OST
            Rules += (Dirtroad ~> lower)~(+13,0,-2,0) | onslope    // R0 lower < OST
          }
          if (hasR1CurveBase(upper) && hasR1Curve(upper, inside=false)) {
            Rules += onslope | (Dirtroad ~> upper)~(+2,0,-123,0)    // OST > R1 upper
            Rules += rhw2Slope | upper~(+2,0,-123,0) | onslope | %  // OST < R1 upper
            Rules += lower~(+121,0,-2,0) | rhw2Slope | % | onslope  // R1 lower > OST
            Rules += (Dirtroad ~> lower)~(+121,0,-2,0) | onslope    // R1 lower < OST
          }
          if (hasR1CurveBase(upper) && hasR1Curve(upper, inside=true)) {
            Rules += onslope | (Dirtroad ~> upper)~(+2,0,-121,0)    // OST > R1 upper
            Rules += rhw2Slope | upper~(+2,0,-121,0) | onslope | %  // OST < R1 upper
            Rules += lower~(+123,0,-2,0) | rhw2Slope | % | onslope  // R1 lower > OST
            Rules += (Dirtroad ~> lower)~(+123,0,-2,0) | onslope    // R1 lower < OST
          }
        }
        if (main == Mis || main == Rhw4) {  // FlexFly curves
          Rules += rhw2Slope | upper~(+2,0,-213,0) | onslope | %  // OST < FlexFly upper
          Rules += rhw2Slope | upper~(+2,0,-211,0) | onslope | %  //
          Rules += lower~(+211,0,-2,0) | rhw2Slope | % | onslope  // FlexFly lower > OST
          Rules += lower~(+213,0,-2,0) | rhw2Slope | % | onslope  //
        }

        // OST adjacent to OST
        if (upper.height <= maxHeight - 1) {
          // +L1
          val onslopeUpL1 = IdTile((onslope.id & 0xFFFFF0FF) + 0x10 * levelDiff, R1F0, onslope.symmetries)
          Rules += onslope | rhw2SlopeL1 | % | onslopeUpL1  // lower > upper
          Rules += rhw2Slope | onslopeUpL1 | onslope | %    // lower < upper
        }
        if (upper.height <= maxHeight - 2) {
          // +L2
          val onslopeUpL2 = IdTile((onslope.id & 0xFFFFF0FF) + 0x100 + 0x10 * levelDiff, R1F0, onslope.symmetries)
          Rules += onslope | rhw2SlopeL2 | % | onslopeUpL2  // lower > upper
          Rules += rhw2Slope | onslopeUpL2 | onslope | %    // lower < upper
        }
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

// Compile individually with `sbt "runMain metarules.module.CompileOnslopeCode"`.
object CompileOnslopeCode extends AbstractMain {
  lazy val resolve: IdResolver = new MiscResolver orElse new flexfly.FlexFlyResolver orElse new NwmResolver
  lazy val generator: RuleGenerator = new OnslopeGenerator(RuleTransducer.Context(resolve))
  lazy val file = new java.io.File("target/OnslopeMetaGenerated_MANAGED.txt")
}
