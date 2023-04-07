package metarules.module

import org.scalatest.{WordSpec, Matchers}
import metarules.meta._
import Implicits._, RotFlip._, Network._, Flags._

class MirrorVariantsSpec extends WordSpec with Matchers {

  "MirrorVariants" should {
    val resolve = new RealRailwayResolver orElse new MiscResolver
    "produce expected results for Avenue × Rail crossings" in {
      val generator = new RuleGenerator with MirrorVariants {
        val resolver = resolve
        def start(): Unit = {
          val main = L1Dtr
          Rules += main~ES | (Rail ~> main)~NW & Avenue~NS              // diag > 16
          Rules += main~EN & Avenue~EW | (Rail ~> main)~SW & Avenue~EW  // 16 > 17
          Rules += main~ES & Avenue~NS | (Rail ~> main)~NW & Avenue~SN  // 17 > 17
          Rules += main~EN & Avenue~WE | (Rail ~> main)~SW & Avenue~WE  // 17 > 16
          Rules += main~ES & Avenue~SN | (Rail ~> main)~NW              // 16 > diag
          createRules()
        }
      }
      generator.start()
      generator.queue shouldBe Seq(
        Rule(0x5D640100,3,0, 0x04001600,2,0, 0x5D640100,3,0, 0x5D677300,0,1),  // diag > 16
        Rule(0x5D640100,0,0, 0x5D571600,0,1, 0x5D640100,0,0, 0x5D677300,2,0),  //
        Rule(0x5D677300,3,0, 0x04001700,1,1, 0x5D677300,3,0, 0x5D677305,1,0),  // 16 > 17
        Rule(0x5D677300,3,0, 0x5D571700,1,1, 0x5D677300,3,0, 0x5D677305,1,0),  //
        Rule(0x5D677305,0,0, 0x04001700,0,1, 0x5D677305,0,0, 0x5D677305,2,0),  // 17 > 17
        Rule(0x5D677305,0,0, 0x5D571700,0,1, 0x5D677305,0,0, 0x5D677305,2,0),  //
        Rule(0x5D677305,3,0, 0x04001600,1,1, 0x5D677305,3,0, 0x5D677300,1,0),  // 17 > 16
        Rule(0x5D677305,3,0, 0x5D571600,1,1, 0x5D677305,3,0, 0x5D677300,1,0),  //
        Rule(0x5D677300,0,0, 0x03001A00,2,0, 0x5D677300,0,0, 0x5D640100,2,0),  // 16 > diag
        Rule(0x5D677300,0,0, 0x03001A00,3,1, 0x5D677300,0,0, 0x5D640100,3,1))  //
    }

    "produce expected results for Street × Rail crossings" in {
      // This Street crossing differs from the Avenue crossing above in that
      // both variants are unmirrored-only.
      val generator = new RuleGenerator with MirrorVariants {
        val resolver = resolve
        def start(): Unit = {
          val main = L1Dtr
          Rules += main~ES | (Rail ~> main)~NW & Street~NE
          createRules()
        }
      }
      generator.start()
      generator.queue shouldBe Seq(
        Rule(0x5D640100,0,0, 0x5F502700,3,0, 0x5D640100,0,0, 0x5D67A000,2,0),
        Rule(0x5D640100,3,0, 0x5F502800,2,0, 0x5D640100,3,0, 0x5D67A000,0,1))
    }
  }
}
