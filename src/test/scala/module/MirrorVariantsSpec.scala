package metarules.module

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import metarules.meta._
import syntax._, Implicits._, RotFlip._, Network._, Flags._

class MirrorVariantsSpec extends AnyWordSpec with Matchers {

  "MirrorVariants" should {
    val resolve = new MiscResolver orElse new RealRailwayResolver orElse new NwmResolver
    "produce expected results for Avenue × Rail crossings" in {
      val generator = new RuleGenerator {
        var context = RuleTransducer.Context(resolve, preprocess = MirrorVariants.preprocessor)
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
      withClue(generator.queue.mkString("\n")) { generator.queue shouldBe Seq(
        Rule(0x5D640100,3,0, 0x04001600,2,0, 0x5D640100,3,0, 0x5D677300,0,1),  // diag > 16
        Rule(0x5D640100,0,0, 0x5D571600,0,1, 0x5D640100,0,0, 0x5D677300,2,0),  //
        Rule(0x5D677300,1,1, 0x04001700,3,0, 0x5D677300,1,1, 0x5D677305,3,1),  // 16 > 17
        Rule(0x5D677300,3,0, 0x5D571700,1,1, 0x5D677300,3,0, 0x5D677305,1,0),  //
        Rule(0x5D677305,2,1, 0x04001700,2,0, 0x5D677305,2,1, 0x5D677305,0,1),  // 17 > 17
        Rule(0x5D677305,0,0, 0x5D571700,0,1, 0x5D677305,0,0, 0x5D677305,2,0),  //
        Rule(0x5D677305,1,1, 0x04001600,3,0, 0x5D677305,1,1, 0x5D677300,3,1),  // 17 > 16
        Rule(0x5D677305,3,0, 0x5D571600,1,1, 0x5D677305,3,0, 0x5D677300,1,0),  //
        Rule(0x5D677300,0,0, 0x03001A00,2,0, 0x5D677300,0,0, 0x5D640100,2,0),  // 16 > diag
        Rule(0x5D677300,2,1, 0x03001A00,1,0, 0x5D677300,2,1, 0x5D640100,1,0))  //
      }
    }

    "produce expected results for Street × Rail crossings" in {
      // This Street crossing differs from the Avenue crossing above in that
      // both variants are unmirrored-only.
      val generator = new RuleGenerator {
        var context = RuleTransducer.Context(resolve, preprocess = MirrorVariants.preprocessor)
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

    "produce expected results for TLA network crossings" in {
      implicit val context = RuleTransducer.Context(resolve, preprocess = MirrorVariants.preprocessor)
      RuleTransducer(Tla3~WE | (Road~>Tla3)~WE & Rail~ES).toSeq shouldBe Seq(
        Rule(0x51000000,1,0, 0x03010200,1,0, 0x51000000,1,0, 0x51005500,3,0),
        Rule(0x51000000,3,0, 0x03010200,1,0, 0x51000000,3,0, 0x51005500,3,0),
        Rule(0x51000000,3,0, 0x03020500,3,1, 0x51000000,3,0, 0x51005500,1,1),
        Rule(0x51000000,1,0, 0x03020500,3,1, 0x51000000,1,0, 0x51005500,1,1))
      RuleTransducer(Tla5~WE | (Road~>Tla5)~WE & Rail~ES).toSeq shouldBe Seq(
        Rule(0x51100000,3,0, 0x03010200,1,0, 0x51100000,3,0, 0x51105500,3,0),
        Rule(0x51100000,1,0, 0x03020500,3,1, 0x51100000,1,0, 0x51105500,1,1))
      RuleTransducer(Tla5~WE | (Road~>Tla5)~WE & Road~ES).toSeq shouldBe Seq(
        Rule(0x51100000,3,0, 0x00003900,1,0, 0x51100000,3,0, 0x51105100,3,0),
        Rule(0x51100000,1,0, 0x00003900,3,1, 0x51100000,1,0, 0x71105100,1,1))  // 0x71... variant
      RuleTransducer(Tla3~WE | (Road~>Tla3)~WE & Lightrail~ES).toSeq shouldBe Seq(
        Rule(0x51000000,1,0, 0x08DD1600,1,1, 0x51000000,1,0, 0x51005600,3,0),
        Rule(0x51000000,3,0, 0x08DD1600,1,1, 0x51000000,3,0, 0x51005600,3,0),
        Rule(0x51000000,3,0, 0x08DD1600,3,0, 0x51000000,3,0, 0x51005600,1,1),
        Rule(0x51000000,1,0, 0x08DD1600,3,0, 0x51000000,1,0, 0x51005600,1,1))
    }
  }
}
