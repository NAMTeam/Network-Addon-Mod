package com.sc4nam.scripts

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import io.github.memo33.metarules.meta._
import com.sc4nam.module._
import syntax._, Implicits._, RotFlip._, Network._, Flags._, NetworkProperties._

class SegmentOrientationCheckerSpec extends AnyWordSpec with Matchers {

  "SegmentOrientationChecker" should {
    "detect wrong base orientations" in {
      SegmentOrientationChecker.isBaseOrientationDifferent(Avenue~WE, L1Avenue~WE).shouldBe(false)
      SegmentOrientationChecker.isBaseOrientationDifferent(Avenue~EW, L1Avenue~WE).shouldBe(true)
      SegmentOrientationChecker.isBaseOrientationDifferent(Avenue~NS, L1Avenue~WE).shouldBe(true)
    }
    "detect wrong segment reversal" in {
      SegmentOrientationChecker.hasSegmentReversal(Dirtroad~WE & Ard3~NS, Mis~WE & Ard3~SN).shouldBe(true)
      SegmentOrientationChecker.hasSegmentReversal(Avenue~WE & Road~NS, L1Avenue~EW & Road~NS).shouldBe(true)
    }
    "detect badly connected segments" in {
      SegmentOrientationChecker.areSegmentsBadlyConnected(Ave2~(11,0,2,0), Road~WS & Ard3~SE, null).shouldBe(true)
    }
    "avoid false positives" in {
      Seq[(Tile, Tile, Rule[IdTile])](
        (Rhw4~WE & Mis~ES, Mis~NW & Dirtroad~EN, Rule(0x57127d05,0,0,0x5700aa00,2,0,0x57127d05,0,0,0x5712aa80,2,0)),
        (Rhw4~EW & Mis~EN, Mis~SW & Dirtroad~ES, Rule(0x57127d05,2,1,0x5700aa00,0,0,0x57127d05,2,1,0x5712aa80,0,1)),
        (Rhw4~WE & Mis~SE, Mis~WN & Dirtroad~EN, Rule(0x57127d85,0,0,0x5700aa00,2,0,0x57127d85,0,0,0x5712aa00,2,0)),
        (Rhw4~EW & Mis~NE, Mis~WS & Dirtroad~ES, Rule(0x57127d85,2,1,0x5700aa00,0,0,0x57127d85,2,1,0x5712aa00,0,1)),
        (Road~CW & Avenue~SN & Onewayroad~EC, Onewayroad~EW, Rule(0x04008800,0,0,0x09000300,3,0,0x04008900,2,0,0x09004b00,3,0)),
      ).foreach { case (t1, t2, rule) =>
        SegmentOrientationChecker.areSegmentsBadlyConnected(t1, t2, rule).shouldBe(false)
      }
    }
  }

}
