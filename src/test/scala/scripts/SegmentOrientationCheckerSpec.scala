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
  }

}
