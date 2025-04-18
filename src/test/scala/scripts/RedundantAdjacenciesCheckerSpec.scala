package com.sc4nam.scripts

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import io.github.memo33.metarules.meta._
import com.sc4nam.module._
import syntax._, Implicits._, RotFlip._, Network._, Flags._, NetworkProperties._
import RedundantAdjacenciesChecker.isRedundantAdjacency

class RedundantAdjacenciesCheckerSpec extends AnyWordSpec with Matchers {

  "redundant adjacencies" should {
    "be compatible for equivalent rules" in {
      val rul2 = Seq(
        Rule(0x57227C10,2,0,0x57000000,1,0,0x57227C10,2,0,0x57120000,3,0),
        Rule(0x57227C10,2,0,0x57000000,3,0,0x57227C10,2,0,0x57120000,3,0),
        Rule(0x57227C10,2,0,0x57000000,1,1,0x57227C10,2,0,0x57120000,3,1),
        Rule(0x57227C10,2,0,0x57000000,3,1,0x57227C10,2,0,0x57120000,3,1),
        Rule(0x57120000,3,0,0x57031A00,0,0,0x57120000,3,0,0x57121D00,3,0),
        Rule(0x57120000,1,0,0x57031A00,0,0,0x57120000,1,0,0x57121D00,1,1),
        Rule(0x57120000,3,0,0x57031A00,2,0,0x57120000,3,0,0x57121D00,3,1),
        Rule(0x57120000,1,0,0x57031A00,2,0,0x57120000,1,0,0x57121D00,1,0),
        //
        Rule(0x5C020A00,0,1,0x04006100,1,0,0x5C020A00,0,1,0x5C020000,1,0),
        Rule(0x5C020A00,2,0,0x04006100,3,0,0x5C020A00,2,0,0x5C020000,3,0),
        Rule(0x5C020000,1,0,0x04006300,0,0,0x5C020000,1,0,0x5C020A00,0,0),
        Rule(0x5C020000,3,0,0x04006300,2,1,0x5C020000,3,0,0x5C020A00,2,1),
        )
      val lookupRule = rul2.map(r => (new EquivRule(r), r)).toMap
      val adjacencies = Seq(
        Rule(0x57227c10,0,1,0x57031a00,0,0,0x57227c10,0,1,0x57121d00,1,1),
        Rule(0x57227c10,2,0,0x57031a00,2,1,0x57227c10,2,0,0x57121d00,3,0),
        Rule(0x57031a00,2,0,0x57227c10,2,1,0x57121d00,3,1,0x57227c10,2,1),
        Rule(0x57031a00,0,1,0x57227c10,0,0,0x57121d00,1,0,0x57227c10,0,0),
        //
        Rule(0x5C020A00,2,0,0x04006300,2,1,0x5C020A00,2,0,0x5C020A00,2,1),
        Rule(0x5C020A00,0,1,0x04006300,0,0,0x5C020A00,0,1,0x5C020A00,0,0),
        Rule(0x04006300,0,1,0x5C020A00,0,0,0x5C020A00,0,1,0x5C020A00,0,0),
        Rule(0x04006300,2,0,0x5C020A00,2,1,0x5C020A00,2,0,0x5C020A00,2,1),
      )
      for (rule <- adjacencies) {
        withClue(rule) {
          isRedundantAdjacency(rule, lookupRule).shouldBe(true)
        }
      }
    }
  }
}
