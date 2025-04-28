package com.sc4nam.module

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import io.github.memo33.metarules.meta._
import syntax._, Implicits._, RotFlip._, Network._, Flags._, NetworkProperties._

class ReverseResolverSpec extends AnyWordSpec with Matchers {

  "ReverseResolver" should {
    "find all possible orientations of a tile" in {
      val preimage = new ReverseResolver(Map((0x00004b00, ::(Road~NS, Nil))))
      for (rf <- RotFlip.values) {
        preimage(IdTile(0x00004b00, rf)).shouldBe(Seq[Tile](Road~NS * rf))
      }
    }
    "find multiple inequivalent tiles if they map to the same ID" in {
      val t1: Tile = Street~(2,2,2,2)
      val t2: Tile = Street~NS & Street~EW
      val preimage = new ReverseResolver(Map((0x05020700, ::(t1, t2 :: Nil))))
      preimage(0x05020700).toSet.shouldBe(Set(t1, t2))
      preimage(IdTile(0x05020700,0,0)).toSet.shouldBe(Set(t1, t2))
    }
  }
}
