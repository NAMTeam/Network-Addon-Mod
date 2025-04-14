package com.sc4nam.module

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import io.github.memo33.metarules.meta._
import syntax._, Implicits._, RotFlip._, Network._, Flags._, NetworkProperties._

class RhwResolverSpec extends AnyWordSpec with Matchers {

  "RhwResolver" should {
    val resolve = new RhwResolver
    "have correctly initialized IIDs" in {
      assert {
        Network.values.forall { n =>
          (RhwResolver.rhwPieceId.contains(n) || !RhwResolver.rhwRangeId.contains(n)) &&
            (RhwResolver.rhwPieceId.get(n) forall { id => id / 0x10 % 0x10 == n.height }) &&
            (RhwResolver.rhwRangeId.get(n) forall { id => id / 0x100000 % 0x10 == n.height && id / 0x10000000 == 5 })
        }
      }
    }
    "be a 1-to-1 correspondence (bijective)" in {
      val reverseTileMap = resolve.tileMap.map { case (a, b) => (b, a) }
      for ((tile, idTile) <- resolve.tileMap) {
        val tile2 = reverseTileMap(idTile)
        if (!tile.segs.exists(_.network.isTla)) {
          tile2.shouldBe(tile)
        } else {
          unprojectTla(tile2).shouldBe(unprojectTla(tile))
        }
      }
    }
  }
}
