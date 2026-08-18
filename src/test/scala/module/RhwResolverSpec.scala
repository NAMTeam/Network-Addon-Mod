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
    "use only expected 8th digits" in {
      val expected8thDigits = Set(0x0, 0x5, 0x9, 0xa)
      for ((tile, idTile) <- resolve.tileMap) {
        withClue((tile, idTile)) {
          expected8thDigits should contain (idTile.id % 0x10)
        }
      }
    }
    "handle OWR-4 shared-tile diagonals correctly" in {
      val crossingDirs = Seq(0x3000 -> Seq(EW, WE, NS, SN), 0x9000 -> Seq(NE, EN, SW, WS))
      for ((offset, dirs) <- crossingDirs; dir <- dirs) {
        (resolve(Owr4~SE & Owr4m~NW & L1Rhw4~dir).id & 0xffffff00).shouldBe(RhwResolver.rhwRangeId(L1Rhw4) + RhwResolver.rhwPieceId(Owr4) + offset)
        (resolve(Owr4~ES            & L1Rhw4~dir).id & 0xffffff00).shouldBe(RhwResolver.rhwRangeId(L1Rhw4) + RhwResolver.rhwPieceId(Owr4) + offset)
        (resolve(Owr4m~WN           & L1Rhw4~dir).id & 0xffffff00).shouldBe(RhwResolver.rhwRangeId(L1Rhw4) + RhwResolver.rhwPieceId(Owr4m) + offset)
        intercept[Exception](resolve(Owr4~SE & L1Rhw4~dir))  // not defined
        intercept[Exception](resolve(Owr4m~NW & L1Rhw4~dir))  // not defined
      }
    }
  }
}
