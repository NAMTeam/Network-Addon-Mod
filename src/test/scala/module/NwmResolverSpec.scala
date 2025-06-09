package com.sc4nam.module

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import io.github.memo33.metarules.meta._
import syntax._, Implicits._, RotFlip._, Network._, Flags._, NetworkProperties._

class NwmResolverSpec extends AnyWordSpec with Matchers {

  "NwmResolver" should {
    val resolve = new NwmResolver
    "be compatible with original O×O IID scheme" in {
      for (rf <- RotFlip.values) {
        resolve((Ard3~SN & Rd4~EW) * rf).rf shouldBe rf
        if (!rf.flipped) {
          resolve((Ave2~SN & Rd4~EW) * rf).rf shouldBe rf
          resolve((Rail~SN & Rd4~EW) * rf).rf shouldBe rf
          resolve((Ard3~SN & Rail~EW) * rf).rf shouldBe rf
        }
      }
    }
    "handle TLA networks correctly" in {
      val expected = Seq[(Tile, IdTile)](
        ((Tla5~EW).projectLeft  & Avenue~NS) -> IdTile(0x51101300, R0F0, nonMirroredOnly),  // left-turn  lane in RHD
        ((Tla5~EW).projectRight & Avenue~NS) -> IdTile(0x71101300, R0F0, mirroredOnly),     // right-turn lane in LHD
        ((Tla5~EW).projectLeft  & Avenue~SN) -> IdTile(0x71101300, R0F1, mirroredOnly),     // left-turn  lane in RHD
        ((Tla5~EW).projectRight & Avenue~SN) -> IdTile(0x51101300, R0F1, nonMirroredOnly),  // right-turn lane in LHD
        ((Tla3~EW).projectLeft  & Road~NS) -> IdTile(0x51001100, R0F0, nonMirroredOnly),
        ((Tla3~EW).projectRight & Road~NS) -> IdTile(0x51001100, R0F0, nonMirroredOnly),
        ((Tla5~NS).projectLeft  & Avenue~SW) -> IdTile(0x51105300, R0F0, nonMirroredOnly),  // left-turn  lane in RHD
        ((Tla5~NS).projectRight & Avenue~SW) -> IdTile(0x71105300, R0F0, mirroredOnly),     // right-turn lane in LHD
        ((Tla5~NS).projectLeft  & Avenue~SharedDiagLeft) -> IdTile(0x51105309, R0F0, nonMirroredOnly),  // left-turn  lane in RHD
        ((Tla5~NS).projectRight & Avenue~SharedDiagLeft) -> IdTile(0x71105309, R0F0, mirroredOnly),     // right-turn lane in LHD
        ((Tla5~SN).projectLeft  & Avenue~SW) -> IdTile(0x51105380, R0F0, nonMirroredOnly),  // left-turn  lane in RHD
        ((Tla5~SN).projectRight & Avenue~SW) -> IdTile(0x71105380, R0F0, mirroredOnly),     // right-turn lane in LHD
        ((Tla5~SN).projectLeft  & Avenue~SharedDiagLeft) -> IdTile(0x51105309, R2F0, nonMirroredOnly),  // left-turn  lane in RHD (duplicate)
        ((Tla5~SN).projectRight & Avenue~SharedDiagLeft) -> IdTile(0x71105309, R2F0, mirroredOnly),     // right-turn lane in LHD (duplicate)
      ).flatMap { case (tile, idTile) => Seq(R0F0, R1F0, R2F0, R3F0).map(rf => (tile * rf, idTile * tile.symmetries.reduceLeftCoset(rf))) }
      for ((tile, idTile) <- expected) {
        val resolved = resolve(tile)
        (tile, resolved, resolved.mappedRepr(tile.representations)) shouldBe (tile, idTile, idTile.mappedRepr(tile.representations))
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
    "handle OWR-4 shared-tile diagonals correctly" in {
      for (dir <- Seq(EW, WE, NS, SN, NE, SW, SharedDiagLeft)) {
        (resolve(Owr4~SE & Owr4m~NW & Avenue~dir).id & 0xffff0000).shouldBe(NwmResolver.nwmRangeId(Owr4))
        (resolve(Owr4~ES            & Avenue~dir).id & 0xffff0000).shouldBe(NwmResolver.nwmRangeId(Owr4))
        (resolve(Owr4m~WN           & Avenue~dir).id & 0xffff0000).shouldBe(NwmResolver.nwmRangeId(Owr4m))
        intercept[Exception](resolve(Owr4~SE & Avenue~dir))  // not defined
        intercept[Exception](resolve(Owr4m~NW & Avenue~dir))  // not defined
      }
      for (dir <- Seq(EW, WE, NS, SN, NE, EN, SW, WS)) {
        Set(Ard3, Owr4 ).map(NwmResolver.nwmRangeId) should contain (resolve(Owr4~SE & Owr4m~NW & Ard3~dir).id & 0xffff0000)
        Set(Ard3, Owr4 ).map(NwmResolver.nwmRangeId) should contain (resolve(Owr4~ES            & Ard3~dir).id & 0xffff0000)
        Set(Ard3, Owr4m).map(NwmResolver.nwmRangeId) should contain (resolve(Owr4m~WN           & Ard3~dir).id & 0xffff0000)
        intercept[Exception](resolve(Owr4~SE & Ard3~dir))  // not defined
        intercept[Exception](resolve(Owr4m~NW & Ard3~dir))  // not defined
      }
    }
  }
}
