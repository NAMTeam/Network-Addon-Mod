package com.sc4nam.module

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import io.github.memo33.metarules.meta._
import syntax._, Implicits._, RotFlip._, Network._, Flags._, NetworkProperties._

class ResolverBuilderSpec extends AnyWordSpec with Matchers {

  "ResolverBuilder" should {
    "handle mirror variants as expected" in {
      val builder = new ResolverBuilder()
      builder.addOne((Road~NS).projectLeft & Rail~NE, IdTile(0x03010200, R0F0, nonMirroredOnly))
      builder.addOne((Road~NS).projectRight & Rail~NE, IdTile(0x03020500, R0F0, mirroredOnly))
      builder.addOne((Road~WN).projectLeft & Rail~NS, IdTile(0x03020100, R0F0, nonMirroredOnly))
      builder.addOne((Road~WN).projectRight & Rail~NS, IdTile(0x03020400, R0F0, mirroredOnly))
      builder.addOne((Road~ES).projectLeft & Rail~NE, IdTile(0x03020200, R0F0, nonMirroredOnly))
      builder.addOne((Road~NW).projectRight & Rail~NE, IdTile(0x03020300, R0F0, nonMirroredOnly))

      val m = builder.result()
      val resolve = new MiscResolver
      m.foreach { case (tile, idTile) =>
        val resolved = resolve(tile)
        (tile, idTile, idTile.mappedRepr(tile.representations)) shouldBe (tile, resolved, resolved.mappedRepr(tile.representations))
      }
    }
    "show correctly trimmed stack trace for duplicate IDs" in {
      val builder = new ResolverBuilder()
      builder.add(Road~(0,0,0,0), 0x00000100)
      val thrown = intercept[ResolverBuilder.DuplicateId] {
        builder.add(Road~(0,0,0,0), 0x00000100)
      }
      thrown.getStackTrace.head.getFileName.shouldBe("ResolverBuilderSpec.scala")
    }
    "allow defining tiles that only appear mirrored (in R-spin variants)" in {
      // actually this came up with midblock crosswalks, not TLAs
      val builder = new ResolverBuilder()
      builder.addOne((Tla5~EW).projectLeft  & Rhw4~NS, (IdTile(0x180, R0F0), IdTile(0x100, R0F1)))
      builder.addOne((Tla5~EW).projectRight & Rhw4~NS, (IdTile(0x100, R0F1), IdTile(0x180, R0F0)))
      val m = builder.result()
      val expected0 = Map(
        ((Tla5~EW).projectLeft  & Rhw4~NS) -> IdTile(0x180, R0F0),
        ((Tla5~EW).projectRight & Rhw4~NS) -> IdTile(0x100, R0F1),  // i.e. R-projection appears flipped only
        ((Tla5~EW).projectLeft  & Rhw4~SN) -> IdTile(0x100, R0F0),
        ((Tla5~EW).projectRight & Rhw4~SN) -> IdTile(0x180, R0F1),  // i.e. R-projection appears flipped only
      )
      m.filter { (t, i) => i.rf.rot == 0 }.shouldBe(expected0)
      m.shouldBe(expected0.flatMap((t, i) => Seq(R0F0, R1F0, R2F0, R3F0).map(rf => (t * rf, i * rf))))
    }
  }
}
