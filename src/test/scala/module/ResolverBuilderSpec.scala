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
  }
}
