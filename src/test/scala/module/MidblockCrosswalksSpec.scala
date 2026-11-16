package com.sc4nam.module

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import io.github.memo33.metarules.meta._
import syntax._, Implicits._, RotFlip._, Network._, Flags._, NetworkProperties._

class MidblockCrosswalksSpec extends AnyWordSpec with Matchers {

  "MidblockCrosswalkResolver" should {
    "use smallest possible rotations (preserving mirroring) according to RotFlip order" in {
      val tileMap = (new MidblockCrosswalkResolver).tileMap
      val values = tileMap.values.toSet
      values.size.shouldBe(tileMap.size)
      (R3F1 < R1F1).shouldBe(true)
      for ((tile, idTile) <- tileMap) {
        for (rf <- tile.symmetries) {
          if (!rf.flipped) {  // preserve mirroring, as the resolver specifically maps all LHD tiles to flipped IdTiles
            val idTile2 = idTile.copy(rf = idTile.rf * rf)
            if (idTile2.rf < idTile.rf) {
              withClue(s"$tile represented by $idTile could be represented by smaller $idTile2:") {
                values.contains(idTile2).shouldBe(true)
              }
            }
          }
        }
      }
    }
    "produce expected overrides for RHD/LHD specific RHW-4 crosswalks" in {
      val resolve = new MiscResolver orElse new RhwResolver orElse new MidblockCrosswalkResolver
      implicit val context = RuleTransducer.Context(resolve, preprocess = MirrorVariants.preprocessor)
      Seq(WE, EW).flatMap { dir =>
        RuleTransducer(Rhw4~dir | (Dirtroad~>Rhw4)~dir & Crosswalk1~NS & CrosswalkRhd~(2,2,2,2))
      } should contain theSameElementsAs (
        """
        0x57030000,3,0,0x5C5F0000,1,0=0x57030000,3,0,0x5C5B8000,1,0
        0x57030000,3,0,0x5C5F0000,3,0=0x57030000,3,0,0x5C5B8000,1,0
        0x57030000,3,0,0x5C5F0000,1,1=0x57030000,3,0,0x5C5B8000,1,1
        0x57030000,3,0,0x5C5F0000,3,1=0x57030000,3,0,0x5C5B8000,1,1
        0x57030000,1,0,0x5C5F0000,3,1=0x57030000,1,0,0x5C5B8000,3,1
        0x57030000,1,0,0x5C5F0000,1,1=0x57030000,1,0,0x5C5B8000,3,1
        0x57030000,1,0,0x5C5F0000,3,0=0x57030000,1,0,0x5C5B8000,3,0
        0x57030000,1,0,0x5C5F0000,1,0=0x57030000,1,0,0x5C5B8000,3,0
        """.trim.linesIterator.map(Rul2Model.parseRule(_).get).toSeq
      )
    }
  }
}
