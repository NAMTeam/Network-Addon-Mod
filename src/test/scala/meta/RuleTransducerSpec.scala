package io.github.memo33.metarules.meta

import scala.collection.immutable.StringOps
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import com.sc4nam.module, module.syntax._, Implicits._
import com.sc4nam.module.{NetworkProperties => NP}
import group._, RotFlip._, SymGroup._, Network._, Flags._
import RuleTransducer._

class RuleTransducerSpec extends AnyWordSpec with Matchers {

  val resolver = new module.RhwResolver orElse new module.NwmResolver orElse new module.MiscResolver
  val context = RuleTransducer.Context(resolver, preprocess = module.MirrorVariants.preprocessor)

  "preprocessor" should {
    "produce expected number of rules for Tla3" in {
      val orth: Seq[Rule[SymTile]] = context.preprocess( Tla3~WE | (Road ~> Tla3)~WE ).toSeq
      orth should have size (1)
      orth.asInstanceOf[Seq[Rule[Tile]]].exists(_.exists(_.segs.exists(s => s.flags.manifest == Flag.RightSpinBi && s.flags.exists(_ == 2)))) should be (false)
      createRules(orth.head.map(_.toIdSymTile(resolver)), context.tileOrientationCache.cache, context.tileOrientationCache.accum).toSeq should have size (2)

      context.preprocess( Tla3~WE & Road~NS | (Road ~> Tla3)~WE ).toSeq should have size (1)
      val diag = context.preprocess( Tla3~WE & Road~WS | (Road ~> Tla3)~WE ).toSeq
      diag should have size (2)
      for (r <- diag) {
        createRules(r.map(_.toIdSymTile(resolver)), context.tileOrientationCache.cache, context.tileOrientationCache.accum).toSeq should have size (2)
      }
    }
  }

  "Resolver" should {
    "resolve left/right-spinned TLAs correctly" in {
      val tiles = Seq[Tile]( Tla3~WE & Road~ES, Tla3~WE & Ard3~ES, Tla3~WE & Tla3~ES )
      val tile2 = Seq[Tile]( Tla3~WE, Tla3~WE & Road~NS, Tla3~WE & Ard3~NS, Tla3~WE & Tla3~NS )
      for (t <- tiles) {
        resolver(NP.projectTlaLeft(t)) should not be resolver(NP.projectTlaRight(t))
      }
      for (t <- tile2) {
        resolver(NP.projectTlaLeft(t)) should be (resolver(NP.projectTlaRight(t)))
      }
    }
    "handle flipped left/right-spinned TLAs correctly" in {
      val (t1, t2) = ( Tla3~WE & Road~ES, Tla3~WE & Road~WS )
      resolver(NP.projectTlaLeft(t1)).id should not be (resolver(NP.projectTlaLeft(t2)).id)
      for ((t, i) <- Seq(t1, t2).zipWithIndex) {
        NP.projectTlaLeft(t).toIdSymTile(resolver).repr.filter(_.flipped ^ (i!=0)) should be (Symbol("empty"))
        NP.projectTlaRight(t).toIdSymTile(resolver).repr.filter(!_.flipped ^ (i!=0)) should be (Symbol("empty"))
      }
    }
    "find RHS for TLA" in {
      val rule = (Tla3~WE | (Road ~> Tla3)~(2,0,11,0)) map NP.projectTlaLeft map (_.toIdSymTile(resolver))
      possibleMapOrientation(Set(R0F0, R1F0), R3F0/R2F1, Quotient.Dih4, R1F1/R2F1) should not be (Symbol("empty"))
      createRules(rule, context.tileOrientationCache.cache, context.tileOrientationCache.accum)
    }
    "resolve diagonal TLA intersections" in {
      val t1 = NP.projectTlaLeft(Tla3~ES & Road~WS)
      val t2 = NP.projectTlaRight(Tla3~ES & Road~WS)
      val t3 = NP.projectTlaLeft(Tla3~WS & Road~ES)
      resolver(t3) * R0F1 should be (resolver(t2))
      resolver(t1).id should not be (resolver(t2).id)
      resolver(NP.projectTlaLeft(Tla3~ES & Tla3~WS)) should be (resolver(NP.projectTlaRight(Tla3~ES & Tla3~WS)))
    }
  }
}
