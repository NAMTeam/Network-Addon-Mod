package metarules.meta

// The tests in this file have been moved from the upstream metarules package to
// the NAM repository as they depend on a proper rule resolver which only exists
// here in the NAM repository.


import scala.collection.immutable.StringOps
import org.scalatest.{WordSpec, Matchers}
import Implicits._
import Group._, RotFlip._, SymGroup._, Network._, Flags._
import RuleTransducer._
import metarules.module


class RuleTransducerSpec extends WordSpec with Matchers {

  "isReachable" should {
    "work properly" in {
      isReachable(Dih2A.quotient, R0F0, Dih2A.quotient, R0F0) should be (true)
      isReachable(Dih2A.quotient, R0F0, Dih2A.quotient, R2F0) should be (false)
      isReachable(Dih2A.quotient, R1F0, Dih4.quotient, R0F0) should be (true)
      isReachable(Dih2A.quotient, R1F0, Dih4.quotient, R1F0) should be (true)
      isReachable(Dih2A.quotient, R3F0, Dih4.quotient, R2F0) should be (true)
      isReachable(Dih2A.quotient, R3F0, Dih4.quotient, R3F0) should be (true)
      isReachable(Dih2A.quotient, R3F0, Dih4.quotient, R0F0) should be (false)
      isReachable(Dih2A.quotient, R3F0, Dih4.quotient, R1F0) should be (false)
      isReachable(Dih2A.quotient, R1F0, Dih4.quotient, R2F0) should be (false)
      isReachable(Dih2A.quotient, R1F0, Dih4.quotient, R3F0) should be (false)
    }
  }

  "hasSmallerEquivRepr" should {
    "work for tiles with same IID" in {
      def count(ag: GroupElement, bg: GroupElement) {
        val l1 = Seq(ag, ag * R2F1, bg * R0F1, bg * R2F0)
        val l2 = Seq(bg, bg * R2F1, ag * R0F1, ag * R2F0)
        withClue(ag + " " + bg + ":") {
          (l1 zip l2).toSet map { tup: Tuple2[GroupElement, GroupElement] =>
            !hasSmallerEquivRepr(42, tup._1, 42, tup._2)
          } count (_ == true) should be (1)
        }
      }
      val vs = Seq(R0F0, R1F0, R2F0, R3F0, R0F1, R1F1, R2F1, R3F1)
      for (ag <- vs; bg <- vs) {
        count(ag, bg)
      }
    }
    "be correct for tiles with same IID (TODO FIX THIS!)" ignore {
      RuleTransducer(Road~NS | Road~>Road~WE).toSeq.size should be (RuleTransducer(Ave2~NS | Road~>Ave2~WE).toSeq.size)
    }
  }

  "possibleMapOrientation" should {
    "pass test cases" in {
      possibleMapOrientation(Dih4.quotient, R0F0, Dih4.quotient, R0F0) should be (Set(R0F0))
      possibleMapOrientation(Dih4.quotient, R1F0, Dih4.quotient, R1F0) should be (Set(R1F0))
      possibleMapOrientation(Dih4.quotient, R0F1, Dih4.quotient, R0F1) should be (Set(R0F1))

      possibleMapOrientation(Dih4.quotient, R0F0, Dih2A.quotient, R0F0) should be (Set(R0F0))
      possibleMapOrientation(Dih4.quotient, R0F0, Dih2A.quotient, R1F0) should be (Set(R0F0))
      possibleMapOrientation(Dih4.quotient, R0F0, Dih2A.quotient, R2F0) should be ('empty)
      possibleMapOrientation(Dih4.quotient, R0F0, Dih2A.quotient, R0F1) should be ('empty)

      possibleMapOrientation(Dih2A.quotient, R0F0, Dih2A.quotient, R0F0) should be (Set(R0F0, R3F0))
      possibleMapOrientation(Dih2A.quotient, R1F0, Dih2A.quotient, R1F0) should be (Set(R0F0, R1F0))
      possibleMapOrientation(Dih2A.quotient, R0F0, Dih2A.quotient, R1F0) should be (Set(R0F0))
      possibleMapOrientation(Dih2A.quotient, R1F0, Dih2A.quotient, R0F0) should be (Set(R0F0))

      possibleMapOrientation(Dih2A.quotient, R0F0, Cyc2B.quotient, R0F0) should be (Set(R0F0, R3F0))
      possibleMapOrientation(Dih2A.quotient, R0F0, Cyc2B.quotient, R1F0) should be (Set(R0F0, R3F0))

      possibleMapOrientation(Cyc2B.quotient, R0F0, Cyc2B.quotient, R2F0) should be (Set(R0F0, R1F0, R2F0, R3F0))
      possibleMapOrientation(Cyc2B.quotient, R0F0, Cyc2B.quotient, R1F0) should be (Set(R0F0, R1F0, R2F0, R3F0))

      for (sg <- SymGroup.values; h <- RotFlip.values) {
        possibleMapOrientation(sg.quotient, R0F0, Cyc1.quotient, h) should be (sg.quotient.map((rf: RotFlip) => R0F0 / rf))
      }
    }
  }

  class DummyIdSymTile(override val id: Int, override val rf: RotFlip, override val symmetries: SymGroup) extends IdSymTile(null, IdTile(0, null))
  def DIST(id: Int, rf: RotFlip, sg: SymGroup) = new DummyIdSymTile(id, rf, sg)

  implicit val resolver = new module.RhwResolver orElse new module.NwmResolver orElse new module.MiscResolver
  val tileOrientationCache = collection.mutable.Map.empty[Int, Set[RotFlip]]
  implicit val context = RuleTransducer.Context(resolver, tileOrientationCache, module.MirrorVariants.preprocessor)
  "RuleTransducer" should {
    val c = DIST(0x3000, R0F0, Cyc1)
    val d = DIST(0x4000, R0F0, Cyc1)
    val s = Seq(
      (DIST(0x1000, R0F0, Dih2A), DIST(0x2000, R0F0, Dih2A), 2),
      (DIST(0x1000, R0F0, Dih2A), DIST(0x2000, R1F0, Dih2A), 4),
      (DIST(0x1000, R0F0, Dih2A), DIST(0x1000, R0F0, Dih2A), 1),

      (DIST(0x1000, R0F0, Dih2A), DIST(0x2000, R0F0, Cyc1), 4),

      (DIST(0x1000, R0F0, Cyc1),  DIST(0x2000, R0F0, Cyc1), 1),
      (DIST(0x1000, R0F0, Cyc1),  DIST(0x1000, R0F0, Cyc1), 1),

      (DIST(0x1000, R0F0, Cyc2B), DIST(0x2000, R0F0, Cyc2B), 2),
      (DIST(0x1000, R0F0, Cyc2B), DIST(0x1000, R0F0, Cyc2B), 1),
      (DIST(0x1000, R0F0, Dih2A), DIST(0x2000, R0F0, Cyc2B), 4),

      (DIST(0x1000, R0F0, Dih2A), DIST(0x2000, R0F0, Dih4), 4),
      (DIST(0x1000, R0F0, Dih4),  DIST(0x2000, R0F0, Dih4), 4)
    )

    "generate correct number of rules" in {
      for ((a, b, x) <- s) {
        val rules = createRules(Rule(a,b,c,d), tileOrientationCache).toIterable
        withClue(a.symmetries + " " + b.symmetries + "\n" + rules.mkString("\n")) {
          rules should have size (x)
        }
      }
    }

////    "rotate output tiles correctly" ignore {
////      for ((a, b, _) <- s) {
////        val rules = autoRule(a,b,c,d)
////        (rules map { r =>
////          val ag = RotFlip(r.a.asInstanceOf[IIDTile].rot, r.a.asInstanceOf[IIDTile].flip)
////          val cg = RotFlip(r.c.asInstanceOf[IIDTile].rot, r.c.asInstanceOf[IIDTile].flip)
////          cg / ag
////        }).toSet should have size 1
////        (rules map { r =>
////          val bg = RotFlip(r.b.asInstanceOf[IIDTile].rot, r.b.asInstanceOf[IIDTile].flip)
////          val dg = RotFlip(r.d.asInstanceOf[IIDTile].rot, r.d.asInstanceOf[IIDTile].flip)
////          dg / bg
////        }).toSet should have size 1
////      }
////    }

    "produce proper output orientation for orth asymm overrides" in {
      val rule = Mis~EW | Dirtroad~EW | Mis~EW | Mis~EW
      rule(0).symmetries should be (Cyc2B)
      rule(1).symmetries should be (Dih2A)
      rule(3).symmetries should be (Cyc2B)
      for (r <- RuleTransducer(rule)) withClue(r) {
        r(2).rf should be (r(0).rf)
        r(3).rf should be (r(0).rf)
      }
    }

    "produce proper output orientation for orth symm overrides" in {
      val rule = L2Rhw2~WE | Dirtroad~WE & L1Rhw2~NS | L2Rhw2~WE | L2Rhw2~WE & L1Rhw2~NS
      rule foreach (_.symmetries should be (Dih2A))
      for (r <- RuleTransducer(rule)) withClue(r) {
        r(2).rf should be (r(0).rf)
        r(3).rf should be (r(0).rf)
      }
    }

    def parseRules(text: String): Seq[Rule[IdTile]] = {
      new StringOps(text).lines.map(_.split(";", 2)(0).trim).filterNot(_.isEmpty).map[Rule[IdTile]] { line =>
        line.split(",|=").grouped(3).toSeq.map(tup =>
          IdTile(java.lang.Long.decode(tup(0)).toInt, RotFlip(tup(1).toInt, tup(2).toInt))
        )(scala.collection.breakOut)
      }
    }.toSeq

    def compareRules(a: Seq[Rule[IdTile]], b: Seq[Rule[IdTile]]) {
      a should have size b.size
      val sa = (a map (_.toString)).sorted
      val sb = (b map (_.toString)).sorted
      for ((r1, r2) <- sa zip sb) withClue(r1 + " <- auto generated\n" + r2 + " <- desired\n") {
        r1 should be (r2)
      }
    }

    "produce proper output for symm +intersections" in {
      val rules = Seq(
        L1Rhw2~WE               | Dirtroad~WE & Dirtroad~NS | L1Rhw2~WE               | L1Rhw2~WE & Dirtroad~NS,
        L1Rhw2~WE & Dirtroad~NS | Dirtroad~WE               | L1Rhw2~WE & Dirtroad~NS | L1Rhw2~WE,
        L1Rhw2~WE               | Dirtroad~WE & L1Rhw2~NS   | L1Rhw2~WE               | L1Rhw2~WE & L1Rhw2~NS,
        L1Rhw2~WE & L1Rhw2~NS   | Dirtroad~WE               | L1Rhw2~WE & L1Rhw2~NS   | L1Rhw2~WE )

      // to be sure, check symmetries first
      rules(0)(0).symmetries should be (Dih2A)
      rules(0)(1).symmetries should be (Dih4)
      rules(0)(3).symmetries should be (Dih2A)
      rules(1)(1).symmetries should be (Dih2A)
      rules(2)(1).symmetries should be (Dih2A)
      rules(2)(3).symmetries should be (Dih4)

      val autoRules = rules.flatMap(RuleTransducer(_)).toSeq

      val matchRules = parseRules("""
        ;single in
        0x57100000,1,0,0x57001A00,0,0=0x57100000,1,0,0x57101A00,1,0
        0x57100000,1,0,0x57001A00,1,0=0x57100000,1,0,0x57101A00,1,0
        0x57100000,3,0,0x57001A00,2,0=0x57100000,3,0,0x57101A00,3,0
        0x57100000,3,0,0x57001A00,3,0=0x57100000,3,0,0x57101A00,3,0
        ;single out
        0x57101A00,1,0,0x57000000,1,0=0x57101A00,1,0,0x57100000,1,0
        0x57101A00,3,0,0x57000000,3,0=0x57101A00,3,0,0x57100000,3,0

        ;double in
        0x57100000,1,0,0x57101A00,2,0=0x57100000,1,0,0x57101A10,1,0
        0x57100000,3,0,0x57101A00,0,0=0x57100000,3,0,0x57101A10,3,0
        ;double out
        0x57101A10,1,0,0x57000000,1,0=0x57101A10,1,0,0x57100000,1,0
        0x57101A10,3,0,0x57000000,3,0=0x57101A10,3,0,0x57100000,3,0
        ;double stability
        0x57101A10,0,0,0x57000000,1,0=0x57101A10,0,0,0x57100000,1,0
        0x57101A10,2,0,0x57000000,3,0=0x57101A10,2,0,0x57100000,3,0
        0x57100000,1,0,0x57101A00,0,0=0x57100000,1,0,0x57101A10,0,0
        0x57100000,3,0,0x57101A00,2,0=0x57100000,3,0,0x57101A10,2,0
      """)
      compareRules(autoRules, matchRules)
    }
  }

  "possibleMapOrientation" should {
    "coincide with isReachable" in {
      for (aRepr <- QuotientGroup.values; bRepr <- QuotientGroup.values;
           ag <- RotFlip.values; bg <- RotFlip.values if isReachable(aRepr, ag, bRepr, bg)) {
        possibleMapOrientation(aRepr, ag, bRepr, bg) should not be ('empty)
      }
      val aRepr = Set(R0F1, R1F1, R2F1, R3F1); val bRepr = QuotientGroup.Cyc2A
      isReachable(aRepr, R1F0, bRepr, R1F0) should be (false)
      possibleMapOrientation(aRepr, R1F0, bRepr, R1F0) should be ('empty)
    }
  }

  "preprocessor" should {
    "produce expected number of rules for Tla3" in {
      val orth: Seq[Rule[SymTile]] = context.preprocess( Tla3~WE | (Road ~> Tla3)~WE ).toSeq
      orth should have size (1)
      orth.asInstanceOf[Seq[Rule[Tile]]].exists(_.exists(_.segs.exists(_.flags.exists(_ == Flag.RightHeadedBi.Orth)))) should be (false)
      createRules(orth.head.map(_.toIdSymTile), tileOrientationCache).toSeq should have size (2)

      context.preprocess( Tla3~WE & Road~NS | (Road ~> Tla3)~WE ).toSeq should have size (1)
      val diag = context.preprocess( Tla3~WE & Road~WS | (Road ~> Tla3)~WE ).toSeq
      diag should have size (2)
      for (r <- diag) {
        createRules(r.map(_.toIdSymTile), tileOrientationCache).toSeq should have size (2)
      }
    }
  }

  def makeTileLeft(t: Tile): Tile = Tile(t.segs map (s => if (!s.network.isTla) s else s.copy(flags = s.flags.makeLeftHeaded)))
  def makeTileRight(t: Tile): Tile = Tile(t.segs map (s => if (!s.network.isTla) s else s.copy(flags = s.flags.makeRightHeaded)))
  "Resolver" should {
    "resolve left/right-headed TLAs correctly" in {
      val tiles = Seq[Tile]( Tla3~WE & Road~ES, Tla3~WE & Ard3~ES, Tla3~WE & Tla3~ES )
      val tile2 = Seq[Tile]( Tla3~WE, Tla3~WE & Road~NS, Tla3~WE & Ard3~NS, Tla3~WE & Tla3~NS )
      for (t <- tiles) {
        resolver(makeTileLeft(t)) should not be resolver(makeTileRight(t))
      }
      for (t <- tile2) {
        resolver(makeTileLeft(t)) should be (resolver(makeTileRight(t)))
      }
    }
    "handle flipped left/right headed TLAs correctly" in {
      val (t1, t2) = ( Tla3~WE & Road~ES, Tla3~WE & Road~WS )
      resolver(makeTileLeft(t1)).id should not be (resolver(makeTileLeft(t2)).id)
      for ((t, i) <- Seq(t1, t2).zipWithIndex) {
        makeTileLeft(t).toIdSymTile.repr.filter(_.flipped ^ (i!=0)) should be ('empty)
        makeTileRight(t).toIdSymTile.repr.filter(!_.flipped ^ (i!=0)) should be ('empty)
      }
    }
    "find RHS for TLA" in {
      val rule = (Tla3~WE | (Road ~> Tla3)~(2,0,11,0)) map makeTileLeft map (_.toIdSymTile)
      possibleMapOrientation(Set(R0F0, R1F0), R3F0/R2F1, QuotientGroup.Dih4, R1F1/R2F1) should not be ('empty)
      createRules(rule, tileOrientationCache)
    }
    "resolve diagonal TLA intersections" in {
      val t1 = makeTileLeft(Tla3~ES & Road~WS)
      val t2 = makeTileRight(Tla3~ES & Road~WS)
      val t3 = makeTileLeft(Tla3~WS & Road~ES)
      resolver(t3) * R0F1 should be (resolver(t2))
      resolver(t1).id should not be (resolver(t2).id)
      resolver(makeTileLeft(Tla3~ES & Tla3~WS)) should be (resolver(makeTileRight(Tla3~ES & Tla3~WS)))
    }
  }
}
