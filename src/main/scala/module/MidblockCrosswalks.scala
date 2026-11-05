package com.sc4nam.module

import io.github.memo33.metarules.meta._
import syntax._, Network._, Flags._, RotFlip._, Implicits._

/** Mid-block crosswalks for Street/SAM, OWR and RHW-2 (RUL0 0x6109-0x6111 and 0x6180-0x6182).
  *
  * Each crosswalk tile is the real network crossed by a FLEX dummy Road with special flags, like the Road
  * crosswalks (0x6100-0x6102) use a dummy Street. The dummy Road gives the along-road edges a Road connection,
  * so the HAWK heads switch with the Road signal phase. Flags are (w, n, e, s) for a network running
  * north-south (rotation 0). The rules here convert the crosswalk tiles next to SAM, NWM OWR-1/OWR-3, OWR-5
  * and RHW-4.
  */
object MidblockCrosswalks {
  /** Street/SAM tiles: Size 1 at +0x000, Size 2 at +0x080 (the south tile turned by 180 degrees), Size 3
    * center at +0x100 and approach at +0x180 (the south one turned). */
  val streetBase = 0x5C5F1000
  val sams: List[(Network, Int)] = List(Sam1, Sam2, Sam3, Sam4, Sam5, Sam6, Sam7, Sam8, Sam9, Sam10, Sam11).zipWithIndex.map {
    case (sam, i) => (sam, streetBase + (i + 1) * 0x1000)
  }
  val nsFlags: List[(Int, IntFlags)] = List(
    0x000 -> (4,1,4,1),
    0x080 -> (2,4,2,1),
    0x100 -> (2,1,2,1),
    0x180 -> (4,4,4,1),
  )
  // the same tiles for a Street running west-east, plus their 180-degree turns
  val weFlags: List[IntFlags] = List(
    (1,4,1,4),
    (1,2,4,2), (4,2,1,2),
    (1,2,1,2),
    (1,4,4,4), (4,4,1,4),
  )
}

/** OWR tiles. They are directional (traffic runs north in rotation 0), so the dummy carries a 3 on one
  * side to make all four rotations distinct: Size 1 at +0x000, Size 2 departing at +0x100 and approach at +0x180,
  * Size 3 center at +0x200 and approach at +0x380 (Size 3 has no departing tile). On Size 2 the outer edge is a 3,
  * not a 4, so that it counts as a Road connection. The Maxis OWR tiles are at 0x5C5FD000, next to NWM OWR-1
  * and OWR-3 they convert to the tiles at 0x5C5FE000 and 0x5C5FF000.
  */
object OwrMidblockCrosswalks {
  val nets: List[(Network, Int)] = List(Onewayroad -> 0x5C5FD000, Owr1 -> 0x5C5FE000, Owr3 -> 0x5C5FF000)
  val nsFlags: List[(Int, IntFlags)] = List(
    0x000 -> (4,1,3,1),
    0x100 -> (2,3,3,1),
    0x180 -> (3,3,2,1),
    0x200 -> (2,1,3,1),
    0x380 -> (3,4,4,1),
  )
  // right-hand column of the Double OWR pieces: the 3 on the west side, facing the left column
  val nsFlagsR: List[(Int, IntFlags)] = List(
    0x000 -> (3,1,1,1),
    0x100 -> (3,3,0,1),
    0x180 -> (0,3,3,1),
    0x200 -> (3,1,0,1),
    0x380 -> (0,4,3,1),
  )
  private def rot1(f: IntFlags): IntFlags = (f._4, f._1, f._2, f._3)
  private def rot3(f: IntFlags): IntFlags = (f._2, f._3, f._4, f._1)
  // for a road running west-east: both quarter turns, as none of the tiles is symmetric
  val weFlags: List[IntFlags] = nsFlags.flatMap { case (_, f) => List(rot1(f), rot3(f)) }
  val weFlagsAll: List[IntFlags] = (nsFlags ++ nsFlagsR).flatMap { case (_, f) => List(rot1(f), rot3(f)) }

  /** NWM OWR-5: two OWR tiles, the east one turned by 180 degrees. Half A = Owr5~NS, half B = Owr5~SN. */
  val owr5A = 0x5C5B0000
  val owr5B = 0x5C5B1000
}

/** RHW-2 tiles (same layout as Street) and their RHW-4 conversion. RHW-4 is dragged once per direction, so
  * next to it the RHW-2 tiles convert to directional tiles laid out like the OWR ones (plus a Size 3
  * departing tile at +0x300, as the RHW-2 piece is two-way). Rhw4~NS has traffic running south.
  */
object RhwMidblockCrosswalks {
  val rhw2Base = 0x5C5F0000
  val rhw4Base = 0x5C5B8000
  // as Street, with a 3 instead of the outer 4 of Size 2
  val roadFlags: Map[Int, IntFlags] = MidblockCrosswalks.nsFlags.toMap + (0x080 -> (2,3,2,1))
  // RHW-4 tile offset -> (RHW-2 tile it comes from, traffic running north)
  val rhw4Tiles: List[(Int, Int, Boolean)] = List(
    (0x000, 0x000, true), (0x100, 0x080, true), (0x180, 0x080, false),
    (0x200, 0x100, true), (0x300, 0x180, true), (0x380, 0x180, false))
  def rhw4Tile(src: Int, up: Boolean): Tile = (if (up) Rhw4~SN else Rhw4~NS) & Road~roadFlags(src)
  // the LHD tiles are the RHD ones flipped. Size 2 is not mirror-symmetric, so its mirrored flags need rules of their own
  private def mirror(f: IntFlags): IntFlags = {
    val m = (x: Int) => if (x == 1) 3 else if (x == 3) 1 else x
    (m(f._3), m(f._2), m(f._1), m(f._4))
  }
  val weFlags: List[IntFlags] = (roadFlags.values.toList ++ roadFlags.values.map(mirror))
    .flatMap(f => List((f._4, f._1, f._2, f._3), (f._2, f._3, f._4, f._1))).distinct
}

class MidblockCrosswalkResolver extends IdResolver {
  import MidblockCrosswalks._
  val tileMap: scala.collection.Map[Tile, IdTile] = {
    val builder = new ResolverBuilder
    for ((net, base) <- (Street, streetBase) :: sams; (off, f) <- nsFlags) {
      builder.add(base + off, net~NS & Road~f)
    }
    for ((net, base) <- OwrMidblockCrosswalks.nets; (off, f) <- OwrMidblockCrosswalks.nsFlags) {
      builder.add(base + off, net~NS & Road~f)
    }
    for ((off, f) <- OwrMidblockCrosswalks.nsFlagsR) {
      builder.add(OwrMidblockCrosswalks.nets.head._2 + off, Onewayroad~NS & Road~f)
    }
    for ((off, f) <- OwrMidblockCrosswalks.nsFlags ++ OwrMidblockCrosswalks.nsFlagsR) {
      builder.add(OwrMidblockCrosswalks.owr5A + off, Owr5~NS & Road~f)
      builder.add(OwrMidblockCrosswalks.owr5B + off, Owr5~SN & Road~f)
    }
    for ((off, f) <- RhwMidblockCrosswalks.roadFlags) {
      builder.add(RhwMidblockCrosswalks.rhw2Base + off, Dirtroad~NS & Road~f)
    }
    for ((off, src, up) <- RhwMidblockCrosswalks.rhw4Tiles) {
      builder.add(RhwMidblockCrosswalks.rhw4Base + off, RhwMidblockCrosswalks.rhw4Tile(src, up))
    }
    builder.result()
  }
  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)
  def apply(t: Tile): IdTile = tileMap(t)
}

class MidblockCrosswalkRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator {
  import MidblockCrosswalks._
  def start(): Unit = {
    for ((sam, _) <- sams) {
      for (x <- weFlags) {
        Rules += sam~WE | (Street ~> sam)~WE & Road~x          // SAM into crosswalk
        Rules += sam~WE & Road~x | (Street ~> sam)~WE          // crosswalk out to Street
        for (y <- weFlags) {
          Rules += sam~WE & Road~x | (Street ~> sam)~WE & Road~y   // multi-tile sizes, adjacent pieces
        }
      }
      createRules()
    }
    for (nwm <- List(Owr1, Owr3)) {
      for (x <- OwrMidblockCrosswalks.weFlags) {
        Rules += nwm~WE | (Onewayroad ~> nwm)~WE & Road~x
        Rules += nwm~WE & Road~x | (Onewayroad ~> nwm)~WE
        for (y <- OwrMidblockCrosswalks.weFlags) {
          Rules += nwm~WE & Road~x | (Onewayroad ~> nwm)~WE & Road~y
        }
      }
      createRules()
    }
    for (dir <- List(WE, EW)) {   // OWR-5 is asymmetric: both halves
      for (x <- OwrMidblockCrosswalks.weFlagsAll) {
        Rules += Owr5~dir | (Onewayroad ~> Owr5)~dir & Road~x
        Rules += Owr5~dir & Road~x | (Onewayroad ~> Owr5)~dir
        for (y <- OwrMidblockCrosswalks.weFlagsAll) {
          Rules += Owr5~dir & Road~x | (Onewayroad ~> Owr5)~dir & Road~y
        }
      }
    }
    createRules()
    for (dir <- List(WE, EW)) {   // RHW-4 is directional: both directions
      for (x <- RhwMidblockCrosswalks.weFlags) {
        Rules += Rhw4~dir | (Dirtroad ~> Rhw4)~dir & Road~x
        Rules += Rhw4~dir & Road~x | (Dirtroad ~> Rhw4)~dir
        for (y <- RhwMidblockCrosswalks.weFlags) {
          Rules += Rhw4~dir & Road~x | (Dirtroad ~> Rhw4)~dir & Road~y
        }
      }
    }
    createRules()
  }
}

object CompileMidblockCrosswalkCode extends AbstractMain {
  lazy val resolve: IdResolver = new MidblockCrosswalkResolver orElse new SamResolver orElse new RhwResolver orElse new MiscResolver
  val generator = new MidblockCrosswalkRuleGenerator(_)
  lazy val file = new java.io.File("Controller/RUL2/15_PedestrianRevolution/Sec15b_MidblockCrosswalks/Sec15b_MidblockCrosswalks_MetaGenerated.txt")
}
