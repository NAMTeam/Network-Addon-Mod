package com.sc4nam.module

import io.github.memo33.metarules.meta._
import syntax._, Network._, Flags._, RotFlip._, Implicits._

/** Mid-block crosswalks for Street/SAM, OWR and RHW-2 (RUL0 0x6109-0x6111 and 0x6180-0x6182), converting
  * next to SAM, OWR-1/OWR-3, OWR-5 and RHW-4. Tiles are for a road running north-south, pieces list their
  * tiles from north to south.
  */
object MidblockCrosswalks {
  /** The models are not mirror-symmetric (signs stand right of the traffic); only rotations keep this marker. */
  val rhd: Segment = CrosswalkRhd~(2,2,2,2)

  // Street, SAM and RHW-2 (two-way)
  val size1:  Tile = Crosswalk1~WE  & rhd
  val size2:  Tile = Crosswalk2~EW  & rhd  // outer edge north
  val size3m: Tile = Crosswalk3m~WE & rhd
  val size3:  Tile = Crosswalk3~EW  & rhd  // outer edge north
  val twoWayTiles: Seq[(Int, Tile)] = Seq(0x000 -> size1, 0x080 -> size2, 0x100 -> size3m, 0x180 -> size3)
  val twoWayPieces: Seq[Seq[Tile]] = Seq(
    Seq(size1),
    Seq(size2, size2 * R2F0),
    Seq(size3, size3m, size3 * R2F0))

  // One-Way Road, traffic running north; the approach tiles are turned by 180 degrees in the piece
  val owrSize1:   Tile = Crosswalk1~WE  & CrosswalkDir~SN & rhd
  val owrSize2:   Tile = Crosswalk2~EW  & CrosswalkDir~SN & rhd  // departing, outer edge north
  val owrSize2In: Tile = Crosswalk2~EW  & CrosswalkDir~NS & rhd  // approach
  val owrSize3m:  Tile = Crosswalk3m~WE & CrosswalkDir~SN & rhd
  val owrSize3In: Tile = Crosswalk3~EW  & CrosswalkDir~NS & rhd  // approach (Size 3 has no departing tile)
  val owrTiles: Seq[(Int, Tile)] =
    Seq(0x000 -> owrSize1, 0x100 -> owrSize2, 0x180 -> owrSize2In, 0x200 -> owrSize3m, 0x380 -> owrSize3In)
  val owrPieces: Seq[Seq[Tile]] = Seq(
    Seq(owrSize1),
    Seq(owrSize2, owrSize2In * R2F0),
    Seq(owrSize3m, owrSize3In * R2F0))

  /** The same pieces with traffic running south. */
  def reversed(pieces: Seq[Seq[Tile]]): Seq[Seq[Tile]] = pieces.map(_.reverse.map(_ * R2F0))

  val streetBase = 0x5C5F1000  // SAM-n at streetBase + n * 0x1000
  val streetNets: Seq[(Network, Int)] = (Street :: SamNetworks.toList).zipWithIndex.map {
    case (net, i) => net -> (streetBase + i * 0x1000)
  }
  // the Double OWR pieces place the same tiles in both columns
  val owrNets: Map[Network, Int] = Map(Onewayroad -> 0x5C5FD000, Owr1 -> 0x5C5FE000, Owr3 -> 0x5C5FF000)
  val owr5A = 0x5C5B0000  // Owr5~NS
  val owr5B = 0x5C5B1000  // Owr5~SN

  // RHW-2 uses the Street tiles; next to RHW-4 they become directional (Rhw4~SN runs north)
  val rhw2Base = 0x5C5F0000
  val rhw4Base = 0x5C5B8000
  val rhw4Tiles: Seq[(Int, Tile)] = Seq(
    0x000 -> (size1  & Rhw4~SN),
    0x100 -> (size2  & Rhw4~SN),  // departing
    0x180 -> (size2  & Rhw4~NS),  // approach
    0x200 -> (size3m & Rhw4~SN),
    0x300 -> (size3  & Rhw4~SN),  // departing
    0x380 -> (size3  & Rhw4~NS))  // approach
}

class MidblockCrosswalkResolver extends IdResolver {
  import MidblockCrosswalks._
  val tileMap: scala.collection.Map[Tile, IdTile] = {
    val builder = new ResolverBuilder
    for ((net, base) <- streetNets; (off, t) <- twoWayTiles) builder.add(base + off, t & net~NS)
    for ((off, t) <- twoWayTiles) builder.add(rhw2Base + off, t & Dirtroad~NS)
    for ((off, t) <- rhw4Tiles) builder.add(rhw4Base + off, t)
    for ((net, base) <- owrNets; (off, t) <- owrTiles) builder.add(base + off, t & net~NS)
    for ((off, t) <- owrTiles) {
      builder.add(owr5A + off, t & Owr5~NS)
      builder.add(owr5B + off, t & Owr5~SN)
    }
    builder.result()
  }
  def isDefinedAt(t: Tile): Boolean = tileMap.isDefinedAt(t)
  def apply(t: Tile): IdTile = tileMap(t)
}

class MidblockCrosswalkRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator {
  import MidblockCrosswalks._

  /** Converts the pieces on `from` next to `to`, entering from the west. */
  private def convert(from: Network, to: Network, dir: IntFlags, pieces: Seq[Seq[Tile]]): Unit = {
    val we = pieces.map(_.map(_ * R3F0))  // north-south to west-east
    for (p <- we) {
      Rules += to~dir | p.head & (from ~> to)~dir
      for ((a, b) <- p.zip(p.tail)) Rules += a & to~dir | b & (from ~> to)~dir
      Rules += p.last & to~dir | (from ~> to)~dir
      for (q <- we) Rules += p.last & to~dir | q.head & (from ~> to)~dir
    }
    createRules()
  }

  def start(): Unit = {
    for (sam <- SamNetworks) convert(Street, sam, WE, twoWayPieces)
    for (nwm <- Seq(Owr1, Owr3); pieces <- Seq(owrPieces, reversed(owrPieces))) {
      convert(Onewayroad, nwm, WE, pieces)
    }
    for (half <- Seq(WE, EW); pieces <- Seq(owrPieces, reversed(owrPieces))) {  // OWR-5 halves A and B
      convert(Onewayroad, Owr5, half, pieces)
    }
    for (dir <- Seq(WE, EW)) convert(Dirtroad, Rhw4, dir, twoWayPieces)
  }
}

object CompileMidblockCrosswalkCode extends AbstractMain {
  lazy val resolve: IdResolver = new MidblockCrosswalkResolver orElse new SamResolver orElse new RhwResolver orElse new MiscResolver
  val generator = new MidblockCrosswalkRuleGenerator(_)
  lazy val file = new java.io.File("Controller/RUL2/15_PedestrianRevolution/Sec15b_MidblockCrosswalks/Sec15b_MidblockCrosswalks_MetaGenerated.txt")
}
