package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._, RotFlip._, Flags._
import Implicits.segmentToTile
import NetworkProperties._

/** Resolves from IDs to meta tiles (on a best effort basis).
  * This is only intended for diagnosing problems with existing rules, not for
  * generating new metarule code.
  */
class ReverseResolver private[module] (val reverseTileMap: collection.Map[Int, ::[Tile]]) extends PartialFunction[IdTile | Int, ::[Tile]] {
  private def toId(idTile: IdTile | Int): Int = idTile match {
    case x: IdTile => x.id
    case id: Int => id
  }
  def isDefinedAt(idTile: IdTile | Int): Boolean = reverseTileMap.contains(toId(idTile))
  def apply(idTile: IdTile | Int): ::[Tile] = {
    val fiber = reverseTileMap.apply(toId(idTile))
    idTile match {
      case x: IdTile => fiber.map(_ * x.rf).asInstanceOf[::[Tile]]
      case id: Int => fiber
    }
  }
}

object ReverseResolver {

  /** Checks if tiles define the same tile if segments are merged.
    * e.g. (2,0,2,0) & (0,2,0,2)  vs  (2,2,2,2)
    * or (2,0,2,0) & (0,3,0,0)  vs  (2,13,2,0)
    */
  def isSegmentContraction(tile1: Tile, tile2: Tile): Boolean = {
    val List(t1, t2) = List(tile1, tile2).sortBy(_.segs.size)
    if (t1.segs.size == 1 && t2.segs.size == 2 && t2.segs.forall(_.network == t1.segs.head.network)) {
      val s1 = t1.segs.head
      val Seq(s2, s3) = t2.segs.toSeq
      if ((0 to 3).forall(i => (s2.flags(i) == 0 || s3.flags(i) == 0) && (
        s1.flags(i) == s2.flags(i) + s3.flags(i)
        || Seq(s1.flags(i), s2.flags(i) + s3.flags(i)).map(_.abs).forall(f => f == 1 || f == 3 || f == 11 || f == 13)
      ))) {
        true
      } else false
    } else false
  }

  /** Checks if tiles are DxD T or + intersections (which might use the same ID)
    * e.g. (3,0,0,1) & (0,3,0,0)  vs  (3,0,0,1) & (1,3,0,0)
    */
  def isDiagonalTAmbuigity(tile1: Tile, tile2: Tile): Boolean = {
    val diff1 = tile1.segs &~ tile2.segs
    val diff2 = tile2.segs &~ tile1.segs
    (diff1.toSeq, diff2.toSeq) match {
      case (Seq(s1), Seq(s2)) if s1.network == s2.network =>
        (0 to 3).filter(i => s1.flags(i) != s2.flags(i)) match {
          case Seq(j) =>
            (s1.flags(j) == 0 || s2.flags(j) == 0) && {
              val f = (s1.flags(j) + s2.flags(j)).abs
              (f == 1 || f == 3) && (tile1.segs & tile2.segs).exists(s => s.flags(j).abs + f == 4)  // i.e. diagonal and opposing diagonal
            }
          case _ => false
        }
      case _ => false
    }
  }

  /** Expand this as necessary to allow multiple metarule definitions to resolve
    * to the same ID.
    */
  def allowedConflict(idTile: IdTile, tile1: Tile, tile2: Tile): Boolean = {
    if (tile1.segs.exists(_.network.isTla) || tile2.segs.exists(_.network.isTla)) {
      true  // TODO refine
    } else if (isSegmentContraction(tile1, tile2)) {
      true
    } else if (isDiagonalTAmbuigity(tile1, tile2)) {
      true
    } else if (idTile.id == 0x5D7E0800) {  // L1Dtr->L2Dtr upper HT uses same ID as L0Dtr->L2Dtr
      true
    } else {
      false
    }
  }

  def create(): ReverseResolver = {
    val maps = Seq(
      (new MiscResolver).tileMap,
      (new SamResolver).tileMap,
      (new RealRailwayResolver).tileMap,
      (new RhwResolver).tileMap,
      (new flexfly.FlexFlyResolver).tileMap,
      (new NwmResolver).tileMap,
    )

    val reverseTileMap = collection.mutable.Map.empty[Int, ::[Tile]]
    val conflicts = collection.mutable.Buffer.empty[(IdTile, Tile, Tile)]
    maps.foreach(m => m.iterator.foreach { case (tile, idTile) =>
      if (idTile.rf == R0F0) {  // we only take and store R0F0 orientations, as tileMaps might not contain all of the other orientations
        val tiles = reverseTileMap.get(idTile.id).getOrElse(Nil)
        if (!tiles.contains(tile)) {
          tiles match {
            case (t :: _) if !allowedConflict(idTile, tile, t) =>
              conflicts += ((idTile, tile, t))
            case _ =>
              reverseTileMap(idTile.id) = ::(tile, tiles)
          }
        }  // else nothing to do
      }
    })

    if (conflicts.nonEmpty) {
      for ((idTile, tile1, tile2) <- conflicts) {
        if (!allowedConflict(idTile, tile1, tile2)) {
          println(s"Conflicting metarule-resolution for ID $idTile:  $tile1  vs  $tile2")
        }
      }
      throw new AssertionError(s"There were ${conflicts.length} conflicting metarule-resolutions for IDs.")
    }
    new ReverseResolver(reverseTileMap)
  }
}
