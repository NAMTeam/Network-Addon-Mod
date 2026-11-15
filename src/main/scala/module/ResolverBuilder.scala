package com.sc4nam.module

import scala.jdk.CollectionConverters._
import io.github.memo33.metarules.meta._, syntax._, RotFlip._

/** A builder of a Tile-to-IdTile mapping for use as a resolver.
  */
class ResolverBuilder(remap: Tile => Tile = identity) extends scala.collection.mutable.Builder[(Tile, IdTile | (IdTile, IdTile)), collection.Map[Tile, IdTile]] {
  var tileMap = (new java.util.concurrent.ConcurrentHashMap[Tile, IdTile]()).asScala
  def clear(): Unit = tileMap = (new java.util.concurrent.ConcurrentHashMap[Tile, IdTile]()).asScala
  def result(): collection.Map[Tile, IdTile] = tileMap

  /** Add a new mapping from a `Tile` to an `IdTile`.
    * All unique rotations of the tile are added, as well.
    *
    * If TLA/spin networks are present (without left- or right-spins), by default both
    * projections are mapped to the same ID.
    *
    * If a tuple `(IdTile, IdTile)` is passed, these are taken to be mirror
    * variants of the same tile (first: non-mirrored, second: mirrored).
    * This allows defining different IDs, depending on whether a tile is
    * mirrored or not (e.g. for TLA networks or D×O Rail×Avenue crossings).
    */
  def addOne(elem: (Tile, IdTile | (IdTile, IdTile))): this.type = {
    val (tile0, idTiles) =
      // Earlier versions of the implementation used to normalize the rotations
      // so that the IdTile appeared with R0F0. This caused problems when a tile
      // should only appear in mirrored form, i.e. without R0F0 (e.g. LHD
      // versions of Crosswalks), which could not be defined.
      // Instead, such a normalization must now be performed add the call site
      // by rerotating the passed Tile, so that the IdTile(s) can be taken as is.
      elem._2 match {
        case i: IdTile => (remap(elem._1), i)
        case (i1, i2) => (remap(elem._1), (i1, i2))
      }

    // by default, we project any TLA/spin flags if necessary and define the same ID for both
    val spinSegs = tile0.segs.filter(_.network.hasSpin)
    val tiles: Seq[Tile] =
      if (spinSegs.nonEmpty) {
        if (spinSegs.forall(s => s.flags.manifest.kind == Flag.Kind.Default)) {
          Seq(NetworkProperties.projectTlaLeft(tile0), NetworkProperties.projectTlaRight(tile0))
        } else if (spinSegs.forall(s => s.flags.manifest.kind == Flag.Kind.LeftSpin)) {
          Seq(tile0)
        } else {
          require(spinSegs.forall(s => s.flags.manifest.kind == Flag.Kind.RightSpin), s"The tile $tile0 ($idTiles) contains TLA/spin flags of mixed kinds.")
          Seq(tile0)
        }
      } else Seq(tile0)

    def put(tile: Tile, idTile: IdTile): Unit = {
      val prev = tileMap.putIfAbsent(tile, idTile)
      if (prev.nonEmpty) {
        throw new ResolverBuilder.DuplicateId(tile = tile, prev = prev.get, next = idTile)
      }
    }

    for (tile <- tiles) {
      idTiles match {
        case (idTile1, idTile2) =>  // passing 2 IDs allows defining two different IDs (mirror variants), depending on whether the tile is flipped
          for (rf <- tile.representations) {
            put(tile * rf, (if (!rf.flipped) idTile1 else idTile2) * rf)
          }
        case idTile: IdTile =>
          for (rf <- tile.representations) {
            put(tile * rf, idTile * rf)
          }
      }
    }

    this
  }

  // for convenience
  def add(tile: Tile, id: Int, when: Boolean = true): this.type = {
    if (when) {
      val idTile = IdTile(id, R0F0)
      addOne((tile, idTile))
    } else this
  }

  // for convenience
  def add(id: Int, tile: Tile): this.type = add(tile, id)

  // for convenience
  def add(id: Int, tile: Tile, when: Boolean): this.type = add(tile, id, when)

}
object ResolverBuilder {
  class DuplicateId(tile: Tile, prev: IdTile, next: IdTile) extends Exception(
    s"Resolver already contains an ID for tile $tile = $next (previous: $prev)"
  ) {
    // remove top frames from trace to make it easier to locate the relevant call
    setStackTrace(
      getStackTrace.dropWhile(elem =>
        elem.getClassName.startsWith("scala.") ||
        elem.getClassName == "com.sc4nam.module.ResolverBuilder" ||
        elem.getClassName.startsWith("io.github.memo33.metarules.meta.")
      )
    )
  }
}
