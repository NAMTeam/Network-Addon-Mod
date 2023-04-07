package metarules.module

import metarules.meta._
import Network._, Flags._, Flag._, RotFlip._, Implicits._
import NetworkProperties.{projectLeftSeg, projectRightSeg}

/** This modifies the rule generating process by duplicating all rules that
  * contain tiles that have mirrored variants with different IIDs, such as Rail
  * crossings.
  * The left projection corresponds to the unmirrored-only tile (in RHD),
  * the right projection to the mirrored-only tile (in RHD).
  * See MiscResolver for how to map the left/right projected tiles to IdTiles.
  *
  * TLA-networks are handled automatically.
  */
object MirrorVariants {

  private lazy val mirrorVariants: scala.collection.Map[SymTile, (SymTile, SymTile)] = {
    val map = scala.collection.mutable.Map.empty[SymTile, (SymTile, SymTile)]
    def add(tile: Tile, tileL: Tile, tileR: Tile): Unit = {
      assert(!map.contains(tile))
      for (rf <- RotFlip.values) {
        map.getOrElseUpdate(tile * rf, (tileL * rf, tileR * rf))
      }
    }
    def addProjectFirst(seg1: Segment, seg2: Segment): Unit = {
      add(seg1 & seg2, projectLeftSeg(seg1) & seg2, projectRightSeg(seg1) & seg2)
    }
    // add additional mirror variants here
    addProjectFirst(Road~NS, Rail~NE)
    addProjectFirst(Road~WN, Rail~NS)
    addProjectFirst(Road~ES, Rail~NE)
    addProjectFirst(Street~WN, Rail~NS)
    addProjectFirst(Street~ES, Rail~NE)
    addProjectFirst(Onewayroad~ES, Rail~NE)
    addProjectFirst(Avenue~SN, Rail~NE)
    addProjectFirst(Avenue~NS, Rail~NE)
    map
  }

  def containsTlaFlags(tile: SymTile): Boolean = tile match {
    case tile: Tile => tile.segs.exists(_.network.isTla)
    case _ => false
  }
  def shouldProjectTlaLeftOnly(tile: SymTile): Boolean = tile match {
    case tile: Tile => tile.segs.forall(!_.network.isTla) || !tile.symmetries.quotient.exists(_.flipped)
    case _ => true
  }
  def projectTlaLeft(tile: SymTile): SymTile = tile match {
    case tile: Tile => Tile.projectLeft(tile)
    case _ => tile
  }
  def projectTlaRight(tile: SymTile): SymTile = tile match {
    case tile: Tile => Tile.projectRight(tile)
    case _ => tile
  }

  private val tlaPreprocessor: Rule[SymTile] => Iterator[Rule[SymTile]] = rule => {
    if (!rule.exists(containsTlaFlags)) {
      Iterator(rule)
    } else if (rule.forall(shouldProjectTlaLeftOnly)) {
      Iterator(rule.map(projectTlaLeft))
    } else {
      Iterator(rule.map(projectTlaLeft), rule.map(projectTlaRight))
    }
  }

  val preprocessor: Rule[SymTile] => Iterator[Rule[SymTile]] = rule => {
    if (!rule.exists(tile => mirrorVariants.contains(tile))) {
      Iterator(rule)
    } else {
      Iterator(  // yield the two projected rules
        rule.map(tile => mirrorVariants.get(tile).map(_._1).getOrElse(tile)),
        rule.map(tile => mirrorVariants.get(tile).map(_._2).getOrElse(tile)))
    }
  }.flatMap(tlaPreprocessor)
}
