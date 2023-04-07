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
  */
trait MirrorVariants { this: RuleGenerator =>
  // TODO: This probably does not play well with TLA-networks yet which have a
  // similar mechanism, but mostly hardcoded in the metarules package.

  val mirrorVariants: scala.collection.Map[SymTile, (SymTile, SymTile)] = {
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

  override protected def createRules(): Unit = {
    Rules.distinct foreach { r =>
      if (!r.exists(tile => mirrorVariants.contains(tile))) {
        queue ++= RuleTransducer(r)(resolver, tileOrientationCache)
      } else {
        val r1 = r.map(tile => mirrorVariants.get(tile).map(_._1).getOrElse(tile))
        val r2 = r.map(tile => mirrorVariants.get(tile).map(_._2).getOrElse(tile))
        queue ++= RuleTransducer(r1)(resolver, tileOrientationCache)
        queue ++= RuleTransducer(r2)(resolver, tileOrientationCache)
      }
    }
    Rules.clear()
  }
}

