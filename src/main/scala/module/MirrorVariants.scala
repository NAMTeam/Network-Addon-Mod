package com.sc4nam.module

import io.github.memo33.metarules.meta._
import syntax._, Network._, Flags._, RotFlip._, Implicits._

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
      add(seg1 & seg2, seg1.projectLeft & seg2, seg1.projectRight & seg2)
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
    // addProjectFirst(Road~ES, Avenue~NEC)
    // addProjectFirst(Road~SE, Avenue~CNE)
    for (n <- Seq(Onewayroad, Owr3)) {
      addProjectFirst(n~NS, Avenue~(0,0,0,1))
      addProjectFirst(n~NS, Avenue~(1,0,0,3))
      addProjectFirst(n~NS, Avenue~(0,0,0,3))
      addProjectFirst(n~CSE, Avenue~NS)
      addProjectFirst(n~CNW, Avenue~NS)
      addProjectFirst(n~CS, Avenue~ES)
      addProjectFirst(n~WE, Road~CNE)
      addProjectFirst(n~NE, Road~CS)
      addProjectFirst(n~WS, Road~CS)
      addProjectFirst(n~EN, Avenue~CW)
      addProjectFirst(n~SW, Avenue~CW)
      addProjectFirst(n~EN, Avenue~WC)
      addProjectFirst(n~SW, Avenue~WC)
      addProjectFirst(n~NE, Onewayroad~CS)
      addProjectFirst(n~WS, Onewayroad~CS)
      addProjectFirst(n~NE, Owr3~CS)
      addProjectFirst(n~WS, Owr3~CS)
      addProjectFirst(n~SE, Onewayroad~CEN)
      addProjectFirst(n~WN, Onewayroad~CEN)
      addProjectFirst(n~SE, Owr3~CSW)
      addProjectFirst(n~WN, Owr3~CSW)
      addProjectFirst(n~ES, Avenue~NEC)
      addProjectFirst(n~SE, Avenue~CNE)
      // addProjectFirst(n~SE, Avenue~(3,1,0,0))
      addProjectFirst(n~SE, Avenue~(0,0,3,1))
    }
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
    case tile: Tile => NetworkProperties.projectTlaLeft(tile)
    case _ => tile
  }
  def projectTlaRight(tile: SymTile): SymTile = tile match {
    case tile: Tile => NetworkProperties.projectTlaRight(tile)
    case _ => tile
  }

  val preprocessor: Rule[SymTile] => Iterator[Rule[SymTile]] = rule => {
    val hasMirrorVariant = rule.exists(tile => mirrorVariants.contains(tile))
    for {
      rule <- if (hasMirrorVariant) Iterator(  // yield the two projected rules
                rule.map(tile => mirrorVariants.get(tile).map(_._1).getOrElse(tile)),
                rule.map(tile => mirrorVariants.get(tile).map(_._2).getOrElse(tile)),
              )
              else Iterator(rule)
      hasTlaFlags = rule.exists(containsTlaFlags)
      rule <- if (!hasTlaFlags) Iterator(rule)
              else if (rule.forall(shouldProjectTlaLeftOnly)) Iterator(rule.map(projectTlaLeft))
              else Iterator(rule.map(projectTlaLeft), rule.map(projectTlaRight))
      // We duplicate the rule by its R2F1 variant if there are TLAs or mirror
      // variants involved, as the left/right distinction can make them resolve
      // to different IDs (and the generators do not necessarily account for
      // this). For example, this is needed for Tla5/Avenue D×D.
      rule <- if (hasMirrorVariant || hasTlaFlags) Iterator(rule, rule.map(_ * R2F1)).distinct
              else Iterator(rule)
    } yield rule
  }

  /* For plain orthogonal and diagonal tiles of TLA networks, we ignore any
   * remapping defined in `tileOrientationCache` that leads to mirroring, as
   * that would amplify the mirroring problems due to the presence of turning
   * lanes. Not a perfect solution, as occasional mirroring problems will remain.
   */
  def ignoreMirroredOrientations(resolve: IdResolver): Set[Int] = {
    val ids = Set.newBuilder[Int]
    for {
      n <- Seq(Tla3, Tla5, Tla7m, Road, Onewayroad)  // TODO consider adding all the networks
      dir <- Seq(NS, ES, SE)  // these are arbitrary orth/diag directions to allow us find the IDs
      tile <- Seq(NetworkProperties.projectTlaLeft(n~dir), NetworkProperties.projectTlaRight(n~dir))
      idTile <- resolve.lift(tile)
    } {
      ids += idTile.id
    }
    ids.result()
  }

}
