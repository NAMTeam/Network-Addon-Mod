package metarules
package module.flexfly

import meta._, Flags._, RotFlip._, Network._, Implicits._
import FlexFlyTiles._
import java.io.{File, PrintWriter}

/* This file contains an ad-hoc implementation for generating the RUL0 and RUL1
 * code for FlexFlys. In particular, it is not specific to MetaRules.
 *
 * Suitable falsies are computed algorithmically. It is crucial that the falsies
 * don't change after release! What follows are a few notes on the algorithm.
 */
/*
  constraints on flag combos:
  - each of the two segments needs at least one flag of 1, 2 or 3
  - the two segments can't have the same flag on an edge, unless the flag is 0 or 4
  - to be able to drag orthogonal RHW through an edge, the edge needs a RHW flag 2
    or 4, or it needs a MHW flag 2 and RHW flag 0

  which flag combinations (RHW & MHW) allow dragging RHW in which directions?

  RHW 0 & MHW 0 => none
  RHW 0 & MHW 1 => diag 1
  RHW 0 & MHW 2 => orth
  RHW 0 & MHW 4 => none
  RHW 1 & MHW 0 => diag 1
  RHW 1 & MHW 1 => illegal
  RHW 1 & MHW 2 => diag 1
  RHW 1 & MHW 3 => diag 1
  RHW 1 & MHW 4 => diag 1
  RHW 2 & MHW 0 => orth
  RHW 2 & MHW 1 => all
  RHW 2 & MHW 2 => illegal
  RHW 2 & MHW 4 => all
  RHW 4 & MHW 0 => all
  RHW 4 & MHW 1 => all
  RHW 4 & MHW 2 => all
  RHW 4 & MHW 4 => all

  Also note that some combinations of flags are prone to auto-connect. For example,
  RHW~(2,4,4,4) & MHW~(1,0,0,0) (WNES) has auto-connect in the EAST(!), but no other side.
  In fact, all falsies with RHW~(2,4,4,4) (or rotations thereof) have auto-connect at the
  side opposite of the 2 flag.
*/

object CompileFlexFlyRul0And1 {

  /** a stream of flag combinations we can choose our FlexFly falsies from */
  private[this] lazy val combos = {
    val cache = collection.mutable.HashSet.empty[Tile]
    def isUnique(tile: Tile) = RotFlip.values.forall(rf => !cache.contains(tile * rf))
    def notZero(a: Int, b: Int, c: Int, d: Int) = a != 0 && a != 4 || b != 0 && b != 4 || c != 0 && c != 4 || d != 0 && d != 4
    def different(a: Int, b: Int) = a != b || a == 0 || a == 4
    val r = 4 to 0 by -1
    for {
      a1 <- r.toStream; a2 <- r if different(a1, a2)
      b1 <- r; b2 <- r if different(b1, b2)
      c1 <- r; c2 <- r if different(c1, c2)
      d1 <- r; d2 <- r if different(d1, d2)
      if notZero(a1,b1,c1,d1) && notZero(a2,b2,c2,d2)
      tile = Segment(Groundhighway, (Road~(a1,b1,c1,d1)).flags) & Dirtroad~(a2,b2,c2,d2) // we use Road to construct symmetrical flags, and then convert to GMHW
      if isUnique(tile)
    } yield {
      cache += tile
      tile
    }
  }

  /** in order MHW/Road, Dirtroad/RHW */
  private[this] def extractFlags(tile: Tile) = tile.segs.toSeq match {
    case Seq(Segment(Groundhighway, rdFlags), Segment(Dirtroad, rhwFlags)) => (rdFlags, rhwFlags)
  }

  /** restricts above flag combinations to ones that allow dragging-through in all directions
    * and which have no symmetries
    */
  private[this] lazy val combosAllDirections = {
    import Flag._, Bi._
    def effectiveFlag(rhwFlag: Flag, mhwFlag: Flag) = (rhwFlag, mhwFlag) match {
      case (Zero, Four) => Zero
      case (Zero, _) => mhwFlag
      case (DiagLeft, DiagLeft) => throw new UnsupportedOperationException
      case (DiagLeft, _) => DiagLeft
      case (Orth, Orth) => throw new UnsupportedOperationException
      case (Orth, Zero) => Orth
      case (Orth, _) => Four
      case (DiagRight, DiagRight) => throw new UnsupportedOperationException
      case (DiagRight, _) => DiagRight
      case (Four, _) => Four
    }
    combos filter { tile =>
      val (mhwFlags, rhwFlags) = extractFlags(tile)
      (rhwFlags zip mhwFlags) forall { case (r, m) => effectiveFlag(r, m) == Flag.Four }
    } filter (_.symmetries == Group.SymGroup.Cyc1)
  }

  /** splits above flag combinations into those that have auto-connect problem
    * and those that have not; the auto-connect tiles will be oriented such that
    * 2 flag is west (so auto-connect is east)
    */
  private[this] lazy val (nonAutoconnectTiles, autoconnectTiles) = {
    val mapped = combosAllDirections map { tile =>
      val flags = tile.segs.find(_.network == Dirtroad).get.flags
      import Flag._
      val rfOpt = RotFlip.values find (rf => flags * rf == Flags(Bi.Orth, Four, Four, Four))
      (tile, rfOpt)
    }
    val nonAutoconnectTiles = mapped collect { case (tile, None) => tile }
    val autoconnectTiles = mapped collect { case (tile, Some(rf)) => tile * rf }
    (nonAutoconnectTiles, autoconnectTiles)
  }

  val flexFlySegs: Seq[Segment] = for {
    orient <- Seq[IntFlags => IntFlags](identity _, reverseIntFlags _)
    network <- (RhwNetworks from Mis to L4Rhw4).iterator
    t <- Seq(T0, T1, T3, T6)
  } yield {
    network~orient(t)
  }

  /** converts virtual FlexFly tiles to concrete falsies */
  lazy val convertVirtualTile: Map[Segment, Tile] = {
    // auto-connect tiles seem to be less useful, so we will use them
    // nevertheless so as to save more valuable falsies; we will use them in
    // cases of adjacent anchors (T0, T1, T6)
    val autoConnectIter = autoconnectTiles.iterator
    val nonAutoconnectIter = nonAutoconnectTiles.iterator
    def buildTiles(n: Network, orient: IntFlags => IntFlags) = (Seq.newBuilder
      += n~orient(T0) -> autoConnectIter.next
      += n~orient(T1) -> autoConnectIter.next * R2F0
      += n~orient(T3) -> nonAutoconnectIter.next
      += n~orient(T6) -> autoConnectIter.next * R2F0
      ).result
    (for {
      orient <- Seq[IntFlags => IntFlags](identity _, reverseIntFlags _)
      network <- (RhwNetworks from Mis to L4Rhw4).iterator
      tuple <- buildTiles(network, orient)
    } yield tuple)(collection.breakOut)
  }

  private[this] def concreteTileToString(tile: Tile): String = {
    tile.segs.toSeq.map { case Segment(network, flags) =>
      network.toString.toLowerCase + ": 0x0" + flags.mkString("0").reverse
    } .mkString(" ")
  }

  def rul0Entry(hid: Int, network: Network, reverse: Boolean, previewIter: Iterator[(Int, String)]) = {
    val (previewId90, previewName90) = previewIter.next
    val (previewId45, previewName45) = previewIter.next
    val orient: IntFlags => IntFlags = if (reverse) reverseIntFlags _ else identity _
    f"""
    |[HighwayIntersectionInfo_0x$hid%08X]
    |;Added by memo 2014/11/16
    |;FlexFly 90
    |Piece = -80.0, 0.0, 0, 1, 0x$previewId90%08X
    |PreviewEffect = $previewName90
    |
    |CellLayout=........
    |CellLayout=.abY....
    |CellLayout=...cY...
    |CellLayout=....dY..
    |CellLayout=.....e..
    |CellLayout=.....fZ<
    |CellLayout=......^.
    |
    |CheckType = Z - dirtroad: 0x02020202
    |CheckType = Y - dirtroad: 0x00000000, 0xFFFFFFFF optional
    |CheckType = a - ${concreteTileToString(convertVirtualTile(network~orient(T0)))}, 0xFFFFFFFF optional
    |CheckType = b - ${concreteTileToString(convertVirtualTile(network~orient(T1)))}, 0xFFFFFFFF optional
    |CheckType = c - ${concreteTileToString(convertVirtualTile(network~orient(T3)))}, 0xFFFFFFFF optional
    |CheckType = d - ${concreteTileToString(convertVirtualTile(network~orient(T3)) * R3F1)}, 0xFFFFFFFF optional
    |CheckType = e - ${concreteTileToString(convertVirtualTile(network~orient(T1)) * R3F1)}, 0xFFFFFFFF optional
    |CheckType = f - ${concreteTileToString(convertVirtualTile(network~orient(T0)) * R3F1)}, 0xFFFFFFFF optional
    |
    |ConsLayout=........
    |ConsLayout=........
    |ConsLayout=........
    |ConsLayout=........
    |ConsLayout=........
    |ConsLayout=......+<
    |ConsLayout=......^.
    |
    |AutoTileBase = 0x55387000
    |PlaceQueryID = 0x5CAB0000
    |Costs = 600
    |
    |[HighwayIntersectionInfo_0x${hid+0x10000}%08X]
    |CopyFrom=0x$hid%08X
    |Rotate=1
    |[HighwayIntersectionInfo_0x${hid+0x20000}%08X]
    |CopyFrom=0x$hid%08X
    |Rotate=2
    |[HighwayIntersectionInfo_0x${hid+0x30000}%08X]
    |CopyFrom=0x$hid%08X
    |Rotate=3
    |[HighwayIntersectionInfo_0x${hid+0x40000}%08X]
    |CopyFrom=0x$hid%08X
    |Transpose=1
    |[HighwayIntersectionInfo_0x${hid+0x50000}%08X]
    |CopyFrom=0x${hid+0x40000}%08X
    |Rotate=1
    |[HighwayIntersectionInfo_0x${hid+0x60000}%08X]
    |CopyFrom=0x${hid+0x40000}%08X
    |Rotate=2
    |[HighwayIntersectionInfo_0x${hid+0x70000}%08X]
    |CopyFrom=0x${hid+0x40000}%08X
    |Rotate=3
    |
    |
    |[HighwayIntersectionInfo_0x${hid+1}%08X]
    |;Added by memo 2014/11/16
    |;FlexFly 45
    |Piece = -32.0, 0.0, 0, 1, 0x$previewId45%08X
    |PreviewEffect = $previewName45
    |
    |CellLayout=........
    |CellLayout=...Xc...
    |CellLayout=....dY..
    |CellLayout=.....e..
    |CellLayout=.....fZ<
    |CellLayout=......^.
    |
    |CheckType = Z - dirtroad: 0x02020202
    |CheckType = Y - dirtroad: 0x00000000, 0xFFFFFFFF optional
    |CheckType = X - dirtroad: 0x00020000, 0xFFFFFFFF optional
    |CheckType = c - ${concreteTileToString(convertVirtualTile(network~orient(T6)) * R3F1)}, 0xFFFFFFFF optional
    |CheckType = d - ${concreteTileToString(convertVirtualTile(network~orient(T3)) * R3F1)}, 0xFFFFFFFF optional
    |CheckType = e - ${concreteTileToString(convertVirtualTile(network~orient(T1)) * R3F1)}, 0xFFFFFFFF optional
    |CheckType = f - ${concreteTileToString(convertVirtualTile(network~orient(T0)) * R3F1)}, 0xFFFFFFFF optional
    |
    |ConsLayout=........
    |ConsLayout=........
    |ConsLayout=........
    |ConsLayout=........
    |ConsLayout=......+<
    |ConsLayout=......^.
    |
    |AutoTileBase = 0x55387000
    |PlaceQueryID = 0x5CAB0000
    |Costs = 600
    |
    |[HighwayIntersectionInfo_0x${hid+0x10001}%08X]
    |CopyFrom=0x${hid+1}%08X
    |Rotate=1
    |[HighwayIntersectionInfo_0x${hid+0x20001}%08X]
    |CopyFrom=0x${hid+1}%08X
    |Rotate=2
    |[HighwayIntersectionInfo_0x${hid+0x30001}%08X]
    |CopyFrom=0x${hid+1}%08X
    |Rotate=3
    |[HighwayIntersectionInfo_0x${hid+0x40001}%08X]
    |CopyFrom=0x${hid+1}%08X
    |Transpose=1
    |[HighwayIntersectionInfo_0x${hid+0x50001}%08X]
    |CopyFrom=0x${hid+0x40001}%08X
    |Rotate=1
    |[HighwayIntersectionInfo_0x${hid+0x60001}%08X]
    |CopyFrom=0x${hid+0x40001}%08X
    |Rotate=2
    |[HighwayIntersectionInfo_0x${hid+0x70001}%08X]
    |CopyFrom=0x${hid+0x40001}%08X
    |Rotate=3
    |""".stripMargin
  }

  def previews(resolve: IdResolver) = RhwNetworks.from(Mis).to(L4Rhw4).toSeq.flatMap { n =>
    FlexFlyRuleGenerator.orientations.flatMap { orient =>
      Seq(T0, T1) map { t =>
        val id = resolve(n~orient(t)).id & ~0xF | 0x5
        (id, f"preview_flexfly$id%08x")
      }
    }
  }

  def printRul0(file: File, resolver: IdResolver) = for (printer <- resource.managed(new PrintWriter(file))) {
    val hid0 = 0x5B00
    for (hid <- hid0 until hid0 + 40) {
      val numbers = (0 until 8) map (i => f"${hid+0x10000*i}%X") mkString ", "
      printer.println(f"AddTypes = $numbers ; flexfly")
    }
    printer.println()
    printer.println(";###separator###")
    printer.println()

    val hids = Iterator.iterate(hid0)(_ + 2)
    val previewIter = previews(resolver).iterator
    for (network <- RhwNetworks from Mis to L4Rhw4; reverse <- Seq(false, true)) {
      printer.println(rul0Entry(hids.next, network, reverse, previewIter))
    }
  }

  def rul1Entry(tile: Tile, id: Int, header: String): String = {
    tile.representations.zipWithIndex.foldLeft(new StringBuilder(f";$header%n")) { case (sb, (rf, i)) =>
      val rft = tile * rf
      val (mhwFlags, rhwFlags) = extractFlags(rft)
      sb ++= f"Type$i=0x0${rhwFlags.mkString("0").reverse},0x0${mhwFlags.mkString("0").reverse},0x$id%08X,${rf.rot},${rf.flip}%n"
    } .result
  }

  def printRul1(file: File, resolve: IdResolver) = for (printer <- resource.managed(new PrintWriter(file))) {
    for (seg <- flexFlySegs) {
      val idTile = resolve(seg)
      val falsie = convertVirtualTile(seg)
      printer.println(rul1Entry(falsie, idTile.id, seg.toString))
    }
  }

  /** Outputs FlexFly RUL0 and RUL1 code to 'target/FlexFlyRUL0.txt' and
    * 'target/FlexFlyRUL1.txt'.
    */
  def main(args: Array[String]): Unit = {
    val rul0File = new File("target/FlexFlyRUL0.txt")
    val rul1File = new File("target/FlexFlyRUL1.txt")
    val resolver = new FlexFlyResolver
    printRul0(rul0File, resolver)
    printRul1(rul1File, resolver)
  }

}
