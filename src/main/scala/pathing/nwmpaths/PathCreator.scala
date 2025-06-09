package com.sc4nam.pathing.nwmpaths

import io.github.memo33.metarules.pathing._, Bezier._
import io.github.memo33.metarules.meta._, com.sc4nam.module.syntax._, Network._, com.sc4nam.module.NetworkProperties
import io.github.memo33.scdbpf._, Sc4Path.{TransportType => TT, _}, Cardinal._, DbpfUtil.RotFlip._
import Trimming._
import NetworkConfig.straightPaths

import scala.language.implicitConversions

object PathCreator {

  type SPaths = Seq[SPath]

  /** `SimplePath`:
    *
    * @param dir the ''from'' direction; merely indicates the overall direction of the path,
    * i.e. main will always be directed vertically and crossing network
    * horizontally, regardless of whether orthogonal or diagonal.
    */
  case class SPath(tt: TT, dir: Cardinal, points: Points) {
    def * (rf: RotFlip): SPath = copy(dir = dir *: rf, points = points map (p => p *: rf))
  }

  def generateNwmPaths(implicit resolver: IdResolver): Iterable[BufferedEntry[Sc4Path]] = {
    val ids = scala.collection.mutable.Map.empty[Int, (Sc4Path, Boolean)]
    // TODO provisional
    val mainNetworks = NwmNetworks -- Set(Ave8)
    val minorNetworks = NwmNetworks ++ Set(Road, Street, Onewayroad, Avenue) -- Set(Ave8)
    for {
      main <- mainNetworks
      minor <- minorNetworks
    } /*do*/ {
      import Flags._, Implicits._
      def add(seg1: Segment, seg2: Segment, modelBased: Boolean) = {
        val tile0 = NetworkProperties.transformSharedDiagonals(seg1 & seg2)
        if (!seg1.network.isTla && !seg2.network.isTla) {
          val idTile = resolver(tile0)
          if (!ids.contains(idTile.id)) {
            val intersection = new PlusIntersection(seg1, seg2)
            ids(idTile.id) = (intersection.buildSc4Path * (R0F0 / idTile.rf), modelBased)
          }
        } else {
          // special handling for center turning lanes of TLAs
          // TODO Orientations and directions of paths need testing,
          // and the alternative TLA turn paths need permanent IIDs
          val idTile1 = resolver(NetworkProperties.projectTlaLeft(tile0))
          val idTile2 = resolver(NetworkProperties.projectTlaLeft(tile0 * R0F1))
          if (!ids.contains(idTile1.id)) {
            val intersection = new PlusIntersection(seg1, seg2)
            ids(idTile1.id) = (intersection.buildSc4Path * (R0F0 / idTile1.rf), modelBased)
          }
          if (!ids.contains(idTile2.id)) {
            val intersection = new PlusIntersection(seg1 * R0F1, seg2 * R0F1)
            val stop = intersection.buildSc4Path * (R0F0 / idTile2.rf)
            ids(idTile2.id) = (stop.copy(stopPaths = stop.stopPaths.map(p => p.copy(uk = !p.uk))), modelBased)  // flip uk flag to account for mirroring
          }
        }
      }
      def hasDiagonalOverhangs(n: Network): Boolean = n.isNwm && NetworkProperties.isSingleTile(n) && n != Owr1
      // In the following, it is important to choose the same directions as in
      // the IID scheme, since otherwise the uk flags can end up flipped.
      for { // OxD
        mainDir <- Seq(NS, SN)
        minDir <- Seq(SW, WS)
      } /*do*/ {
        add(main~mainDir, minor~minDir, modelBased =
          hasDiagonalOverhangs(main) && hasDiagonalOverhangs(minor)
          || main.height != 0 || minor.height != 0,
        )
      }
      for { // DxO
        mainDir <- Seq(ES, SE)
        minDir <- Seq(EW, WE)
      } /*do*/ {
        add(main~mainDir, minor~minDir, modelBased =
          hasDiagonalOverhangs(main) && hasDiagonalOverhangs(minor)
          || main.height != 0 || minor.height != 0,
        )
      }
      for { // DxD
        mainDir <- Seq(ES, SE)
        minDir <- Seq(SW, WS)
      } /*do*/ {
        add(main~mainDir, minor~minDir, modelBased =
          (hasDiagonalOverhangs(main) || hasDiagonalOverhangs(minor)) && (main != Owr1 && minor != Owr1)
          || main.height != 0 || minor.height != 0,
        )
      }
    }
    ids.map { case (id, (p, modelBased)) =>
      require(p.validateClassNumbers, "duplicate class numbers detected in " + p.toString)
      BufferedEntry(
        tgi = Tgi(0,0,id).copy(if (modelBased) Tgi.Sc4Path3d else Tgi.Sc4Path2d),
        content = p.copy(decFormat = Some(Sc4Path.threeDecimals)),
        compressed = true,
      )
    }
  }
}
