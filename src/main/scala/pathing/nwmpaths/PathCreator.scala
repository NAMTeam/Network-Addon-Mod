package metarules.pathing.nwmpaths

import metarules._
import pathing._
import Bezier._
import meta._, module.syntax._, Network._, module.NetworkProperties
import scdbpf._, Sc4Path.{TransportType => TT, _}, Cardinal._, DbpfUtil.RotFlip._
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
    val ids = scala.collection.mutable.Map.empty[Int, Sc4Path]
    // TODO provisional
    val mainNetworks = NwmNetworks -- Set(Ave8)
    val minorNetworks = NwmNetworks ++ Set(Road, Street, Onewayroad, Avenue) -- Set(Ave8)
    for {
      main <- mainNetworks
      minor <- minorNetworks
    } /*do*/ {
      import Flags._, Implicits._
      def add(seg1: Segment, seg2: Segment) = {
        if (!seg1.network.isTla && !seg2.network.isTla) {
          val idTile = resolver(seg1 & seg2)
          if (!ids.contains(idTile.id)) {
            val intersection = new PlusIntersection(seg1, seg2)
            ids(idTile.id) = intersection.buildSc4Path * (R0F0 / idTile.rf)
          }
        } else {
          // special handling for center turning lanes of TLAs
          // TODO Orientations and directions of paths need testing,
          // and the alternative TLA turn paths need permanent IIDs
          val idTile1 = resolver(NetworkProperties.projectTlaLeft(seg1 & seg2))
          val idTile2 = resolver(NetworkProperties.projectTlaLeft((seg1 & seg2) * R0F1))
          if (!ids.contains(idTile1.id)) {
            val intersection = new PlusIntersection(seg1, seg2)
            ids(idTile1.id) = intersection.buildSc4Path * (R0F0 / idTile1.rf)
          }
          if (!ids.contains(idTile2.id)) {
            val intersection = new PlusIntersection(seg1 * R0F1, seg2 * R0F1)
            val stop = intersection.buildSc4Path * (R0F0 / idTile2.rf)
            ids(idTile2.id) = stop.copy(stopPaths = stop.stopPaths.map(p => p.copy(uk = !p.uk)))  // flip uk flag to account for mirroring
          }
        }
      }
      // In the following, it is important to choose the same directions as in
      // the IID scheme, since otherwise the uk flags can end up flipped.
      for { // OxD
        mainDir <- Seq(NS, SN)
        minDir <- Seq(SW, if (minor.typ != AvenueLike) WS else SharedDiagLeft)
      } /*do*/ {
        add(main~mainDir, minor~minDir)
      }
      for { // DxO, DxD
        mainDir <- Seq(ES, if (main.typ != AvenueLike) SE else SharedDiagRight)
        minDir <- Seq(EW, WE, SW, if (minor.typ != AvenueLike) WS else SharedDiagLeft)
      } /*do*/ {
        add(main~mainDir, minor~minDir)
      }
    }
    ids.map { case (id, p) =>
      require(p.validateClassNumbers, "duplicate class numbers detected in " + p.toString)
      BufferedEntry(tgi = Tgi(0,0,id).copy(Tgi.Sc4Path2d), content = p.copy(decFormat = Some(Sc4Path.threeDecimals)), compressed = true)
    }
  }
}
