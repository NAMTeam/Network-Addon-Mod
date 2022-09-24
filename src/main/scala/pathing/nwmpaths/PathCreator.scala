package metarules.pathing.nwmpaths

import metarules._
import pathing._
import Bezier._
import meta._, Network._
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
    for {
      main <- NwmNetworks if main.typ != AvenueLike && main.base.get == Road && main <= Rd6
      minor <- NwmNetworks if minor.typ != AvenueLike && minor.base.get == Road && minor <= Rd6
    } /*do*/ {
      import Flags._, Implicits._
      def add(seg1: Segment, seg2: Segment) = {
        val idTile = resolver(seg1 & seg2)
        if (!ids.contains(idTile.id)) {
          val intersection = new PlusIntersection(seg1, seg2)
          ids(idTile.id) = intersection.buildSc4Path * (R0F0 / idTile.rf)
        }
      }
      for (mainDir <- Seq(NS, SN, NE, EN); minDir <- Seq(EW, WE, ES, SE)) {
        add(main~mainDir, minor~minDir)
      }
    }
    ids.map { case (id, p) =>
      BufferedEntry(tgi = Tgi(0,0,id).copy(Tgi.Sc4Path2d), content = p, compressed = true)
    }
  }
}
