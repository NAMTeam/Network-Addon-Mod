package metarules.pathing.nwmpaths

import metarules._
import pathing._, Bezier._
import scdbpf.Sc4Path.Cardinal, Cardinal._, scdbpf.DbpfUtil.RotFlip._, scdbpf.Sc4Path.{TransportType => TT}
import PathCreator.{SPath, SPaths}

abstract class CommonIntersection extends Intersection {

  /** the line at which traffic coming from direction c stops */
  protected def stopLine(c: Cardinal): Line

  protected def leftTurnPaths(c: Cardinal, tt: TT): SPaths // in order left to right
  protected def straightPaths(c: Cardinal, tt: TT): SPaths // in any order
  protected def rightTurnPaths(c: Cardinal, tt: TT): SPaths // in order right to left
  protected def iterateMergePathsFromRight(c: Cardinal, tt: TT): SPaths // in order right to left
  protected def iterateMergePathsFromLeft(c: Cardinal, tt: TT): SPaths // in order left to right

  def yieldConnections(tt: TT): TraversableOnce[Connection] = {
    def buildTurnPaths(left: Boolean) = for {
      fromDir <- Seq(West, North, East, South)
      (fromPath, toPath) <- if (left) leftTurnPaths(fromDir, tt) zip iterateMergePathsFromLeft(fromDir *: R3F0, tt)
                            else rightTurnPaths(fromDir, tt) zip iterateMergePathsFromRight(fromDir *: R1F0, tt)
      start = stopLine(fromDir)
      end = stopLine(fromDir *: (if (left) R1F0 else R3F0))
    } yield {
      val from = Line(fromPath.points(0), fromPath.points(1))
      val to = { val last = toPath.points.takeRight(2); Line(last(0), last(1)) }
      CurvedConnection(from, start, to, end)
    }
    def buildStraightPaths = for {
      fromDir <- Seq(West, North, East, South)
      path <- straightPaths(fromDir, tt)
    } yield StraightConnection(Line(path.points(0), path.points(1)))
    buildStraightPaths ++ buildTurnPaths(false) ++ buildTurnPaths(true)
  }
}

import meta.Segment

class PlusIntersection(major: Segment, minor: Segment) extends CommonIntersection {
  private[this] val sortedPaths: Map[Cardinal, SPaths] =
    NetworkConfig.straightPaths(major, minor).groupBy(_.dir) mapValues (_ sortWith PlusIntersection.rightToLeftSorter)
  private[this] def network(c: Cardinal) = if (c == North || c == South) major.network else minor.network
  private[this] def hasTurningLane(c: Cardinal) = network(c).isTla
  private[this] def isBidirectionalOneway(c: Cardinal) = {
    val n = network(c)
    n == meta.Network.Onewayroad || n.base.exists(_ == meta.Network.Onewayroad)
  }

  protected def rightTurnPaths(c: Cardinal, tt: TT) = if (tt == TT.Sim || tt == TT.Car) {
    sortedPaths(c).find(_.tt == tt).toSeq
  } else Nil
  protected def leftTurnPaths(c: Cardinal, tt: TT) = if (tt == TT.Car) {
    sortedPaths(c).reverseIterator.find(_.tt == tt).toSeq
  } else Nil
  protected def straightPaths(c: Cardinal, tt: TT) = {
    val paths = sortedPaths(c).filter(_.tt == tt)
    if (tt != TT.Car) paths
    else if (hasTurningLane(c)) paths.dropRight(1) // TLA networks have one thru-lane less
    else if (isBidirectionalOneway(c)) paths.dropRight(paths.length / 2) // OWR networks have duplicated thru-lanes, we need only half of them
    else paths
  }

  protected def iterateMergePathsFromRight(c: Cardinal, tt: TT) = sortedPaths(c).filter(_.tt == tt)
  protected def iterateMergePathsFromLeft(c: Cardinal, tt: TT) = {
    val paths = sortedPaths(c).reverse.filter(_.tt == tt)
    if (tt == TT.Car && hasTurningLane(c)) paths.tail // don't merge into turning lane
    else paths
  }

  protected def stopLine(c: Cardinal) = {
    val dir = c *: R1F0
    val path = sortedPaths(dir).find(_.tt == TT.Sim).get
    Line(path.points(0), path.points(1))
  }
}
private object PlusIntersection {
  val rightToLeftSorter: (SPath, SPath) => Boolean = { case (path1, path2) =>
    val line1 = Line(path1.points(0), path1.points(1))
    line1.side(path2.points(0)) == 1 // side 1 means point is on left side, hence path1 < path2 meaning to the right
  }
}
