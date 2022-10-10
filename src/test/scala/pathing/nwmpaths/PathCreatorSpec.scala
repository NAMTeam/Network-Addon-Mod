package metarules.pathing.nwmpaths

import scala.collection.immutable.{StringOps}

import metarules._
import pathing._
import org.scalatest.{Matchers, WordSpec}
import Bezier._
import scdbpf._, Sc4Path._, DbpfUtil.RotFlip._
import meta.Network._, meta.Flags._
import PathCreator._
import Trimming.trimToTile
import NetworkConfig._

class PathCreatorSpec extends WordSpec with Matchers {

  // yields input for plotting with SAGE
  def show(pss: Seq[(Seq[Point],String)]): String = pss.map { case (ps, col) => listPlot(ps, col) } .mkString("show(", "+", ",aspect_ratio=1)")
  private def pyList(ps: Seq[Point]): String = ps.map(c => s"(${c.x},${c.y})").mkString("[", ",", "]")
  private def listPlot(ps: Seq[Point], col: String): String = s"list_plot(${pyList(ps)},plotjoined=True,color='$col',marker='.')"

  def pathToPoints(path: Sc4Path): Seq[(Points,String)] = path.paths map (p => (p.coords map coordToPoint, if (p.transportType == TransportType.Sim) "red" else "blue"))
  def printInColumns(n: Int, text: String) = {
    val lines = new StringOps(text).lines.toSeq
    val blockSize = (lines.length + n - 1) / n
    for (tokens <- lines.padTo(blockSize * n, "").grouped(blockSize).toSeq.transpose) {
      println(tokens.map(_.padTo(40,' ')).mkString(""))
    }
  }
  def display(path: Sc4Path) = {
    printInColumns(5, path.toString)
    println('\n' + show(pathToPoints(path)) + '\n')
  }

  val sc4p = new PlusIntersection(Ard3~NS, Ard3~WE).buildSc4Path
  display(sc4p)
}
