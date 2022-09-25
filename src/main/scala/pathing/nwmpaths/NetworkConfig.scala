package metarules.pathing.nwmpaths

import metarules._
import meta._, Network._, Flags._
import scdbpf.Sc4Path.Cardinal._, scdbpf.DbpfUtil.RotFlip._
import scdbpf.Sc4Path.{TransportType => TT}
import pathing._
import PathCreator.{SPath, SPaths}
import Bezier.{Point, Dec}

object NetworkConfig {

  def straightPaths(major: Segment, minor: Segment): SPaths = majorPaths(major) ++ minorPaths(minor)

  /** The "fuzzy" direction needs to be adjusted, depending on whether network
    * is main or minor crossing network. Main will always be treated in
    * North-South direction, whereas the minor crossing network runs in
    * East-West direction.
    */
  private def majorPaths(s: Segment): SPaths = simplePaths(s) map { ps =>
    if (ps.dir == East) ps.copy(dir = South)
    else if (ps.dir == West) ps.copy(dir = North)
    else ps
  }

  /** @see majorPaths
    */
  private def minorPaths(s: Segment): SPaths = simplePaths(s) map { ps =>
    if (ps.dir == North) ps.copy(dir = East)
    else if (ps.dir == South) ps.copy(dir = West)
    else ps
  }

  private val simplePaths: Map[Segment, SPaths] = {
    val m = scala.collection.mutable.Map.empty[Segment, SPaths]

    val orth: (TT, Dec, Boolean) => SPath = { (tt, x, rev) =>
      val ps = Seq(Point(x, 100, 0), Point(x, -100, 0))
      if (rev) SPath(tt, South, ps.reverse) else SPath(tt, North, ps)
    }
    def diag(sign: Int): (TT, Dec, Boolean) => SPath = { (tt, x, rev) =>
      val ps = Seq(Point(108,100-x*sign,0), Point(-100+x*sign,-108,0))
      val (dir, qs) = if ((sign == 1) == rev) (South, ps.reverse) else (North, ps)
      SPath(tt, dir, qs)
    }
    val diagES = diag(1)
    val diagSE = diag(-1)

    def create(func: (TT, Dec, Boolean) => SPath, simNS: Seq[Dec], carNS: Seq[Dec], carSN: Seq[Dec], simSN: Seq[Dec]): SPaths = {
      simNS.map(func(TT.Sim, _, false)) ++
      carNS.map(func(TT.Car, _, false)) ++
      carSN.map(func(TT.Car, _, true)) ++
      simSN.map(func(TT.Sim, _, true))
    }

    def add(n: Network, dirs: IntFlags*)(simNS: Dec*)(carNS: Dec*)(carSN: Dec*)(simSN: Dec*): Unit = for (dir <- dirs) {
      val func = dir match {
        case NS => orth
        case ES => diagES
        case SE => diagSE
      }
      val spaths = create(func, simNS, carNS, carSN, simSN)
      for (rf <- Seq(R0F0, R1F0, R2F0, R3F0)) {
        val seg = n~dir * rf
        m.getOrElseUpdate(seg, spaths map (_ * rf))
      }
    }

    add(Road, NS)(-6.5f)(-2.5f)(2.5f)(6.5f)
    add(Road, ES)(-7.3f)(-3.2f)(3.2f)(7.3f)
    add(Onewayroad, NS)(-6.5f)(-2.5f, 2.5f)(-2.5f, 2.5f)(6.5f)
    add(Onewayroad, ES)(-7.3f)(-3.2f, 3.2f)(-3.2f, 3.2f)(7.3f)
    add(Street, NS)(-7f)(-2f)(2f)(7f)
    add(Street, ES)(-7.1f)(-2f)(2f)(7.1f)
    add(Avenue, NS)(-6.39f)(-2.5f, 2.5f)(13.5f, 18.5f)(22.39f)
    add(Avenue, ES)(-7.3f)(-3.2f, 3.2f)(16-3.2f, 16+3.2f)(16+7.3f) // TODO shared diagonal

    add(Tla3, NS)(-7.5f)(-4.5f, -0.125f)(0.125f, 4.5f)(7.5f)
    add(Tla3, ES)(-7.99f)(-5.5f, -0.3172f)(0.3172f, 5.5f)(7.99f)
    add(Ave2, NS)(-7.5f)(-4.5f)(4.5f)(7.5f)
    add(Ave2, ES)(-7.99f)(-5.5f)(5.5f)(7.99f)
    add(Ard3, NS)(-7.5f)(-4.5f, 0f)(4.5f)(7.5f)
    add(Ard3, ES, SE)(-7.99f)(-5.25f, 0f)(5.25f)(7.99f)
    add(Owr1, NS)(-6.39f)(0f)(0f)(6.39f)
    add(Owr1, ES, SE)(-7.3f)(0f)(0f)(7.3f)
    add(Owr3, NS)(-7.25f)(-4.5f, 0f, 4.5f)(-4.5f, 0f, 4.5f)(7.25f)
    add(Owr3, ES)(-7.99f)(-5.25f, 0f, 5.25f)(-5.25f, 0f, 5.25f)(7.99f)
    add(Nrd4, NS)(-7.5f)(-5.25f, -1.95f)(1.95f, 5.25f)(7.5f)
    add(Nrd4, ES)(-10.88f)(-7.3f, -2.95f)(2.95f, 7.3f)(10.88f)

    add(Tla5, NS)(-6.39f)(-2.5f, 2.5f, 7.25f)(8.75f, 13.5f, 18.5f)(22.39f)
    add(Tla5, ES, SE)(-4f)(1.5f, 9f, 16-1.5f)(16+1.5f, 23f, 30.5f)(36f) // TODO these are preliminary (guessed) turning lane coordinates
    add(Owr4, NS)(-6.39f)(1.5f, 5.5f, 16-5.5f, 16-1.5f)(1.5f, 5.5f, 16-5.5f, 16-1.5f)(16+6.39f)
    add(Owr4, ES)(-7.3f)(-2f, 5f, 16-5f, 16+2f)(-2f, 5f, 16-5f, 16+2f)(16+7.3f) // TODO shared diagonal
    add(Owr5, NS)(-6.39f)(-1f, 3.5f, 7.75f, 16-7.75f, 16-3.5f, 16+1f)(-1f, 3.5f, 7.75f, 16-7.75f, 16-3.5f, 16+1f)(16+6.39f)
    add(Owr5, ES, SE)(-2.5f)(3f, 16-6.5f, 16-0f, 16+6.5f, 32-3f)(3f, 16-6.5f, 16-0f, 16+6.5f, 32-3f)(32+2.5f)
    add(Rd4, NS)(-7f)(1f, 5.5f)(16-5.5f, 16-1f)(16+7f)
    add(Rd4, ES)(-7.3f)(-2f, 5f)(16-5f, 16+2f)(16+7.3f) // TODO shared diagonal
    add(Rd6, NS)(-7f)(-3.5f, 1f, 5.5f)(10.5f, 15f, 19.5f)(23f)
    add(Rd6, ES, SE)(-5.5f)(0f, 6f, 12f)(20f, 26f, 32f)(37.5f)

    add(Ave6, NS)(-7f)(0.5f, 5.5f, 16-5.5f)(21.5f, 32-5.5f, 32-0.5f)(39f)
    add(Ave6, ES, SE)(1.5f)(16-5.5f, 16+1.5f, 16+7.5f)(48-7.5f, 48-1.5f, 48+5.5f)(64-1.5f)
    add(Ave6m, NS)(-7f-16)(0.5f-16, 5.5f-16, -5.5f)(21.5f-16, 16-5.5f, 16-0.5f)(23f)
    add(Ave6m, ES, SE)(-32+1.5f)(-16-5.5f, -16+1.5f, -16+7.5f)(16-7.5f, 16-1.5f, 16+5.5f)(32-1.5f)
    // add(Tla7, NS)(-7f)(0.5f, 5.5f, 16-5.5f, 16-1.125f)(17.125f, 21.5f, 32-5.5f, 32-0.5f)(39f)  // corresponds to Ave6
    add(Tla7m, NS)(-7f-16)(0.5f-16, 5.5f-16, -5.5f, -1.125f)(17.125f-16, 21.5f-16, 16-5.5f, 16-0.5f)(23f)
    add(Tla7m, ES, SE)(-32+1.5f)(-16-5.5f, -16+1.5f, -16+7.5f, -0.1f)(0.1f, 16-7.5f, 16-1.5f, 16+5.5f)(32-1.5f)

    m.toMap
  }

}
