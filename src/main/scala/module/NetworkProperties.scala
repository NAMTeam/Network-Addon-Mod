package metarules.module

import metarules.meta._, syntax._, Network._

object NetworkProperties {

  val hasRightShoulder: Network => Boolean =
    Set[Network](Rhw6cm, L1Rhw6cm, L2Rhw6cm, Rhw8sm, L1Rhw8sm, L2Rhw8sm, Ave6m, Tla7m).andThen(!_)

  def hasLeftShoulder(n: Network): Boolean = {
    n.typ != AvenueLike &&
    !(n.typ == Symmetrical && hasRightShoulder(n)) && // this is treated as right shoulder only, for efficiency
    !(n >= Rhw8s && n <= L2Rhw10c) &&
    !(n >= Tla5 && n <= Ave6m)
  }

  def hasOverhang(n: Network): Boolean = {
    Set[Network](Rhw6s, L1Rhw6s, L2Rhw6s).contains(n)
  } 

  def isDoubleTile(n: Network): Boolean = {
    n.typ == AvenueLike || n >= Rhw8sm && n <= L2Rhw12s || n >= Tla5 && n <= Rd6
  }
  def isTripleTile(n: Network): Boolean = {
    n >= Rhw6cm && n <= L2Rhw10c || n >= Ave6 && n <= Ave6m
  }
  def isSingleTile(n: Network): Boolean = !isDoubleTile(n) && !isTripleTile(n)

  // currently, RHW only
  val ground: Map[Network, Network] = RhwNetworks.from(L1Rhw2).scanLeft(Dirtroad -> Dirtroad) { case ((prev, base), n) =>
    if (n.height > prev.height) n -> base else n -> n
  } (collection.breakOut)

  // currently, RHW only
  def atHeight(ground: Network, height: Int) = {
    require(ground.isRhw && ground.height == 0)
    val n = if (ground != Dirtroad) {
      Network(ground.id + height)
    } else if (height == 0) {
      Dirtroad
    } else {
      Network(L1Rhw2.id - 1 + height)
    }
    require(n.height == height, ground + " does not have height level " + height)
    n
  }

  def isHrw(n: Network): Boolean = Hrw <= n && n <= L2Hrw

  private def rhwIntersectionAllowed(rhw: Network, any: Network): Boolean = {
    if (!rhw.isRhw) {
      assert(any.isRhw)
      rhwIntersectionAllowed(any, rhw)
    } else {
      if (!RhwResolver.rhwPieceId.contains(any)) false
      else if (rhw.height != any.height) true
      else if (rhw.height != 0) false
      else if (any > rhw && any.isRhw) rhwIntersectionAllowed(any, rhw)
      else {
        rhw == Dirtroad && any == Dirtroad ||
        rhw == Rhw3 && any == Dirtroad ||
        rhw == Mis && (any == Dirtroad || any == Rhw3) ||
        rhw == Rhw4 && any == Dirtroad ||
        rhw <= Rhw4 && any < Dirtroad
      }
    }
  }

  private val nonintersectingNetworks = (Groundhighway + Highway + Lightrail + Monorail + Subway) ++
    (Hsr + L2Hsr) ++ (OverrideNetworks from L1Dtr to L2StrAlt) ++ (OverrideNetworks from Hrw to L2Hrw)

  def intersectionAllowed(a: Network, b: Network): Boolean = {
    if (BaseNetworks.contains(a) && BaseNetworks.contains(b)) {
      true  // intersections between base networks are always allowed so that override networks can override them
    } else if (a.isRhw || b.isRhw) {
      rhwIntersectionAllowed(a, b)
    } else if ((nonintersectingNetworks.contains(a) || nonintersectingNetworks.contains(b)) && a.height == b.height) {
      false
    } else {
      true // TODO
    }
  }

  /** Returns whether intersections of these networks have paths turning from
    * one to the other network (assuming intersections of the two networks are allowed).
    */
  def hasTurnPaths(a: Network, b: Network): Boolean = {
    if (a.height != b.height) false
    else RoadNetworks.contains(a) && RoadNetworks.contains(b)
  }

  private def projectTla(t: Tile, p: Flags => Flags): Tile = t.copy(segs =
    t.segs.map(s => if (!s.network.isTla) s else s.copy(flags = p(s.flags)))
    )
  val projectTlaLeft = (t: Tile) => projectTla(t, _.spinLeft)
  val projectTlaRight = (t: Tile) => projectTla(t, _.spinRight)

  val nonMirroredOnly: group.Quotient => Set[RotFlip] = _.filter(!_.flipped)
  val mirroredOnly: group.Quotient => Set[RotFlip] = _.filter(_.flipped)
}
