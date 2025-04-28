package com.sc4nam.module

import io.github.memo33.metarules.meta._, syntax._, Network._

object NetworkProperties {

  val hasRightShoulder: Network => Boolean =
    Set[Network](Rhw6cm, L1Rhw6cm, L2Rhw6cm, Rhw8sm, L1Rhw8sm, L2Rhw8sm, Ave6m, Tla7m).andThen(!_)

  def hasLeftShoulder(n: Network): Boolean = {
    n.typ != AvenueLike &&
    !(n.typ == Symmetrical && hasRightShoulder(n)) && // this is treated as right shoulder only, for efficiency
    !(n >= Rhw8s && n <= L2Rhw10c) &&
    !(n >= Tla5 && n <= Ave6m)
  }

  def isDoubleTile(n: Network): Boolean = {
    n.typ == AvenueLike || n >= Rhw8sm && n <= L2Rhw12s || n >= Tla5 && n <= Rd6
  }
  def isTripleTile(n: Network): Boolean = {
    n >= Rhw6cm && n <= L2Rhw10c || n >= Ave6 && n <= Ave6m
  }
  def isSingleTile(n: Network): Boolean = !isDoubleTile(n) && !isTripleTile(n)

  def hasMiniCurve(n: Network, inside: Boolean): Boolean = {
    inside && (n >= Rhw8s && n <= L2Rhw10c && (n < Rhw6cm || n > L2Rhw6cm)) ||
    !inside && (n >= Rhw8sm && n <= L2Rhw8sm) ||
    n == Ave6m || n == Tla7m
  }

  def hasExtendedCurve(n: Network, inside: Boolean): Boolean = {
    (n.isRhw && n > L4Rhw6s && n <= L2Rhw10c) && !hasMiniCurve(n, inside) ||
    !inside && (n == Ave6 || n == Ave8)
  }

  val isRhwShoulder = Network.ValueSet(
    Rhw8s, L1Rhw8s, L2Rhw8s, Rhw10s, L1Rhw10s, L2Rhw10s, Rhw12s, L1Rhw12s, L2Rhw12s,
    Rhw6c, L1Rhw6c, L2Rhw6c, Rhw8c, L1Rhw8c, L2Rhw8c, Rhw10c, L1Rhw10c, L2Rhw10c)

  val isRhwShoulderMedian = Network.ValueSet(Rhw8sm, L1Rhw8sm, L2Rhw8sm)

  // currently, RHW only
  val ground: Map[Network, Network] = RhwNetworks.rangeFrom(L1Rhw2).iterator.scanLeft(Dirtroad -> Dirtroad) { case ((prev, base), n) =>
    if (n.height > prev.height) n -> base else n -> n
  }.toMap

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
    require(n.height == height, s"$ground does not have height level $height")
    n
  }

  def isHrw(n: Network): Boolean = Hrw <= n && n <= L2Hrw

  private def rhwIntersectionAllowed(rhw: Network, any: Network): Boolean = {
    if (!rhw.isRhw) {
      assert(any.isRhw)
      rhwIntersectionAllowed(any, rhw)
    } else {
      if (rhw.height == 0 && isSingleTile(rhw) && List(L1Dtr, L2Dtr).contains(any)) true
      else if (!RhwResolver.rhwPieceId.contains(any)) false
      else if (rhw.height != any.height) true
      else if (Viaducts.contains(any) && rhw <= L2Rhw4) true
      else if (rhw.height != 0) false
      else if (any > rhw && any.isRhw) rhwIntersectionAllowed(any, rhw)
      else {
        rhw == Dirtroad && (any == Dirtroad || GlrNetworks.contains(any)) ||
        rhw == Rhw3 && any == Dirtroad ||
        rhw == Mis && (any == Dirtroad || any == Rhw3) ||
        rhw == Rhw4 && any == Dirtroad ||
        rhw <= Rhw4 && (any < Dirtroad || any == Str)
      }
    }
  }

  private val nonintersectingNetworks = (Groundhighway + Highway + Lightrail + Monorail + Subway) ++
    (Hsr + L2Hsr) ++ (OverrideNetworks rangeFrom L1Dtr rangeTo L2StrAlt) ++ (OverrideNetworks rangeFrom Hrw rangeTo L2Hrw)

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
  def unprojectTla(t: Tile): Tile = projectTla(t, (flags: Flags) => {
    val newMf = flags.manifest match {
      case Flag.LeftSpinBi => Flag.Bi
      case Flag.RightSpinBi => Flag.Bi
      case Flag.LeftSpinInOut => Flag.InOut
      case Flag.RightSpinInOut => Flag.InOut
      case Flag.Bi => Flag.Bi
      case Flag.InOut => Flag.InOut
    }
    Flags(flags, newMf)
  })

  val nonMirroredOnly: group.Quotient => Set[RotFlip] = _.filter(!_.flipped)
  val mirroredOnly: group.Quotient => Set[RotFlip] = _.filter(_.flipped)
}
