package metarules.module

import metarules.meta._, Network._

object NetworkProperties {

  val hasRightShoulder: Network => Boolean =
    Set[Network](Rhw6cm, L1Rhw6cm, L2Rhw6cm, Rhw8sm, L1Rhw8sm, L2Rhw8sm, Ave6m, Tla7m).andThen(!_)

  def hasLeftShoulder(n: Network): Boolean = {
    n.typ != AvenueLike &&
    !(n.typ == Symmetrical && !hasRightShoulder(n)) && // this is treated as right shoulder only, for efficiency
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

}
