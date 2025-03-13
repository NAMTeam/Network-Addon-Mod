package com.sc4nam.module

import io.github.memo33.metarules.meta.{NetworkType, Symmetrical, Asymmetrical, AvenueLike, Syntax}
import io.github.memo33.scalaenum

object syntax extends Syntax {

class Network private (val height: Int, val typ: NetworkType, val base: Option[Network]) extends Network.Val with AbstractNetwork {
  import Network._
  def isRhw: Boolean = RhwNetworks.contains(this)
  def isNwm: Boolean = NwmNetworks.contains(this)
  def isTla: Boolean = this == Tla3 || this == Tla5 || this == Tla7m
  def isSymm: Boolean = typ == Symmetrical
}

/** List of all the base and override networks.
  *
  * If you add or change anything here, note that the order of the networks is
  * significant -- and some code in the resolvers and rule generators depends on
  * this (e.g. whenever networks are compared using < or > relations).
  */
object Network extends scalaenum.Enum {
  type Value = Network

  /* base networks */
  val Road          = new Network(0, Symmetrical, None)
  val Rail          = new Network(0, Symmetrical, None)
  val Highway       = new Network(2, AvenueLike,  None)
  val Street        = new Network(0, Symmetrical, None)
  val Avenue        = new Network(0, AvenueLike,  None)
  val Lightrail     = new Network(2, Symmetrical, None)
  val Monorail      = new Network(2, Symmetrical, None)
  val Onewayroad    = new Network(0, Symmetrical, None)
  val Dirtroad      = new Network(0, Symmetrical, None)
  val Groundhighway = new Network(0, AvenueLike,  None)
  val Subway        = new Network(0, Symmetrical, None)

  /* override networks */
  // RAM/RRW networks
  val Str = new Network(0, Symmetrical, Some(Rail))
  //val Ttr = new Network(0, Symmetrical, Some(Rail))
  //val Qtr = new Network(0, Symmetrical, Some(Rail))

  // GLR networks
  val Glr1 = new Network(0, Symmetrical, Some(Lightrail))
  val Glr2 = new Network(0, Symmetrical, Some(Lightrail))
  val Glr3 = new Network(0, Symmetrical, Some(Lightrail))
  val Glr4 = new Network(0, Symmetrical, Some(Lightrail))

  // HSR networks
  val   Hsr = new Network(0, Symmetrical, Some(Monorail))
  val L2Hsr = new Network(2, Symmetrical, Some(Monorail))

  // RHW networks
  val L1Rhw2   = new Network(1, Symmetrical,  Some(Dirtroad))
  val L2Rhw2   = new Network(2, Symmetrical,  Some(Dirtroad))
  val   Rhw3   = new Network(0, Asymmetrical, Some(Dirtroad))
  val L1Rhw3   = new Network(1, Asymmetrical, Some(Dirtroad))
  val L2Rhw3   = new Network(2, Asymmetrical, Some(Dirtroad))
  val   Mis    = new Network(0, Asymmetrical, Some(Dirtroad))
  val L1Mis    = new Network(1, Asymmetrical, Some(Dirtroad))
  val L2Mis    = new Network(2, Asymmetrical, Some(Dirtroad))
  val L3Mis    = new Network(3, Asymmetrical, Some(Dirtroad))
  val L4Mis    = new Network(4, Asymmetrical, Some(Dirtroad))
  val   Rhw4   = new Network(0, Asymmetrical, Some(Dirtroad))
  val L1Rhw4   = new Network(1, Asymmetrical, Some(Dirtroad))
  val L2Rhw4   = new Network(2, Asymmetrical, Some(Dirtroad))
  val L3Rhw4   = new Network(3, Asymmetrical, Some(Dirtroad))
  val L4Rhw4   = new Network(4, Asymmetrical, Some(Dirtroad))
  val   Rhw6s  = new Network(0, Asymmetrical, Some(Dirtroad))
  val L1Rhw6s  = new Network(1, Asymmetrical, Some(Dirtroad))
  val L2Rhw6s  = new Network(2, Asymmetrical, Some(Dirtroad))
  val L3Rhw6s  = new Network(3, Asymmetrical, Some(Dirtroad))
  val L4Rhw6s  = new Network(4, Asymmetrical, Some(Dirtroad))
  val   Rhw8sm = new Network(0, Asymmetrical, Some(Dirtroad))
  val L1Rhw8sm = new Network(1, Asymmetrical, Some(Dirtroad))
  val L2Rhw8sm = new Network(2, Asymmetrical, Some(Dirtroad))
  val   Rhw8s  = new Network(0, Asymmetrical, Some(Dirtroad))
  val L1Rhw8s  = new Network(1, Asymmetrical, Some(Dirtroad))
  val L2Rhw8s  = new Network(2, Asymmetrical, Some(Dirtroad))
  val   Rhw10s = new Network(0, Asymmetrical, Some(Dirtroad))
  val L1Rhw10s = new Network(1, Asymmetrical, Some(Dirtroad))
  val L2Rhw10s = new Network(2, Asymmetrical, Some(Dirtroad))
  val   Rhw12s = new Network(0, Asymmetrical, Some(Dirtroad))
  val L1Rhw12s = new Network(1, Asymmetrical, Some(Dirtroad))
  val L2Rhw12s = new Network(2, Asymmetrical, Some(Dirtroad))
  val   Rhw6cm = new Network(0, Symmetrical,  Some(Dirtroad))
  val L1Rhw6cm = new Network(1, Symmetrical,  Some(Dirtroad))
  val L2Rhw6cm = new Network(2, Symmetrical,  Some(Dirtroad))
  val   Rhw6c  = new Network(0, Asymmetrical, Some(Dirtroad))
  val L1Rhw6c  = new Network(1, Asymmetrical, Some(Dirtroad))
  val L2Rhw6c  = new Network(2, Asymmetrical, Some(Dirtroad))
  val   Rhw8c  = new Network(0, Asymmetrical, Some(Dirtroad))
  val L1Rhw8c  = new Network(1, Asymmetrical, Some(Dirtroad))
  val L2Rhw8c  = new Network(2, Asymmetrical, Some(Dirtroad))
  val   Rhw10c = new Network(0, Asymmetrical, Some(Dirtroad))
  val L1Rhw10c = new Network(1, Asymmetrical, Some(Dirtroad))
  val L2Rhw10c = new Network(2, Asymmetrical, Some(Dirtroad))

  // NWM networks
  val Tla3  = new Network(0, Symmetrical,  Some(Road))
  val Ave2  = new Network(0, Symmetrical,  Some(Road))
  val Ard3  = new Network(0, Asymmetrical, Some(Road))
  val Owr1  = new Network(0, Symmetrical,  Some(Onewayroad))
  val Owr3  = new Network(0, Symmetrical,  Some(Onewayroad))
  val Nrd4  = new Network(0, Symmetrical,  Some(Road))
  val Tla5  = new Network(0, Asymmetrical, Some(Road))
  val Owr4  = new Network(0, AvenueLike,   Some(Onewayroad))
  val Owr5  = new Network(0, Asymmetrical, Some(Onewayroad))
  val Rd4   = new Network(0, AvenueLike,   Some(Avenue))
  val Rd6   = new Network(0, Asymmetrical, Some(Road))
  val Ave6  = new Network(0, Asymmetrical, Some(Road))
  val Tla7m = new Network(0, Symmetrical,  Some(Road))
  val Ave8  = new Network(0, Asymmetrical, Some(Road))
  val Ave6m = new Network(0, Symmetrical,  Some(Road))

  // SAM networks
  val Sam1, Sam2, Sam3, Sam4, Sam5, Sam6, Sam7, Sam8, Sam9, Sam10, Sam11 = new Network(0, Symmetrical, Some(Street))

  // Roundabout networks
  val RdRndbt = new Network(0, Asymmetrical, Some(Road))
  
  // RRW networks
  val L1Dtr    = new Network(1, Symmetrical, Some(Rail))
  val L2Dtr    = new Network(2, Symmetrical, Some(Rail))
  val L1Str    = new Network(1, Symmetrical, Some(Rail))
  val L2Str    = new Network(2, Symmetrical, Some(Rail))
  val L1DtrAlt = new Network(1, Symmetrical, Some(Rail))
  val L2DtrAlt = new Network(2, Symmetrical, Some(Rail))
  val L1StrAlt = new Network(1, Symmetrical, Some(Rail))
  val L2StrAlt = new Network(2, Symmetrical, Some(Rail))

  // HRW networks
  val Hrw   = new Network(0, Symmetrical, None)
  val L1Hrw = new Network(1, Symmetrical, None)
  val L2Hrw = new Network(2, Symmetrical, None)

  // viaducts
  val L1Road        = new Network(1, Symmetrical, Some(Road))
  val L2Road        = new Network(2, Symmetrical, Some(Road))
  val L1Onewayroad  = new Network(1, Symmetrical, Some(Onewayroad))
  val L2Onewayroad  = new Network(2, Symmetrical, Some(Onewayroad))
  val L1Avenue      = new Network(1, AvenueLike,  Some(Avenue))
  val L2Avenue      = new Network(2, AvenueLike,  Some(Avenue))

  // network collections
  val BaseNetworks: ValueSet = Network.values rangeFrom Road rangeTo Subway
  val OverrideNetworks: ValueSet = Network.values.filter(_.base.isDefined)
  val GlrNetworks: ValueSet = Network.values rangeFrom Glr1 rangeTo Glr4
  val RhwNetworks: ValueSet = ValueSet(Dirtroad) ++ (Network.values rangeFrom L1Rhw2 rangeTo L2Rhw10c)
  val NwmNetworks: ValueSet = Network.values rangeFrom Tla3 rangeTo Ave6m
  val SamNetworks: ValueSet = Network.values rangeFrom Sam1 rangeTo Sam11
  val Viaducts: ValueSet = Network.values rangeFrom L1Road rangeTo L2Avenue
  val RoadNetworks: ValueSet = RhwNetworks ++ NwmNetworks ++ SamNetworks ++ Viaducts + Road + Highway + Street + Avenue + Onewayroad + Groundhighway

}

}  // end of syntax
