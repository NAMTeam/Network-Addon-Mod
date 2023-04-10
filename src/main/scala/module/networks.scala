package metarules.module

import metarules.meta.{Symmetrical, Asymmetrical, AvenueLike}

object syntax extends metarules.meta.Syntax {

class Network private (
    val height: Int,
    val typ: metarules.meta.NetworkType,
    val base: Option[Network],
    val rhwPieceId: Option[Int],
    val rhwRangeId: Option[Int]) extends Network.Val with AbstractNetwork {

  import Network._
  def isRhw: Boolean = rhwRangeId.isDefined
  def isNwm: Boolean = this >= Tla3 && this <= Ave6m
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
  val Road          = new Network(0, Symmetrical, None, Some(0x1100), None)
  val Rail          = new Network(0, Symmetrical, None, Some(0x1500), None)
  val Highway       = new Network(2, AvenueLike,  None, Some(0x1420), None)
  val Street        = new Network(0, Symmetrical, None, Some(0x1000), None)
  val Avenue        = new Network(0, AvenueLike,  None, Some(0x1300), None)
  val Lightrail     = new Network(2, Symmetrical, None, Some(0x1720), None)
  val Monorail      = new Network(2, Symmetrical, None, Some(0x1920), None)
  val Onewayroad    = new Network(0, Symmetrical, None, Some(0x1200), None)
  val Dirtroad      = new Network(0, Symmetrical, None, Some(0x1A00), Some(0x57000000))
  val Groundhighway = new Network(0, AvenueLike,  None, Some(0x1400), None)
  val Subway        = new Network(0, Symmetrical, None, None,         None)

  /* override networks */
  // RAM/RRW networks
  val Str = new Network(0, Symmetrical, Some(Rail), Some(0x1505), None)
  //val Ttr = new Network(0, Symmetrical, Some(Rail), Some(0x1600), None)
  //val Qtr = new Network(0, Symmetrical, Some(Rail), Some(0x1605), None)

  // GLR networks
  val Glr1 = new Network(0, Symmetrical, Some(Lightrail), Some(0x1700), None)
  val Glr2 = new Network(0, Symmetrical, Some(Lightrail), Some(0x1800), None)
  val Glr3 = new Network(0, Symmetrical, Some(Lightrail), Some(0x1705), None)
  val Glr4 = new Network(0, Symmetrical, Some(Lightrail), Some(0x1805), None)

  // HSR networks
  val   Hsr = new Network(0, Symmetrical, Some(Monorail), Some(0x1905), None)
  val L2Hsr = new Network(2, Symmetrical, Some(Monorail), Some(0x1925), None)

  // RHW networks
  val L1Rhw2   = new Network(1, Symmetrical,  Some(Dirtroad), Some(0x1A10), Some(0x57100000))
  val L2Rhw2   = new Network(2, Symmetrical,  Some(Dirtroad), Some(0x1A20), Some(0x57200000))
  val   Rhw3   = new Network(0, Asymmetrical, Some(Dirtroad), Some(0x1B00), Some(0x57010000))
  val L1Rhw3   = new Network(1, Asymmetrical, Some(Dirtroad), Some(0x1B10), Some(0x57110000))
  val L2Rhw3   = new Network(2, Asymmetrical, Some(Dirtroad), Some(0x1B20), Some(0x57210000))
  val   Mis    = new Network(0, Asymmetrical, Some(Dirtroad), Some(0x1C00), Some(0x57020000))
  val L1Mis    = new Network(1, Asymmetrical, Some(Dirtroad), Some(0x1C10), Some(0x57120000))
  val L2Mis    = new Network(2, Asymmetrical, Some(Dirtroad), Some(0x1C20), Some(0x57220000))
  val L3Mis    = new Network(3, Asymmetrical, Some(Dirtroad), Some(0x1C30), Some(0x57320000))
  val L4Mis    = new Network(4, Asymmetrical, Some(Dirtroad), Some(0x1C40), Some(0x57420000))
  val   Rhw4   = new Network(0, Asymmetrical, Some(Dirtroad), Some(0x1D00), Some(0x57030000))
  val L1Rhw4   = new Network(1, Asymmetrical, Some(Dirtroad), Some(0x1D10), Some(0x57130000))
  val L2Rhw4   = new Network(2, Asymmetrical, Some(Dirtroad), Some(0x1D20), Some(0x57230000))
  val L3Rhw4   = new Network(3, Asymmetrical, Some(Dirtroad), Some(0x1D30), Some(0x57330000))
  val L4Rhw4   = new Network(4, Asymmetrical, Some(Dirtroad), Some(0x1D40), Some(0x57430000))
  val   Rhw6s  = new Network(0, Asymmetrical, Some(Dirtroad), Some(0x1E00), Some(0x57040000))
  val L1Rhw6s  = new Network(1, Asymmetrical, Some(Dirtroad), Some(0x1E10), Some(0x57140000))
  val L2Rhw6s  = new Network(2, Asymmetrical, Some(Dirtroad), Some(0x1E20), Some(0x57240000))
  val L3Rhw6s  = new Network(3, Asymmetrical, Some(Dirtroad), Some(0x1E30), Some(0x57340000))
  val L4Rhw6s  = new Network(4, Asymmetrical, Some(Dirtroad), Some(0x1E40), Some(0x57440000))
  val   Rhw8sm = new Network(0, Asymmetrical, Some(Dirtroad), Some(0x1F00), Some(0x57050080))
  val L1Rhw8sm = new Network(1, Asymmetrical, Some(Dirtroad), Some(0x1F10), Some(0x57150080))
  val L2Rhw8sm = new Network(2, Asymmetrical, Some(Dirtroad), Some(0x1F20), Some(0x57250080))
  val   Rhw8s  = new Network(0, Asymmetrical, Some(Dirtroad), Some(0x2000), Some(0x57050000))
  val L1Rhw8s  = new Network(1, Asymmetrical, Some(Dirtroad), Some(0x2010), Some(0x57150000))
  val L2Rhw8s  = new Network(2, Asymmetrical, Some(Dirtroad), Some(0x2020), Some(0x57250000))
  val   Rhw10s = new Network(0, Asymmetrical, Some(Dirtroad), Some(0x2100), Some(0x57060000))
  val L1Rhw10s = new Network(1, Asymmetrical, Some(Dirtroad), Some(0x2110), Some(0x57160000))
  val L2Rhw10s = new Network(2, Asymmetrical, Some(Dirtroad), Some(0x2120), Some(0x57260000))
  val   Rhw12s = new Network(0, Asymmetrical, Some(Dirtroad), Some(0x2200), Some(0x57070000))
  val L1Rhw12s = new Network(1, Asymmetrical, Some(Dirtroad), Some(0x2210), Some(0x57170000))
  val L2Rhw12s = new Network(2, Asymmetrical, Some(Dirtroad), Some(0x2220), Some(0x57270000))
  val   Rhw6cm = new Network(0, Symmetrical,  Some(Dirtroad), Some(0x2300), Some(0x57080080))
  val L1Rhw6cm = new Network(1, Symmetrical,  Some(Dirtroad), Some(0x2310), Some(0x57180080))
  val L2Rhw6cm = new Network(2, Symmetrical,  Some(Dirtroad), Some(0x2320), Some(0x57280080))
  val   Rhw6c  = new Network(0, Asymmetrical, Some(Dirtroad), Some(0x2400), Some(0x57080000))
  val L1Rhw6c  = new Network(1, Asymmetrical, Some(Dirtroad), Some(0x2410), Some(0x57180000))
  val L2Rhw6c  = new Network(2, Asymmetrical, Some(Dirtroad), Some(0x2420), Some(0x57280000))
  val   Rhw8c  = new Network(0, Asymmetrical, Some(Dirtroad), Some(0x2500), Some(0x57090000))
  val L1Rhw8c  = new Network(1, Asymmetrical, Some(Dirtroad), Some(0x2510), Some(0x57190000))
  val L2Rhw8c  = new Network(2, Asymmetrical, Some(Dirtroad), Some(0x2520), Some(0x57290000))
  val   Rhw10c = new Network(0, Asymmetrical, Some(Dirtroad), Some(0x2600), Some(0x570A0000))
  val L1Rhw10c = new Network(1, Asymmetrical, Some(Dirtroad), Some(0x2610), Some(0x571A0000))
  val L2Rhw10c = new Network(2, Asymmetrical, Some(Dirtroad), Some(0x2620), Some(0x572A0000))

  // NWM network
  val Tla3  = new Network(0, Symmetrical,  Some(Road),       Some(0x2700), None)
  val Ave2  = new Network(0, Symmetrical,  Some(Road),       Some(0x2800), None)
  val Ard3  = new Network(0, Asymmetrical, Some(Road),       Some(0x2900), None)
  val Owr1  = new Network(0, Asymmetrical, Some(Road),       Some(0x2A00), None)
  val Owr3  = new Network(0, Symmetrical,  Some(Onewayroad), Some(0x2B00), None)
  val Nrd4  = new Network(0, Symmetrical,  Some(Road),       Some(0x2C00), None)
  val Tla5  = new Network(0, Asymmetrical, Some(Road),       Some(0x2D00), None)
  val Owr4  = new Network(0, AvenueLike,   Some(Onewayroad), Some(0x2E00), None)
  val Owr5  = new Network(0, Asymmetrical, Some(Onewayroad), Some(0x2F00), None)
  val Rd4   = new Network(0, AvenueLike,   Some(Avenue),     Some(0x3000), None)
  val Rd6   = new Network(0, Asymmetrical, Some(Road),       Some(0x3100), None)
  val Ave6  = new Network(0, Asymmetrical, Some(Road),       Some(0x3200), None)
  val Tla7m = new Network(0, Symmetrical,  Some(Road),       Some(0x3300), None)
  val Ave8  = new Network(0, Asymmetrical, Some(Road),       Some(0x3400), None)
  val Ave6m = new Network(0, Symmetrical,  Some(Road),       Some(0x3500), None)

  // SAM networks
  val Sam1, Sam2, Sam3, Sam4, Sam5, Sam6, Sam7, Sam8, Sam9, Sam10, Sam11 =
    new Network(0, Symmetrical, Some(Street), None, None)

  // RRW networks
  val L1Dtr    = new Network(1, Symmetrical, Some(Rail), None, None)
  val L2Dtr    = new Network(2, Symmetrical, Some(Rail), None, None)
  val L1Str    = new Network(1, Symmetrical, Some(Rail), None, None)
  val L2Str    = new Network(2, Symmetrical, Some(Rail), None, None)
  val L1DtrAlt = new Network(1, Symmetrical, Some(Rail), None, None)
  val L2DtrAlt = new Network(2, Symmetrical, Some(Rail), None, None)
  val L1StrAlt = new Network(1, Symmetrical, Some(Rail), None, None)
  val L2StrAlt = new Network(2, Symmetrical, Some(Rail), None, None)

  // HRW networks
  val Hrw   = new Network(0, Symmetrical, None, None, None)
  val L1Hrw = new Network(1, Symmetrical, None, None, None)
  val L2Hrw = new Network(2, Symmetrical, None, None, None)

  // viaducts
  val L1Road        = new Network(1, Symmetrical, Some(Road),       Some(0x1110), None)
  val L2Road        = new Network(2, Symmetrical, Some(Road),       Some(0x1120), None)
  val L1Onewayroad  = new Network(1, Symmetrical, Some(Onewayroad), Some(0x1210), None)
  val L2Onewayroad  = new Network(2, Symmetrical, Some(Onewayroad), Some(0x1220), None)
  val L1Avenue      = new Network(1, AvenueLike,  Some(Avenue),     Some(0x1310), None)
  val L2Avenue      = new Network(2, AvenueLike,  Some(Avenue),     Some(0x1320), None)

  // network collections
  val BaseNetworks = values.filter(_.base.isEmpty)
  val OverrideNetworks = values.filter(_.base.isDefined)
  val GlrNetworks = values from Glr1 to Glr4
  val RhwNetworks = ValueSet(Dirtroad) ++ (values from L1Rhw2 to L2Rhw10c)
  val NwmNetworks = values from Tla3 to Ave6m
  val SamNetworks = values from Sam1 to Sam10
}

}  // end of syntax
