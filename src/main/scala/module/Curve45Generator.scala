package com.sc4nam.module

import io.github.memo33.metarules.meta._
import syntax._, Network._, Flags._, RotFlip._, Implicits._, group.SymGroup.noSymmetries
import NetworkProperties.{isSingleTile, isDoubleTile, owr4AltNetwork}

/* Flags of sharp curves:
 *       ,---------,---------,
 *       |         |         |
 *       |         |       +1|
 *       |         |   -3    |
 *       ;---------;---------;
 *       |         |   +3    |
 *       |-2    +11|-11      |
 *       |         |         |
 *       '---------'---------'
 * Flags of R1 curves (single-tile networks):
 *       ,---------,---------,---------,
 *       |         |         |   +3    |
 *       |         |       +1|-1       |
 *       |         |  -113   |         |
 *       ;---------;---------;---------;
 *       |         |  +113   |         |
 *       |-2   +121|-121     |         |
 *       |         |         |         |
 *       '---------'---------'---------'
 * Flags of mini curves (multi-tile networks):
 *       ,---------,---------,---------,
 *       |         |         |   +3    |
 *       |         |       +1|-1       |
 *       |         |  -113   |         |
 *       ;---------;---------;---------;
 *       |         |  +113   |         |
 *       |-2    +11|-11      |         |
 *       |         |         |         |
 *       '---------'---------'---------'
 * Flags of extended curves (multi-tile networks):
 *       ,---------,---------,---------,---------,
 *       |         |         |         |   +3    |
 *       |         |         |       +1|-1       |
 *       |         |         |  -113   |         |
 *       ;---------;---------;---------;---------;
 *       |         |         |  +113   |         |
 *       |-2   +111|-111  +11|-11      |         |
 *       |         |         |         |         |
 *       '---------'---------'---------'---------'
 * Micro 90 curve (MIS-style or Avenue outside curve):
 *       ,---------,---------,
 *       |         |   +2    |
 *       |         |         |
 *       |         |  -113   |
 *       ;---------;---------;
 *       |         |   +2    |
 *       |-2   +111|-2       |
 *       |         |         |
 *       '---------'---------'
 * 2×2 90 degree curve:
 *       ,---------,---------,
 *       |         |   +2    |
 *       |     +141|         |
 *       |  -143   |  -133   |
 *       ;---------;---------;
 *       |         |  +133   |
 *       |-2   +131|-131     |
 *       |         |         |
 *       '---------'---------'
 */

trait Stability { this: RuleGenerator =>
  export NetworkProperties.stabilize
}

trait Curve45Generator extends Stability { this: RuleGenerator =>

  def hasSharedDiagCurve(n: Network): Boolean = n.typ == AvenueLike

  def hasSharpCurveBase(n: Network, inside: Boolean): Boolean = {
    n.base.exists(b => b == Dirtroad || b == Road || b == Onewayroad || b == Lightrail)
  }

  def hasR1CurveBase(n: Network): Boolean = {
    n.base.exists(b => b == Dirtroad || b == Road || b == Onewayroad)
  }

  def hasSharpCurve(n: Network, inside: Boolean): Boolean = {
    n >= L1Rhw2 && n <= L4Rhw6s || n >= Tla3 && n <= Nrd4 || n == Tla5 || n == Rd6 || n == Owr5 ||
    inside && (n == Ave6 || n == Ave8) ||
    GlrNetworks.contains(n)
  }

  def hasMiniCurve(n: Network, inside: Boolean): Boolean = NetworkProperties.hasMiniCurve(n, inside = inside)

  def hasExtendedCurve(n: Network, inside: Boolean): Boolean = NetworkProperties.hasExtendedCurve(n, inside = inside)

  def hasR1Curve(n: Network, inside: Boolean): Boolean = {
    (n.isRhw && n <= L4Rhw6s && n != L1Rhw3 && n != L2Rhw3)  // Elevated R1 Rhw3 models are currently missing
    // TODO add NWM
  }

  def hasMisStyle90Curve(n: Network, inside: Boolean): Boolean = {
    (n >= Mis && n <= L4Mis) ||
    !inside && (n == Tla5 || n == Rd4 || n.isOwr4Like)
  }

  def has90Curve(n: Network, inside: Boolean): Boolean = {
    n.isNwm && (isSingleTile(n) || inside && (n == Tla5 || n == Rd4 || n.isOwr4Like)) ||
    GlrNetworks.contains(n)
  }

  def createCurve45Rules(main: Network): Unit = {
    // all curves are written in form of outside curve; orient can be used
    // to reverse the flags in order to create the corresponding inside
    // curves
    def curveCode(inside: Boolean): Unit = {
      val orient: IntFlags => IntFlags = if (!inside) identity else reverseIntFlags
      assert(main.base.isDefined)
      val base = main.base.get
      if (hasSharedDiagCurve(main)) {
        val sharedCurveTileBase: Tile = base~(+11,-3,+1,-3)
        val mainAlt = if (!main.isOwr4Like) main else owr4AltNetwork(main)
        val sharedCurveTile: Tile = if (!main.isOwr4Like) main~(+11,-3,+1,-3) else main~(+11,-3,0,0) & mainAlt~(0,0,+1,-3)
        // orth to diag
        Rules += main~WE~EW             | (base ~> main)~(-2,0,+11,0)~(+2,0,-11,0)
        Rules += main~(-2,0,+11,0)      | (base ~> main)~(-11,+3,0,0)
        Rules += main~(+2,0,-11,0)      | sharedCurveTileBase        | % | sharedCurveTile
        Rules += mainAlt~(0,-11,+3,0)   | sharedCurveTileBase * R1F0 | % | sharedCurveTile * R1F0
        Rules += sharedCurveTile        | (base ~> mainAlt)~WN
        Rules += sharedCurveTile * R1F0 | (base ~> main)~SW
        // diag to orth
        Rules += (base ~> main)~WE~EW           | main~(-2,0,+11,0)~(+2,0,-11,0)
        Rules += (base ~> main)~(-2,0,+11,0)    | main~(-11,+3,0,0)
        Rules += (base ~> main)~(+2,0,-11,0)    | sharedCurveTile
        Rules += (base ~> mainAlt)~(0,-11,+3,0) | sharedCurveTile * R1F0
        Rules += sharedCurveTileBase            | mainAlt~WN | sharedCurveTile        | %
        Rules += sharedCurveTileBase * R1F0     | main~SW    | sharedCurveTile * R1F0 | %
      }
      if (hasSharpCurveBase(main, inside)) {
        if (hasSharpCurve(main, inside)) {
          // orth to diag
          Rules += main~orient(WE)         | (base ~> main)~orient(-2,0,+11,0)
          Rules += main~orient(-2,0,+11,0) | (base ~> main)~orient(-11,+3,0,0)
          Rules += main~orient(0,-11,+3,0) | (base ~> main)~orient(WS)
          // diag to orth
          Rules += (base ~> main)~orient(WE)         | main~orient(-2,0,+11,0)
          Rules += (base ~> main)~orient(-2,0,+11,0) | main~orient(-11,+3,0,0)
          Rules += (base ~> main)~orient(0,-11,+3,0) | main~orient(WS)
        } else if (hasMiniCurve(main, inside)) {
          // orth to diag
          Rules += main~orient(WE)          | (base ~> main)~orient(-2,0,+11,0)
          Rules += main~orient(-2,0,+11,0)  | base~orient(-11,+3,0,0)    | % | main~orient(-11,+113,0,0)
          Rules += main~orient(0,-11,+113,0) | base~orient(WS)            | % | main~orient(-113,0,0,+1)
          Rules += main~orient(0,0,+1,-113)  | (base ~> main)~orient(WN)
          // diag to orth
          Rules += (base ~> main)~orient(WE)         | main~orient(-2,0,+11,0)
          Rules += (base ~> main)~orient(-2,0,+11,0) | main~orient(-11,+113,0,0)
          Rules ++= stabilize (
            base~orient(0,-11,+3,0) | main~orient(WS) | main~orient(0,-11,+113,0) | main~orient(-113,0,0,+1)
          )
        } else if (hasExtendedCurve(main, inside)) {
          // orth to diag
          Rules ++= stabilize (
            main~orient(WE) | base~orient(-2,0,+11,0) | main~orient(-2,0,+111,0) | main~orient(-111,0,+11,0)
          )
          Rules += main~orient(-111,0,+11,0) | base~orient(-11,+3,0,0) | % | main~orient(-11,+113,0,0)
          Rules += main~orient(0,-11,+113,0)  | base~orient(WS)         | % | main~orient(-113,0,0,+1)
          Rules += main~orient(0,0,+1,-113)   | (base ~> main)~orient(WN)
          // diag to orth
          Rules += (base ~> main)~orient(WE) | main~orient(-2,0,+111,0)
          Rules += base~orient(WE) | main~orient(-111,0,+11,0) | main~orient(-2,0,+111,0) | %
          Rules += base~orient(-2,0,+11,0) | main~orient(-11,+113,0,0) | main~orient(-111,0,+11,0) | %
          Rules ++= stabilize (
            base~orient(0,-11,+3,0) | main~orient(WS) | main~orient(0,-11,+113,0) | main~orient(-113,0,0,+1)
          )
        }
      }
      if (hasR1CurveBase(main)) {
        if (hasR1Curve(main, inside)) {
          // R1 curve: orth to diag
          Rules += main~orient(WE)            | (base ~> main)~orient(-2,0,+121,0)
          Rules += main~orient(-2,0,+121,0)   | (base ~> main)~orient(-121,+113,0,0)
          Rules += main~orient(0,-121,+113,0) | (base ~> main)~orient(-113,0,0,+1)
          Rules += main~orient(0,-121,+113,0) | (base ~> main)~orient(-113,0,0,+111)  // R1 curve: 90 degree
          Rules += main~orient(0,0,+1,-113)   | (base ~> main)~orient(WN)
          // R1 curve: diag to orth
          Rules += (base ~> main)~orient(WE)            | main~orient(-2,0,+121,0)
          Rules += (base ~> main)~orient(-2,0,+121,0)   | main~orient(-121,+113,0,0)
          Rules += (base ~> main)~orient(0,-121,+113,0) | main~orient(-113,0,0,+1)
          Rules += (base ~> main)~orient(0,-121,+113,0) | main~orient(-113,0,0,+111)  // R1 curve: 90 degree
          Rules += (base ~> main)~orient(0,0,+1,-113)   | main~orient(WN)
          // R1 curve: S-type curves
          if (hasR1Curve(main, !inside)) {
            Rules += main~orient(0,0,+1,-113)   | (base ~> main)~orient(-1,+113,0,0)  // trans diag
            Rules += main~orient(-121,0,+2,0)   | (base ~> main)~orient(-2,0,+121,0)  // trans orth
          }
          Rules += main~orient(-123,0,+2,0)   | (base ~> main)~orient(-2,0,+121,0)  // cis orth
          // Rhw6s R1 outside curve has an extra tile due to overhangs
          if (!inside && Rhw6s <= main && main <= L4Rhw6s) {
            val offset = if (main.height == 0) 0 else main.height * 0x10 + 5
            Rules += Dirtroad~(0,0,0,0) | main~(0,-2,0,+123) | IdTile(0x57945380 + offset, R1F0, noSymmetries) | %
          }
        }
      }
    }
    curveCode(inside = false)
    if (!main.isSymm && !hasSharedDiagCurve(main)) {
      curveCode(inside = true)
    }
    createRules()
  }

  def createCurve90Rules(main: Network): Unit = {
    // all curves are written in form of outside curve; orient can be used
    // to reverse the flags in order to create the corresponding inside
    // curves
    def curveCode(inside: Boolean): Unit = {
      val orient: IntFlags => IntFlags = if (!inside) identity else reverseIntFlags
      assert(main.base.isDefined)
      val base = main.base.get
      if (hasMisStyle90Curve(main, inside)) {
        if (base.typ != AvenueLike) {
          // Mis 90 curve
          Rules ++= stabilize(main~orient(WE) | base~orient(-2,+2,0,0) | main~orient(-2,0,+111,0) | main~orient(-2,+2,0,0))
          Rules += main~orient(0,-2,+2,0) | base~orient(WE) | % | main~orient(-113,0,+2,0)
          Rules += main~orient(-113,0,+2,0) | (base ~> main)~orient(WE)
        } else {
          // Avenue-style outside curve
          Rules += main~orient(WE) | (base ~> main)~orient(-2,0,+111,0)
          Rules += main~orient(-2,0,+111,0) | (base ~> main)~orient(-2,+2,0,0)
          Rules += main~orient(0,-2,+2,0) | (base ~> main)~orient(-113,0,+2,0)
          Rules += main~orient(-113,0,+2,0) | (base ~> main)~orient(WE)
        }
      }
      if (has90Curve(main, inside)) {
        // 90 degree curve
        Rules += main~orient(WE) | (base ~> main)~orient(-2,+2,0,0)
        Rules += main~orient(0,-2,+2,0) | (base ~> main)~orient(WE)
      }
    }
    curveCode(inside = false)
    if (!main.isSymm) {
      curveCode(inside = true)
    }
    createRules()
  }
}
