package com.sc4nam.module

import io.github.memo33.metarules.meta._
import syntax._, Network._, Flags._, Flag._, RotFlip._, Implicits._, group.SymGroup._
import NetworkProperties._


class RoundaboutGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Stability {

  private def createRoadRoundabout(): Unit = {

    // This implementation of road roundabouts relies on a surrogate tile.
    // The surrogate tile does not appear in the game. Rather, it is used to trigger adjacencies with NAM.dll.
    // The surrogate tile is a "straight roundabout" tile which overrides ortho avenue.
    // Avenue is used because the asymmetry reduces the number of dummy rules produced.

    def makeRulesToSurrogate(t: Tile): Unit = {
      // constructs rules to override the surrogate tile
      // assumes t is oriented for top left position of roundabout
      Rules += t        | Avenue~(2,0,-2,0) | % | RdRndbt~(2,0,-2,0)
      Rules += t * R1F1 | Avenue~(2,0,-2,0) | % | RdRndbt~(2,0,-2,0)
      createRules()
    }

    def makeRulesFromSurrogate(t1: Tile, t2: Tile): Unit = {
      // constructs rules for surrogate to override t1 -> t2
      // assumes t1 & t2 are oriented for top left position of roundabout
      Rules += RdRndbt~(2,0,-2,0) | t1 * R1F0 | % | t2 * R1F0
      Rules += RdRndbt~(2,0,-2,0) | t1 * R0F1 | % | t2 * R0F1
      createRules()
    }

    def makeRulesToDiagBlend(t: Tile): Unit = {
      // adds rule to override diag network into a orth-diag blend tile
      for (seg <- t.segs) {
        if (seg == Street~CEN) {
          Rules += t * R1F0 | Street~WN | % | Street~(11,3,0,0)
        }
        if (seg == Street~CWN) {
          Rules += t * R1F0 | Street~WS | % | Street~(13,0,0,1)
        }
        if (seg == Road~CEN) {
          Rules += t * R1F0 | Road~WN | % | Road~(11,3,0,0)
        }
        if (seg == Road~CWN) {
          Rules += t * R1F0 | Road~WS | % | Road~(13,0,0,1)
        }
        if (seg == Onewayroad~CEN) {
          Rules += t * R1F0 | Onewayroad~WN | % | Onewayroad~(11,3,0,0)
        }
        if (seg == Onewayroad~CWN) {
          Rules += t * R1F0 | Onewayroad~WS | % | Onewayroad~(13,0,0,1)
        }
      createRules()
      }
    }

    // roundabout tiles that are defined in the road INRUL
    val inrulTiles = Seq[Tile] (
      RdRndbt~(0,0,-2,2),                             // no connection
      RdRndbt~(0,0,-2,2) & Road~NC,                   // road orth
      RdRndbt~(0,0,-2,2) & Road~(0,1,0,0),            // road diag left
      RdRndbt~(0,0,-2,2) & Road~(0,3,0,0),            // road diag right
      RdRndbt~(0,0,-2,2) & Road~WC & Road~(0,3,0,0),  // road orth + road diag right
      RdRndbt~(0,0,-2,2) & Road~WC & Road~(0,1,0,0),  // road orth + road diag left
      RdRndbt~(0,0,-2,2) & Road~(102,102,0,0),        // overlapping roundabouts
    )
    // inrul tile override of surrogate tile
    for (t <- inrulTiles){
      makeRulesToSurrogate(t)
    }

    // RUL1 intersection -> roundabout tile
    // all oriented such that they occupy the top left tile of the roundabout
    val rul1Mapping = Seq[(Tile, Tile)](
      // one connection
      (Road~(0,0,2,2) & Street~NC      , RdRndbt~(0,0,-2,2) & Street~NC        ),
      (Road~(0,0,2,2) & Street~CEN     , RdRndbt~(0,0,-2,2) & Street~CEN       ),
      (Road~(0,0,2,2) & Street~CWN     , RdRndbt~(0,0,-2,2) & Street~CWN       ),
      (Road~(0,0,2,2) & Onewayroad~NC  , RdRndbt~(0,0,-2,2) & Onewayroad~NC    ),
      (Road~(0,0,2,2) & Onewayroad~CEN , RdRndbt~(0,0,-2,2) & Onewayroad~CEN   ),
      (Road~(0,0,2,2) & Onewayroad~CWN , RdRndbt~(0,0,-2,2) & Onewayroad~CWN   ),
      (Road~(0,0,2,2) & Dirtroad~NC    , RdRndbt~(0,0,-2,2) & Dirtroad~NC      ),
      // two connections - double road
      (Road~NS        & Road~WE             , RdRndbt~(0,0,-2,2) & Road~WC & Road~NC         ),
      (Road~(2,11,2,2)                      , RdRndbt~(0,0,-2,2) & Road~WC & Road~CEN        ),
      (Road~(2,13,2,2)                      , RdRndbt~(0,0,-2,2) & Road~WC & Road~CWN        ),
      // two connections - road plus other
      (Road~(2,0,2,2) & Street~NC           , RdRndbt~(0,0,-2,2) & Road~WC & Street~NC       ),
      (Road~(2,0,2,2) & Street~CEN          , RdRndbt~(0,0,-2,2) & Road~WC & Street~CEN      ),
      (Road~(2,0,2,2) & Street~CWN          , RdRndbt~(0,0,-2,2) & Road~WC & Street~CWN      ),
      (Road~(2,0,2,2) & Onewayroad~NC       , RdRndbt~(0,0,-2,2) & Road~WC & Onewayroad~NC   ),
      (Road~(2,0,2,2) & Onewayroad~CEN      , RdRndbt~(0,0,-2,2) & Road~WC & Onewayroad~CEN  ),
      (Road~(2,0,2,2) & Onewayroad~CWN      , RdRndbt~(0,0,-2,2) & Road~WC & Onewayroad~CWN  ),
      (Road~(2,0,2,2) & Dirtroad~NC         , RdRndbt~(0,0,-2,2) & Road~WC & Dirtroad~NC     ),
      // two connections - double street
      (Road~(0,0,2,2) & Street~(2,2,0,0)    , RdRndbt~(0,0,-2,2) & Street~WC & Street~NC     ),
      (Road~(0,0,2,2) & Street~(2,1,0,0)    , RdRndbt~(0,0,-2,2) & Street~WC & Street~CEN    ),
      (Road~(0,0,2,2) & Street~(2,3,0,0)    , RdRndbt~(0,0,-2,2) & Street~WC & Street~CWN    ),
      // two connections - double onewayroad
      (Road~(0,0,2,2) & Onewayroad~(2,2,0,0), RdRndbt~(0,0,-2,2) & Onewayroad~(2,2,0,0)      ),
      // two connections - double dirtroad
      (Road~(0,0,2,2) & Dirtroad~(2,2,0,0)  , RdRndbt~(0,0,-2,2) & Dirtroad~(2,2,0,0)        ),
    )

    for ((t1, t2) <- rul1Mapping) {
      // overrides to/from surrogate tile
      makeRulesToSurrogate(t2)
      makeRulesFromSurrogate(t1, t2)
      // diag blend fix rules
      makeRulesToDiagBlend(t2)
    }

    // Tiles that require RUL2 from outside the roundabout
    // oriented for top left position of roundabout
    val rul2Tiles = Seq[Tile] (
      RdRndbt~(0,0,-2,2) & Owr1~NC,
      RdRndbt~(0,0,-2,2) & Owr1~(0,1,0,0),
      RdRndbt~(0,0,-2,2) & Owr1~(0,3,0,0),
      RdRndbt~(0,0,-2,2) & Owr1~(2,2,0,0),
      RdRndbt~(0,0,-2,2) & Dirtroad~WC & Mis~NC,
      RdRndbt~(0,0,-2,2) & Dirtroad~WC & Mis~CN,
      RdRndbt~(0,0,-2,2) & Mis~NC,
      RdRndbt~(0,0,-2,2) & Mis~CN,
      RdRndbt~(0,0,-2,2) & Mis~WC & Mis~NC,
      RdRndbt~(0,0,-2,2) & Mis~WC & Mis~CN,
      RdRndbt~(0,0,-2,2) & Mis~CW & Mis~NC,
      RdRndbt~(0,0,-2,2) & Onewayroad~WC & Street~NC,
      RdRndbt~(0,0,-2,2) & Road~WC & Owr1~NC,
      RdRndbt~(0,0,-2,2) & Road~WC & Owr1~CEN,
      RdRndbt~(0,0,-2,2) & Road~WC & Mis~NC,
      RdRndbt~(0,0,-2,2) & Road~WC & Mis~CN,
    )

    // make overrides of surrogate from tiles which require outside RUL2
    for (t <- rul2Tiles) {
      makeRulesToSurrogate(t)
    }

    // unique rules for tiles which require outside RUL2

    // orth Owr1
    Rules += Owr1~WE | RdRndbt~(0,0,-2,2) & (Onewayroad ~> Owr1)~WC
    // orth Owr1 & orth Owr1
    Rules += Owr1~WE | RdRndbt~(0,0,-2,2) & (Onewayroad ~> Owr1)~(2,2,0,0)
    // orth Owr1 & orth Road
    Rules += Owr1~WE | RdRndbt~(0,0,-2,2) & Road~NC & (Onewayroad ~> Owr1)~WC
    // diag Owr1 away
    Rules ++= stabilize(Owr1~NE | RdRndbt~(0,0,-2,2) & Onewayroad~CSW | Owr1~(0,1,13,0) | RdRndbt~(0,0,-2,2) & Owr1~CSW)
    // diag Owr1 back
    Rules ++= stabilize(Owr1~ES | RdRndbt~(0,0,-2,2) & Onewayroad~CNW | Owr1~(0,0,11,3) | RdRndbt~(0,0,-2,2) & Owr1~CNW)
    // diag Owr1 away & orth Road
    Rules ++= stabilize(Owr1~NE | RdRndbt~(0,0,-2,2) & Road~NC & Onewayroad~CSW | Owr1~(0,1,13,0) | RdRndbt~(0,0,-2,2) & Road~NC & Owr1~CSW)
    // orth Mis
    Rules += Mis~WE~EW | RdRndbt~(0,0,-2,2) & (Dirtroad ~> Mis)~WC~CW
    // orth Mis & orth Mis
    Rules += Mis~WE~EW | RdRndbt~(0,0,-2,2) & Mis~NC & (Dirtroad ~> Mis)~WC~CW
    Rules += Mis~WE~EW | RdRndbt~(0,0,-2,2) & Mis~CN & (Dirtroad ~> Mis)~WC~CW
    // orth Mis & orth Road
    Rules += Mis~WE~EW | RdRndbt~(0,0,-2,2) & Road~NC & (Dirtroad ~> Mis)~WC~CW
    // orth Mis & orth RHW-2
    Rules += Mis~WE~EW | RdRndbt~(0,0,-2,2) & Dirtroad~(2,2,0,0) | % | RdRndbt~(0,0,-2,2) & Dirtroad~NC & Mis~WC~CW

    // special three network stub-stub overide (RdRndbt + Onewayroad + Street)
    Rules ++= stabilize(Onewayroad~WC | RdRndbt~(0,0,-2,2) & Street~NC | Onewayroad~WE | RdRndbt~(0,0,-2,2) & Street~NC & Onewayroad~WC)


  }

  def start(): Unit = {

    createRoadRoundabout()
    createRules()
  }
}

// Compile individually with `sbt "runMain com.sc4nam.module.CompileRoundaboutCode"`.
object CompileRoundaboutCode extends AbstractMain {
  lazy val resolve: IdResolver = new MiscResolver orElse new RhwResolver orElse new NwmResolver
  val generator = new RoundaboutGenerator(_)
  lazy val file = new java.io.File("Controller/RUL2/04_Roundabouts/Sec4_Roundabouts_MetaGenerated.txt")
}