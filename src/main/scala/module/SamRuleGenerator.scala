package com.sc4nam.module

import io.github.memo33.metarules.meta._
import syntax._, Network._, Flags._, Flag._, RotFlip._, Implicits._, group.SymGroup._
import NetworkProperties._


class SamRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Adjacencies with Stability {

  private def createCurveAdjacencies(tile: Tile, sam: Network): Unit = {
      Rules += tile | (Street ~> sam)~WE          // orthogonal
      Rules += tile | (Street ~> sam)~WC          // orthogonal stub
      Rules += tile | (Street ~> sam)~(2,2,0,0)   // 90 degree turn
      Rules += tile | (Street ~> sam)~(2,0,0,2)   // 90 degree turn
      Rules += tile | (Street ~> sam)~(2,0,11,0)  // diagonal turn
      Rules += tile | (Street ~> sam)~(2,0,13,0)  // diagonal turn
      Rules += tile | (Street ~> sam)~WE & (Street ~> sam)~NS  // OxO
      Rules += tile | (Street ~> sam)~WE & (Street ~> sam)~NC  // OxO T
      Rules += tile | (Street ~> sam)~WE & (Street ~> sam)~CS  // OxO T
      Rules += tile | (Street ~> sam)~WC & (Street ~> sam)~NS  // OxO T
      Rules += tile | (Street ~> sam)~(2,0,131,0)  // 2x2 90
      Rules += tile | (Street ~> sam)~(2,0,131,2)  // 2x2 90 w/ t-int
      Rules += tile | (Street ~> sam)~(2,0,133,0)  // 2x2 90
      Rules += tile | (Street ~> sam)~(2,2,133,0)  // 2x2 90 w/ t-int

      Rules += tile | (Street ~> sam)~(2,0,153,0)  // 3x2 s-curve
      Rules += tile | (Street ~> sam)~(2,2,153,0)  // 3x2 s-curve w/ t-int
      Rules += tile | (Street ~> sam)~(2,0,151,0)  // 3x2 s-curve
      Rules += tile | (Street ~> sam)~(2,0,151,2)  // 3x2 s-curve w/ t-int
      createRules()
  }

  private def createSamLarge45Curve(sam: Network): Unit = {
    // large 45 curve (4x3)
    // *                                        (diag)
    // *       ,---------,---------,---------,---------,
    // *       |         |         |         |         |
    // *       |         |    0    |    1    |    2    |
    // *       |         |         |         |         |
    // *       ;---------;---------;---------;---------;
    // *       |         |         |         |         |
    // * ortho |    3    |    4    |    5    |         |
    // *       |         |         |         |         |
    // *       '---------'---------'---------'---------'
    // *       ,---------,---------,---------,---------,
    // *       |         |         |         |    3    |
    // *       |         |       14|        1|1        |
    // *       |         |   15    |   113   |         |
    // *       ;---------;---------;---------;---------;
    // *       |         |         |   113   |         |
    // * ortho |2     111|111    11|11       |         |
    // *       |         |         |         |         |
    // *       '---------'---------'---------'---------'
    /*
    The following code for the large 45 curve is organized like so:
    First, the overide from the orthogonal side to the diagonal side is described, ordered left to right.
    Then, the the diagonal to orthogonal direction follows, ordered from right to left.
    To to and from tiles are annotated in-line.  The necessary stability rules that apply to
    that from and to tile combo (or its destabilized equivalent) follow indented thereafter.
    */
    //  ortho to diagonal (bottom row)
    Rules += sam~WE | (Street ~> sam)~(2,0,111,0)             // ortho to 3
    Rules += sam~(2,0,111,0) | (Street ~> sam)~(111,0,11,0)   // 3 to 4
      Rules += sam~(2,0,111,0) | Street~(2,0,11,0) | % | sam~(111,0,11,0)
    Rules += sam~(111,0,11,0) | (Street ~> sam)~(11,113,0,0)  // 4 to 5
      Rules += sam~(111,0,11,0) | sam~(11,3,0,0) | % | sam~(11,113,0,0)
      Rules += sam~(111,0,11,0) | Street~(11,3,0,0) | % | sam~(11,113,0,0)
      Rules += sam~(111,0,11,0) | Street~(1,3,0,0) | % | sam~(11,113,0,0)
    //  ortho to diagonal (bottom to top)
    Rules += sam~(0,111,0,11) | (Street ~> sam)~(15,0,0,14)   // 4 to 0
      Rules ++= stabilize(sam~(0,2,0,11) | Street~(0,0,0,0) | sam~(0,111,0,11) | sam~(15,0,0,14))
      Rules ++= stabilize(sam~(0,2,0,11) | Street~(15,0,0,14) | sam~(0,111,0,11) | sam~(15,0,0,14))
    Rules += sam~(0,11,113,0) | (Street ~> sam)~(113,0,0,1)   // 5 to 1
      Rules += sam~(0,11,113,0) | Street~(3,0,0,1) | % | sam~(113,0,0,1)
    //  ortho to diagonal (top row)
    Rules += sam~(0,0,14,15) | (Street ~> sam)~(0,0,1,113)    // 0 to 1
      Rules += sam~(0,0,14,15) | sam~(0,0,1,3) | % | sam~(0,0,1,113)
    Rules += sam~(0,0,1,113) | (Street ~> sam)~(1,3,0,0)      // 1 to 2
    //  diagonal to ortho (top row)
    Rules += sam~(0,0,1,3) | (Street ~> sam)~(1,113,0,0)      // 2 to 1
    Rules += sam~(1,113,0,0) | (Street ~> sam)~(14,15,0,0)    // 1 to 0
    //  diagonal to ortho (top to bottom)
    Rules += sam~(0,1,113,0) | (Street ~> sam)~(113,0,0,11)   // 1 to 5
      Rules += sam~(0,1,113,0) | Street~(3,0,0,11) | % | sam~(113,0,0,11)
    Rules += sam~(0,14,15,0) | (Street ~> sam)~(0,11,0,111)   // 0 to 4
    //  diagonal to ortho (bottom row)
    Rules += sam~(0,0,11,113) | (Street ~> sam)~(11,0,111,0)  // 5 to 4
      Rules += sam~(0,0,11,113) | Street~(11,0,2,0) | % | sam~(11,0,111,0)
      Rules += sam~(0,0,11,113) | Street~(2,0,2,0) | % | sam~(11,0,111,0)
    Rules += sam~(11,0,111,0) | (Street ~> sam)~(111,0,2,0)   // 4 to 3
      Rules += sam~(11,0,111,0) | sam~(WE) | % | sam~(111,0,2,0)
      Rules += sam~(11,0,111,0) | Street~(WE) | % | sam~(111,0,2,0)
      Rules += sam~(11,0,111,0) | Street~(11,0,2,0) | % | sam~(111,0,2,0)
    Rules += sam~(111,0,2,0) | (Street ~> sam)~WE             // 3 to ortho
    // -------------
    createRules()
  }

  private def createSam2x290Curve(sam: Network): Unit = {
    // *
    // *        2x2 90 Curve and optional diverter tiles
    // *                                                  SAM diverter
    // *       ,---------,---------,---------,            ,---------,
    // *       |         |         |         |            |   131   |
    // *       |2       2|2     133|133      | <-- -- --> |133   133|
    // *       |         |         |   131   |     or     |   131   |
    // *       ;---------;---------;---------;            '---------'
    // *       |         |   141   |   131   |                 ^
    // *       |         |      143|         |              or |
    // *       |         |         |    2    |                 v
    // *       ;---------;---------;---------;            ,---------,     ,---------,
    // *       |         |         |    2    |            |         |     |   131   |
    // *       |         |         |         |            |133      |  +  |      133|
    // *       |         |         |    2    |            |   131   |     |         |
    // *       '---------'---------'---------'            '---------'     '---------'
    // *                                                      SAM        Street or Road
    // *        2x2 90 Curve w/ optional t-int
    // *       ,---------,---------,---------,
    // *       |         |    2    |         |
    // *       |         |         |         |
    // *       |         |    2    |         |
    // *       ;---------;---------;---------;
    // *       |         |    2    |         |
    // *       |2       2|2     133|133      |  (diverter not possible w/ optional t-int)
    // *       |         |         |   131   |
    // *       ;---------;---------;---------;
    // *       |         |   141   |   131   |
    // *       |         |      143|         |
    // *       |         |         |    2    |
    // *       ;---------;---------;---------;
    // *       |         |         |    2    |
    // *       |         |         |         |
    // *       |         |         |    2    |
    // *       '---------'---------'---------'

    // basic override
    Rules += sam~WE | (Street ~> sam)~(2,0,133,0)
    Rules += sam~(2,0,133,0) | (Street ~> sam)~(133,0,0,131)
    Rules += sam~(0,0,131,133) | (Street ~> sam)~(131,0,2,0)
    Rules += sam~(0,133,0,2) | (Street ~> sam)~(141,143,0,0)
    // t-int variant
    Rules += sam~WE | (Street ~> sam)~(2,2,133,0)
    Rules += sam~(2,2,133,0) | (Street ~> sam)~(133,0,0,131)
    Rules += sam~(0,0,131,133) | (Street ~> sam)~(131,2,2,0)
    Rules += sam~(2,133,0,2) | (Street ~> sam)~(141,143,0,0)
    // diverter overrides with continuances
    Rules += sam~(2,0,133,0) | (Street ~> sam)~(133,0,0,131) & Street~(0,131,133,0)  // half SAM, half street
      Rules += sam~(0,0,131,133) & Street~(131,133,0,0) | (Street ~> sam)~(131,0,2,0)
    Rules += sam~(2,0,133,0) | Street~(133,0,0,131) & sam~(0,131,133,0) | % | sam~(133,131,133,131)  // full SAM
      Rules += sam~(131,133,131,133) | (Street ~> sam)~(131,0,2,0)
    Rules += sam~(2,0,133,0) | (Street ~> sam)~(133,0,0,131) & Road~(0,131,133,0) // half SAM, half road
      Rules += sam~(0,0,131,133) & Road~(131,133,0,0) | (Street ~> sam)~(131,0,2,0)

    // create adjacency rules for the three exit points (curve end, curve end w/ t-int, t-int)
    for (t <- Seq((131,0,2,0), (131,2,2,0), (0,131,2,2))) {
        createCurveAdjacencies(sam~t, sam)
    }
    createRules()
  }

  private def createSamSCurve(sam: Network): Unit = {
    //
    // * SAM S-Curve (3x2)
    // *
    // *                 ,---------,
    // *                 |    2    |
    // *                 |2     153| optional t-int
    // *                 |         |
    // *                 '---------'
    // *                      ^
    // *                      | or
    // *                      v
    // *       ,---------,---------,---------,---------,---------,
    // *       |         |         |         |         |         |
    // *       |2       2|2     153|153      |173      |         |
    // *       |         |         |   161   |   181   |         |
    // *       ;---------;---------;---------;---------;---------;
    // *       |         |   181   |   161   |         |         |
    // *       |         |      173|      153|153     2|2       2|
    // *       |         |         |         |         |         |
    // *       '---------'---------'---------'---------'---------'
    // *
    // *

    // basic override
      // left to right top row
      Rules += sam~WE | (Street ~> sam)~(2,0,153,0)
      Rules += sam~(2,0,153,0) | (Street ~> sam)~(153,0,0,161)
      Rules += sam~(153,0,0,161) | (Street ~> sam)~(173,0,0,181)
      // top to bottom
      Rules += sam~(0,153,0,2) | (Street ~> sam)~(181,173,0,0)
      Rules += sam~(0,0,161,153) | (Street ~> sam)~(161,153,0,0)
      Rules += sam~(0,0,181,173) | (Street ~> sam)~(0,2,0,153)
      // left to right bottom row
      Rules += sam~(0,181,173,0) | (Street ~> sam)~(0,161,153,0)
      Rules += sam~(0,161,153,0) | (Street ~> sam)~(153,0,2,0)

    // t-int variant
      // left to right top row
      Rules += sam~WE | (Street ~> sam)~(2,2,153,0)
      Rules += sam~(2,2,153,0) | (Street ~> sam)~(153,0,0,161)
      // top to bottom
      Rules += sam~(2,153,0,2) | (Street ~> sam)~(181,173,0,0)
      Rules += sam~(0,0,181,173) | (Street ~> sam)~(0,2,2,153)
      // left to right bottom row
      Rules += sam~(0,161,153,0) | (Street ~> sam)~(153,0,2,2)

    // create adjacency rules for the three exit points (curve end, curve end w/ t-int, t-int)
    for (t <- Seq((153,0,2,0), (153,0,2,2), (0,2,2,153))) {
        createCurveAdjacencies(sam~t, sam)
    }
  }

  private def createSamLarge90Curve(sam: Network): Unit = {
    // *
    // *                     (A)       (B)
    // *                 ,---------,---------,
    // *                 |    2    |    2    |  (optional t-ints)
    // *                 |2     183|183   193|
    // *                 |         |    13   |  * cannot be drawn side-by-side *
    // *                 '---------'---------'
    // *                      ^         ^
    // *                   or |    /    | or
    // *                      v         v
    // *       ,---------,---------,---------,---------,---------,
    // *       |         |         |         |         |         |
    // *   0   |2       2|2     183|183   193|193      |         |
    // *       |         |         |    13   |   194   |         |
    // *       ;---------;---------;---------;---------;---------;
    // *       |         |         |    13   |         |         |
    // *   1   |         |         |       82|82       |194      |
    // *       |         |         |         |    82   |   191   |
    // *       '---------;---------;---------;---------;---------;
    // *                           |         |    82   |   191   |
    // *   2                       |         |       11|11    (2)| <--.
    // *                           |         |         |   181   |    |
    // *                           '---------;---------;---------;    |-- optional t-ints
    // *                                     |         |   181   |    |
    // *   3                                 |         |      (2)| <--'
    // *                                     |         |    2    |
    // *                                     ;---------;---------;
    // *                                     |         |    2    |
    // *   4                                 |         |         |
    // *                                     |         |    2    |
    // *                                     '---------'---------'
    // *
    // *            0         1         2         3         4

    // basic override
      // row 0 (left to right)
      Rules += sam~WE | (Street ~> sam)~(2,0,183,0)
      Rules += sam~(2,0,183,0) | (Street ~> sam)~(183,0,193,13)
      Rules += sam~(183,0,193,13) | (Street ~> sam)~(193,0,0,194)
      // row 0 (right to left)
      Rules += sam~(0,194,193,0) | (Street ~> sam)~(193,13,183,0)
      Rules += sam~(193,13,183,0) | (Street ~> sam)~(183,0,2,0)
      // row 1 (left to right)
      Rules += sam~(0,13,82,0) | (Street ~> sam)~(82,0,0,82)
      Rules += sam~(82,0,0,82) | (Street ~> sam)~(194,0,0,191)
      // row 1 (right to left)
      Rules += sam~(0,191,194,0) | (Street ~> sam)~(0,82,82,0)
      Rules += sam~(0,82,82,0) | (Street ~> sam)~(82,0,0,13)
      // row 2 (left to right)
      Rules += sam~(0,82,11,0) | (Street ~> sam)~(11,191,0,181)
      // row 2 (right to left)
      Rules += sam~(0,181,11,191) | (Street ~> sam)~(11,0,0,82)
      // col 2 (top to bottom)
      Rules += sam~(0,193,13,183) | (Street ~> sam)~(13,82,0,0)
      // col 2 (bottom to top)
      Rules += sam~(0,0,13,82) | (Street ~> sam)~(13,183,0,193)
      // col 3 (top to bottom)
      Rules += sam~(0,0,194,193) | (Street ~> sam)~(0,0,82,82)
      Rules += sam~(0,0,82,82) | (Street ~> sam)~(82,11,0,0)
      // col 3 (bottom to top)
      Rules += sam~(0,0,82,11) | (Street ~> sam)~(82,82,0,0)
      Rules += sam~(82,82,0,0) | (Street ~> sam)~(194,193,0,0)
      // col 4 (top to bottom)
      Rules += sam~(0,0,191,194) | (Street ~> sam)~(191,0,181,11)
      Rules += sam~(191,0,181,11) | (Street ~> sam)~(181,0,2,0)
      // col 4 (bottom to top)
      Rules += sam~WE | (Street ~> sam)~(2,0,181,2)
      Rules += sam~(2,0,181,0) | (Street ~> sam)~(181,11,191,0)
      Rules += sam~(181,11,191,0) | (Street ~> sam)~(191,194,0,0)

    // optional t-int type A
      // row 0 (left to right)
      Rules += sam~WE | (Street ~> sam)~(2,2,183,0)
      Rules += sam~(2,2,183,0) | (Street ~> sam)~(183,0,193,13)
      // row 0 (right to left)
      Rules += sam~(193,13,183,0) | (Street ~> sam)~(183,0,2,2)
      // col 4 (top to bottom)
      Rules += sam~(191,0,181,11) | (Street ~> sam)~(181,2,2,0)
      // col 4 (bottom to top)
      Rules += sam~WE | (Street ~> sam)~(2,0,181,2)
      Rules += sam~(2,0,181,2) | (Street ~> sam)~(181,11,191,0)

    // optional t-int type B
      // row 0 (left to right)
      Rules += sam~(2,0,183,0) | (Street ~> sam)~(183,2,193,13)
      Rules += sam~(183,2,193,13) | (Street ~> sam)~(193,0,0,194)
      // row 0 (right to left)
      Rules += sam~(0,194,193,0) | (Street ~> sam)~(193,13,183,2)
      Rules += sam~(193,13,183,2) | (Street ~> sam)~(183,0,2,0)
      // col 2 (top to bottom)
      Rules += sam~(2,193,13,183) | (Street ~> sam)~(13,82,0,0)
      // col 2 (bottom to top)
      Rules += sam~(0,0,13,82) | (Street ~> sam)~(13,183,2,193)
      // col 4 (top to bottom)
      Rules += sam~(0,0,191,194) | (Street ~> sam)~(191,2,181,11)
      Rules += sam~(191,2,181,11) | (Street ~> sam)~(181,0,2,0)
      // col 4 (bottom to top)
      Rules += sam~(2,0,181,0) | (Street ~> sam)~(181,11,191,2)
      Rules += sam~(181,11,191,2) | (Street ~> sam)~(191,194,0,0)

    /* create continuation rules for the four exit points
       - curve end
       - curve end w/ t-int type A
       - t-int type A
       - t-int type B
    */
    for (t <- Seq((181,0,2,0), (181,2,2,0), (0,181,2,2), (11,191,2,181))) {
        Rules += sam~t | (Street ~> sam)~WE
    }

    createRules()
  }


  private def createSamDiagSCurve(sam: Network): Unit = {
    // TODO
  }

  def start(): Unit = {

    val SamNetworks = List(Sam2, Sam3, Sam4, Sam5, Sam6, Sam7, Sam8, Sam9, Sam10, Sam11)

    val CrossNetworks = List(Road, Avenue, Onewayroad,
    Rail, L1Dtr, L2Dtr, Lightrail, Monorail, Glr1, Glr2/*, Str*/, Dirtroad/*, Rhw3, Mis, Rhw4, Rhw6s, Rhw8sm, Rhw8s, Rhw10s, Rhw12s, Rhw6cm,
    Rhw6c, Rhw8c, L1Rhw2, L1Rhw3, L1Mis, L1Rhw4, L1Rhw6s, L1Rhw8sm, L1Rhw8s, L1Rhw10s, L1Rhw12s, L1Rhw6cm,
    L1Rhw6c, L1Rhw8c, L2Rhw2, L2Rhw3, L2Mis, L2Rhw4, L2Rhw6s, L2Rhw8sm, L2Rhw8s, L2Rhw10s, L2Rhw12s, L2Rhw6cm,
    L2Rhw6c, L2Rhw8c, L3Mis, L3Rhw4, L3Rhw6s, L4Mis, L4Rhw4, L4Rhw6s*/, Tla3, Ave2, Ard3, Owr1, Owr3, Nrd4/*, Tla5, Owr4, 
	Owr5, Rd4, Rd6, Ave6, Tla7m, Ave6m*/)

    for (sam <- SamNetworks) {

      Rules += sam~WE | (Street ~> sam)~WE~EW     // ortho
      Rules += sam~WE | Street~CW | % | sam~CW    // ortho stub
      Rules += sam~SE~ES | (Street ~> sam)~WN~NW  // diagonal
      Rules += sam~SE~ES | (Street ~> sam)~CNW    // diagonal stub

      Rules += sam~WE | (Street ~> sam)~(2,2,0,0)           // ortho to sharp 90
      Rules += sam~(0,0,2,2) | (Street ~> sam)~WE           // sharp 90 to ortho
      Rules += sam~(0,0,2,2) | (Street ~> sam)~WC           // sharp 90 to ortho stub
      Rules += sam~(0,0,2,2) | (Street ~> sam)~(2,0,0,2)    // sharp 90 to sharp 90 1
      Rules += sam~(0,0,2,2) | (Street ~> sam)~(2,2,0,0)    // sharp 90 to sharp 90 2
      Rules += sam~(0,0,2,2) | (Street ~> sam)~(2,0,131,0)  // sharp 90 to 2x2 90
      Rules += sam~(0,0,2,2) | (Street ~> sam)~(2,0,131,2)  // sharp 90 to 2x2 90 T
      Rules += sam~(0,0,2,2) | (Street ~> sam)~(2,0,153,0)  // sharp 90 to 3x2 S
      Rules += sam~(0,0,2,2) | (Street ~> sam)~(2,2,153,0)  // sharp 90 to 3x2 S T
      Rules += sam~(0,0,2,2) | (Street ~> sam)~(2,0,11,0)	  // sharp 90 to orth-diag bottom

      Rules += sam~WE | (Street ~> sam)~(2,0,11,0)                                  // orth to orth-diag bottom
      Rules += sam~(2,0,11,0) | (Street ~> sam)~(11,3,0,0)                          // orth-diag bottom to orth-diag top
      Rules += sam~(2,0,11,0) | Street~(1,3,0,0) | sam~(2,0,11,0) | sam~(11,3,0,0)  // orth-diag bottom to orth-diag top fix 1
      Rules += sam~WE | Street~(11,3,0,0) | sam~(2,0,11,0) | sam~(11,3,0,0)         // orth-diag bottom to orth-diag top fix 2
      Rules += sam~WE | Street~(1,3,0,0) | sam~(2,0,11,0) | sam~(11,3,0,0)          // orth-diag bottom to orth-diag top fix 3
      Rules += sam~(2,0,11,0) | Street~(2,13,0,0) | % | sam~(11,3,0,0)              // orth-diag bottom to orth-diag top fix 4
      Rules += sam~(0,0,1,13) | (Street ~> sam)~NW~WN                               // orth-diag top to diag
      // Rules += sam~(0,0,1,13) | Street~(11,3,0,0) | % | sam~(1,3,0,0)               // orth-diag top to diag fix
      Rules += sam~(0,11,3,0) | Street~(13,0,0,1) | % | sam~(3,0,0,1)               // orth-diag top to diag fix
      Rules += sam~(0,11,3,0) | sam~(13,0,0,1) | % | sam~(3,0,0,1)                  // orth-diag top to diag fix
      Rules += sam~(0,1,3,0) | sam~(13,0,0,1) | % | sam~(3,0,0,1)                   // diag top to diag fix
      Rules += sam~(0,0,1,3) | Street~(2,13,0,0) | % | sam~(1,3,0,0)                // diag top to diag fix
      // Rules += sam~(0,0,1,3) | sam~(2,13,0,0) | % | sam~(3,0,0,1)                   // diag top to diag fix
      Rules += sam~(0,0,1,13) | (Street ~> sam)~CNW                                 // orth-diag top to diag stub
      Rules += sam~(0,0,1,13) | (Street ~> sam)~(1,13,0,0)                          // orth-diag top to orth-diag top

      Rules += sam~SE~ES | (Street ~> sam)~(1,13,0,0)                               // diag to orth-diag top
      Rules += sam~(0,0,11,3) | (Street ~> sam)~(11,0,2,0)                          // orth-diag top to orth-diag bottom
      Rules += sam~(0,0,1,3) | Street~(11,0,2,0) | sam~(0,0,11,3) | sam~(11,0,2,0)  // orth-diag top to orth-diag bottom fix 1
      Rules += sam~(0,0,11,3) | Street~WE | sam~(0,0,11,3) | sam~(11,0,2,0)         // orth-diag top to orth-diag bottom fix 1
      Rules += sam~(0,0,1,3) | Street~WE | sam~(0,0,11,3) | sam~(11,0,2,0)          // orth-diag top to orth-diag bottom fix 3
      Rules += sam~(11,0,2,0) | (Street ~> sam)~WE          // orth-diag bottom to orth
      Rules += sam~(11,0,2,0) | (Street ~> sam)~WC          // orth-diag bottom to orth stub
      Rules += sam~(11,0,2,0) | (Street ~> sam)~(2,0,11,0)  // orth-diag bottom to orth-diag bottom 1
      Rules += sam~(11,0,2,0) | (Street ~> sam)~(2,0,13,0)  // orth-diag bottom to orth-diag bottom 2
      Rules += sam~(13,0,2,0) | (Street ~> sam)~(2,0,11,0)  // orth-diag bottom to orth-diag bottom 3
      // Rules += sam~WE | (Street ~> sam)~(2,2,2,2) //alternate attempt at OxO +
      // Rules += sam~(2,2,2,2) | (Street ~> sam)~WE //alternate attempt at OxO continue
      // Rules += sam~(2,2,2,2) | (Street ~> sam)~WC //alternate attempt at OxO continue stub

      // curves
      createSamLarge45Curve(sam)
      createSam2x290Curve(sam)
      createSamSCurve(sam)
      createSamLarge90Curve(sam)

      // sam & sam intersections
      Rules += sam~WE | (Street ~> sam)~WE & (Street ~> sam)~NS           // OxO from orth
      Rules += sam~(0,0,2,2) | (Street ~> sam)~WE & (Street ~> sam)~NS    // OxO from 90-bend
      Rules += sam~(11,0,2,0) | (Street ~> sam)~WE & (Street ~> sam)~NS   // OxO from orth-diag
      Rules += sam~WE & sam~NS | (Street ~> sam)~WE                       // OxO continue
      Rules += sam~WE & sam~NS | (Street ~> sam)~WC                       // OxO continue stub
      Rules += sam~WE & sam~NS | (Street ~> sam)~(2,2,0,0)                // OxO continue 90-bend
      Rules += sam~WE & sam~NS | (Street ~> sam)~(2,0,11,0)               // OxO continue orth-diag
      Rules += sam~WE & sam~NS | (Street ~> sam)~WE & (Street ~> sam)~NS  // OxO continue +
      Rules += sam~WE & sam~NS | (Street ~> sam)~WE & (Street ~> sam)~NC  // OxO continue T-Thru Side
      Rules += sam~WE & sam~NS | (Street ~> sam)~WC & (Street ~> sam)~NS  // OxO continue T-End Side
      Rules += sam~WE & sam~NS | (Street ~> sam)~WE & (Street ~> sam)~ES  // OxO to OxD	  

      Rules += sam~WE | (Street ~> sam)~WE & (Street ~> sam)~NC           // OxO T Thru-Side from orth
      Rules += sam~(0,0,2,2) | (Street ~> sam)~WE & (Street ~> sam)~NC    // OxO T Thru-Side from 90-bend
      Rules += sam~(11,0,2,0) | (Street ~> sam)~WE & (Street ~> sam)~NC   // OxO T Thru-Side from orth-diag
      Rules += sam~WE & sam~NC | (Street ~> sam)~WE                       // OxO T Thru-Side continue orth 1
      Rules += sam~WE & sam~CS | (Street ~> sam)~WE                       // OxO T Thru-Side continue orth 2
      Rules += sam~WE & sam~NC | (Street ~> sam)~WC                       // OxO T Thru-Side continue stub orth stub 1
      Rules += sam~WE & sam~CS | (Street ~> sam)~WC                       // OxO T Thru-Side continue stub orth stub 2
      Rules += sam~WE & sam~NC | (Street ~> sam)~(2,2,0,0)                // OxO T Thru-Side continue sharp-90 1
      Rules += sam~WE & sam~CS | (Street ~> sam)~(2,2,0,0)                // OxO T Thru-Side continue sharp-90 2
      Rules += sam~WE & sam~NC | (Street ~> sam)~(2,0,11,0)               // OxO T Thru-Side continue orth-diag 1
      Rules += sam~WE & sam~CS | (Street ~> sam)~(2,0,11,0)               // OxO T Thru-Side continue orth-diag 2
      Rules += sam~WE & sam~NC | (Street ~> sam)~WE & (Street ~> sam)~NS  // OxO T Thru-Side + 1
      Rules += sam~WE & sam~CS | (Street ~> sam)~WE & (Street ~> sam)~NS  // OxO T Thru-Side + 2
      Rules += sam~WE & sam~NC | (Street ~> sam)~WE & (Street ~> sam)~NC  // OxO T Thru-Side to Thru-Side 1a
      Rules += sam~WE & sam~NC | (Street ~> sam)~WE & (Street ~> sam)~CS  // OxO T Thru-Side to Thru-Side 1b
      Rules += sam~WE & sam~CS | (Street ~> sam)~WE & (Street ~> sam)~NC  // OxO T Thru-Side to Thru-Side 2a
      Rules += sam~WE & sam~CS | (Street ~> sam)~WE & (Street ~> sam)~CS  // OxO T Thru-Side to Thru-Side 2b
      Rules += sam~WE & sam~NC | (Street ~> sam)~WC & (Street ~> sam)~NS  // OxO T Thru-Side to End-Side 1
      Rules += sam~WE & sam~CS | (Street ~> sam)~WC & (Street ~> sam)~NS  // OxO T Thru-Side to End-Side 2	

      Rules += sam~WE | (Street ~> sam)~WC & (Street ~> sam)~NS           // OxO T End-Side from orth
      Rules += sam~(0,0,2,2) | (Street ~> sam)~WC & (Street ~> sam)~NS    // OxO T End-Side from 90-bend
      Rules += sam~(11,0,2,0) | (Street ~> sam)~WC & (Street ~> sam)~NS   // OxO T End-Side from orth-diag
      Rules += sam~CE & sam~NS | (Street ~> sam)~WE                       // OxO T End-Side continue orth 1
      Rules += sam~CE & sam~NS | (Street ~> sam)~WC                       // OxO T End-Side continue stub orth stub 1
      Rules += sam~CE & sam~NS | (Street ~> sam)~(2,2,0,0)                // OxO T End-Side continue sharp-90
      Rules += sam~CE & sam~NS | (Street ~> sam)~(2,0,11,0)               // OxO T End-Side continue orth-diag
      Rules += sam~CE & sam~NS | (Street ~> sam)~WE & (Street ~> sam)~NS  // OxO T End-Side + 1
      Rules += sam~CE & sam~NS | (Street ~> sam)~WE & (Street ~> sam)~NC  // OxO T End-Side to Thru-Side 1a
      Rules += sam~CE & sam~NS | (Street ~> sam)~WE & (Street ~> sam)~CS  // OxO T End-Side to Thru-Side 2a
      Rules += sam~CE & sam~NS | (Street ~> sam)~WC & (Street ~> sam)~NS  // OxO T End-Side to End-Side 1

      Rules += sam~WE | (Street ~> sam)~WE & (Street ~> sam)~ES  // OxD from orth
      Rules += sam~WE | (Street ~> sam)~WC & (Street ~> sam)~ES  // OxD T from orth

      Rules += sam~WE & sam~ES | (Street ~> sam)~WE & (Street ~> sam)~NW  // OxD Tile 2
      Rules += sam~(0,0,2,2) | (Street ~> sam)~WE & (Street ~> sam)~ES    // OxD from 90-bend
      Rules += sam~(11,0,2,0) | (Street ~> sam)~WE & (Street ~> sam)~ES   // OxD from orth-diag
      Rules += sam~WE & sam~NW | (Street ~> sam)~WE           // OxD orth continue
      Rules += sam~WE & sam~NW | (Street ~> sam)~WC           // OxD orth continue stub
      Rules += sam~WE & sam~NW | (Street ~> sam)~(2,2,0,0)    // OxD orth continue 90-bend
      Rules += sam~WE & sam~NW | (Street ~> sam)~(2,0,11,0)   // OxD orth continue orth-diag
      Rules += sam~WE & sam~NW | (Street ~> sam)~WE & (Street ~> sam)~NS    // OxD to OxO + continue
      Rules += sam~WE & sam~NW | (Street ~> sam)~WE & (Street ~> sam)~NC    // OxD continue to OxO T Thru-Side from orth
      Rules += sam~WE & sam~NW | (Street ~> sam)~WC & (Street ~> sam)~NS    // OxD continue to OxO T End-Side from orth
      Rules += sam~WE & sam~NW | (Street ~> sam)~WE & (Street ~> sam)~ES    // OxD to OxD + continue

      Rules += sam~WE & sam~ES | Street~WN | sam~WC & sam~ES | sam~WN & sam~WC  // OxD Tile 2 T
      Rules += sam~WE & sam~ES | sam~WN | sam~WC & sam~ES | sam~WN & sam~WC     // OxD Tile 2 T
      Rules += sam~WE & sam~ES | Street~WN & Street~WC | sam~WC & sam~ES | sam~WN & sam~WC  // OxD Tile 2 T
      Rules += sam~WC & sam~ES | (Street ~> sam)~WN & (Street ~> sam)~WC  // OxD Tile 2 T
      Rules += sam~WC & sam~ES | Street~WN | % | sam~WN & sam~WC          // OxD Tile 2 T
      Rules += sam~WC & sam~ES | sam~WN | % | sam~WN & sam~WC             // OxD Tile 2 T
      Rules += sam~NE | Street~SW & Street~WE | sam~NE & sam~EC | sam~SW & sam~EC           // OxD T from Tile 2 to Tile 1
      Rules += sam~NE | Street~SW & Street~EC | sam~NE & sam~EC | sam~SW & sam~EC           // OxD T from Tile 2 to Tile 1
      Rules += sam~NE & sam~EC | Street~SW & Street~WE | sam~NE & sam~EC | sam~SW & sam~EC  // OxD T from Tile 2 to Tile 1
      Rules += sam~NE & sam~EC | (Street ~> sam)~SW & (Street ~> sam)~EC                    // OxD T from Tile 2 to Tile 1
      Rules += sam~SW & sam~EC | (Street ~> sam)~WE           // OxD T orth continue
      Rules += sam~SW & sam~EC | (Street ~> sam)~WC           // OxD T orth continue stub
      Rules += sam~SW & sam~EC | (Street ~> sam)~(2,2,0,0)    // OxD T orth continue 90-bend
      Rules += sam~SW & sam~EC | (Street ~> sam)~(2,0,11,0)   // OxD T orth continue orth-diag


      Rules += sam~(0,0,11,3) | Street~CNW & Street~NS | sam~(0,0,11,3) | sam~CNW & sam~NS  // DxO T from Orth-Diag Top
      Rules += sam~ES | Street~WC & Street~NS | sam~(0,0,11,3) | sam~CNW & sam~NS           // DxO T from Diag for Old-Style Diag Streets
      Rules += sam~(0,0,11,3) | Street~WC & Street~NS | sam~(0,0,11,3) | sam~CNW & sam~NS   // DxO T from Orth-Diag Top for Old-Style Diag Streets


      Rules += sam~NE & sam~NC | (Street ~> sam)~SW
      Rules += sam~NE | (Street ~> sam)~SW & (Street ~> sam)~CS


      Rules += sam~ES | (Street ~> sam)~NW & (Street ~> sam)~NS   // DxO
      Rules += sam~ES | (Street ~> sam)~NW & (Street ~> sam)~CS   // DxO T
      Rules += sam~ES & sam~NS | (Street ~> sam)~NW               // DxO diag continue
      Rules += sam~ES & sam~NS | (Street ~> sam)~CNW              // DxO diag stub continue
      Rules += sam~ES & sam~NC | (Street ~> sam)~NW               // DxO T diag continue
      Rules += sam~ES & sam~NC | (Street ~> sam)~CNW              // DxO T diag stub continue

      Rules += sam~ES | (Street ~> sam)~NW & (Street ~> sam)~EN               // DxD
      Rules += sam~ES | (Street ~> sam)~CNW & (Street ~> sam)~EN              // DxD T (End)
      Rules += sam~ES | (Street ~> sam)~NW & (Street ~> sam)~CNE              // DxD T (Thru 1)
      Rules += sam~NE | (Street ~> sam)~(3,0,0,1) & (Street ~> sam)~(1,0,0,0) // DxD T (Thru 2)

      //Orth-Diag into Diag Fixes
      Rules += sam~(0,0,11,3) | Street~NW & Street~EN | sam~ES | sam~NW & sam~EN    // DxD
      Rules += sam~(0,0,11,3) | Street~CNW & Street~EN | sam~ES | sam~CNW & sam~EN  // DxD T (End)
      Rules += sam~(0,0,11,3) | Street~NW & Street~CNE | sam~ES | sam~NW & sam~CNE  // DxD T (Thru 1)
      Rules += sam~(0,1,13,0) | Street~(3,0,0,1) & Street~(1,0,0,0) | sam~NE | sam~(3,0,0,1) & sam~(1,0,0,0)  // DxD T (Thru 2)
      Rules += sam~(0,0,11,3) | sam~NW & sam~EN | sam~ES | sam~NW & sam~EN                              // DxD
      Rules += sam~(0,0,11,3) | sam~CNW & sam~EN | sam~ES | sam~CNW & sam~EN                            // DxD T (End)
      Rules += sam~(0,0,11,3) | sam~NW & sam~CNE | sam~ES | sam~NW & sam~CNE                            // DxD T (Thru 1)
      Rules += sam~(0,1,13,0) | sam~(3,0,0,1) & sam~(1,0,0,0) | sam~NE | sam~(3,0,0,1) & sam~(1,0,0,0)  // DxD T (Thru 2)

      Rules += sam~SE & sam~EN | (Street ~> sam)~NW & (Street ~> sam)~WS                        // DxD Tile 2
      Rules += sam~SE & sam~EN | sam~(2,13,0,11) | % |  sam~NW & sam~WS                         // DxD Tile 2 Fix 1
      Rules += sam~(0,11,2,13) | sam~(2,13,0,11) | sam~SE & sam~EN |  sam~NW & sam~WS           // DxD Tile 2 Fix 2
      Rules += sam~SE & sam~EN | Street~WN | sam~SE & sam~CEN | sam~(1,3,0,0) & sam~(0,1,0,0)   // DxD Tile 2 T
      Rules += sam~SE & sam~EN | sam~WN | sam~SE & sam~CEN | sam~(1,3,0,0) & sam~(0,1,0,0)      // DxD Tile 2 T
      Rules += sam~SE & sam~CEN | Street~WN | sam~SE & sam~CEN | sam~(1,3,0,0) & sam~(0,1,0,0)  // DxD Tile 2 T
      Rules += sam~SE & sam~CEN | sam~WN | sam~SE & sam~CEN | sam~(1,3,0,0) & sam~(0,1,0,0)     // DxD Tile 2 T
      Rules += sam~SE & sam~EN | Street~(1,3,0,0) & Street~(0,1,0,0) | sam~SE & sam~CEN | sam~(1,3,0,0) & sam~(0,1,0,0)   // DxD Tile 2 T
      Rules += sam~SE & sam~CEN | Street~(1,3,0,0) & Street~(0,1,0,0) | sam~SE & sam~CEN | sam~(1,3,0,0) & sam~(0,1,0,0)  // DxD Tile 2 T
      Rules += sam~(0,0,1,3) & sam~(0,0,0,1) | (Street ~> sam)~NW & (Street ~> sam)~CWS         // DxD Tile 1 T from Tile 2

      Rules += sam~SE & sam~WS | (Street ~> sam)~NW                 // DxD continue
      Rules += sam~SE & sam~WS | Street~(11,3,0,0) | % | sam~NW     // DxD continue fix
      Rules += sam~SE & sam~WS | (Street ~> sam)~CNW                // DxD continue stub
      Rules += sam~CSE & sam~WS | (Street ~> sam)~NW                // DxD T (End Side) continue
      Rules += sam~CSE & sam~WS | (Street ~> sam)~CNW               // DxD T (End Side) continue stub
      Rules += sam~CNW & sam~NE | (Street ~> sam)~WS                // DxD T (Thru Side 1) continue
      Rules += sam~CNW & sam~NE | (Street ~> sam)~CWS               // DxD T (Thru Side 1) continue stub
      Rules += sam~(0,1,3,0) & sam~(0,0,1,0) | (Street ~> sam)~WS   // DxD T (Thru Side 2) continue
      Rules += sam~(0,1,3,0) & sam~(0,0,1,0) | (Street ~> sam)~CWS  // DxD T (Thru Side 2) continue stub

      /* 
      Rules += sam~SE & sam~EN | Street~NW | % | sam~NW & sam~CWS	// DxD Tile 2 T
      Rules += sam~SE & sam~EN | sam~NW | % | sam~NW & sam~CWS	// DxD Tile 2 T
      Rules += sam~SE & sam~EN | (Street ~> sam)~NW & (Street ~> sam)~CWS		// DxD Tile 2 T
      Rules += sam~SE & sam~CEN | (Street ~> sam)~NW & (Street ~> sam)~CWS		// DxD Tile 2 T
      Rules += sam~SE & sam~CEN | Street~NW | % | sam~NW & sam~CWS	// DxD Tile 2 T
      Rules += sam~SE & sam~CEN | sam~NW | % | sam~NW & sam~CWS	// DxD Tile 2 T
      Rules += sam~NE & sam~CNW | (Street ~> sam)~SW
      Rules += sam~NE | (Street ~> sam)~SW & (Street ~> sam)~CSE
      */
	  
      // non-standard self-intersections
      // T-ints
      // from ortho
      Rules += sam~WE | (Street ~> sam)~(2,11,2,0)
      Rules += sam~WE | (Street ~> sam)~(2,13,2,0)

      Rules += sam~WE | (Street ~> sam)~(2,2,11,0)
      Rules += sam~WE | (Street ~> sam)~(2,2,13,0)

      Rules += sam~WE | (Street ~> sam)~(2,11,0,2)
      Rules += sam~WE | (Street ~> sam)~(2,13,0,2)

      Rules += sam~WE | (Street ~> sam)~(2,11,11,0)
      Rules += sam~WE | (Street ~> sam)~(2,13,13,0)

      // Rules += sam~WE | (Street ~> sam)~(2,11,0,13)
      Rules += sam~WE | (Street ~> sam)~(2,13,0,11)
      // from orth-diag top
      Rules += sam~(0,0,11,3) | (Street ~> sam)~(11,2,0,2)
      Rules += sam~(0,0,11,3) | (Street ~> sam)~(11,0,2,2)
      Rules += sam~(0,0,11,3) | (Street ~> sam)~(11,2,2,0)
      Rules += sam~(0,0,1,3) | Street~(11,2,0,2) | sam~(0,0,11,3) | %
      Rules += sam~(0,0,1,3) | Street~(11,0,2,2) | sam~(0,0,11,3) | %
      Rules += sam~(0,0,1,3) | Street~(11,2,2,0) | sam~(0,0,11,3) | %
      Rules += sam~(0,0,1,3) | sam~(11,2,0,2) | sam~(0,0,11,3) | %
      Rules += sam~(0,0,1,3) | sam~(11,0,2,2) | sam~(0,0,11,3) | %
      Rules += sam~(0,0,1,3) | sam~(11,2,2,0) | sam~(0,0,11,3) | %

      Rules += sam~(0,1,13,0) | (Street ~> sam)~(13,2,0,2)
      Rules += sam~(0,1,13,0) | (Street ~> sam)~(13,0,2,2)
      Rules += sam~(0,1,13,0) | (Street ~> sam)~(13,2,2,0)
      Rules += sam~(0,1,13,0) | Street~(13,2,0,2) | sam~(0,1,13,0) | %
      Rules += sam~(0,1,13,0) | Street~(13,0,2,2) | sam~(0,1,13,0) | %
      Rules += sam~(0,1,13,0) | Street~(13,2,2,0) | sam~(0,1,13,0) | %

      Rules += sam~(0,0,11,3) | (Street ~> sam)~(11,2,0,11)
      Rules += sam~(0,0,11,3) | (Street ~> sam)~(11,0,2,11)
      Rules += sam~(0,0,11,3) | (Street ~> sam)~(11,11,2,0)
      Rules += sam~(0,0,1,3) | Street~(11,2,0,11) | sam~(0,0,11,3) | sam~(11,2,0,11)
      Rules += sam~(0,0,1,3) | Street~(11,0,2,11) | sam~(0,0,11,3) | sam~(11,2,0,11)
      Rules += sam~(0,0,1,3) | Street~(11,11,2,0) | sam~(0,0,11,3) | sam~(11,11,0,2)
      Rules += sam~(0,0,1,3) | sam~(11,2,0,11) | sam~(0,0,11,3) | sam~(11,2,0,11)
      Rules += sam~(0,0,1,3) | sam~(11,0,2,11) | sam~(0,0,11,3) | sam~(11,2,0,11)
      Rules += sam~(0,0,1,3) | sam~(11,11,2,0) | sam~(0,0,11,3) | sam~(11,11,0,2)

      Rules += sam~(0,1,13,0) | (Street ~> sam)~(13,2,0,13)
      Rules += sam~(0,1,13,0) | (Street ~> sam)~(13,0,2,13)
      Rules += sam~(0,1,13,0) | (Street ~> sam)~(13,13,2,0)
      Rules += sam~(0,1,3,0) | Street~(13,2,0,13) | sam~(0,1,13,0) | sam~(13,2,0,13)
      Rules += sam~(0,1,3,0) | Street~(13,0,2,13) | sam~(0,1,13,0) | sam~(13,2,0,13)
      Rules += sam~(0,1,3,0) | Street~(13,13,2,0) | sam~(0,1,13,0) | sam~(13,13,0,2)
      Rules += sam~(0,1,3,0) | sam~(13,2,0,13) | sam~(0,1,13,0) | sam~(13,2,0,13)
      Rules += sam~(0,1,3,0) | sam~(13,0,2,13) | sam~(0,1,13,0) | sam~(13,2,0,13)
      Rules += sam~(0,1,3,0) | sam~(13,13,2,0) | sam~(0,1,13,0) | sam~(13,13,0,2)

      Rules += sam~(0,0,11,3) | (Street ~> sam)~(11,2,13,0)
      Rules += sam~(0,0,1,3) | Street~(11,2,13,0) | sam~(0,0,11,3) | sam~(11,2,13,0)
      Rules += sam~(0,0,1,3) | sam~(11,2,13,0) | sam~(0,0,11,3) | sam~(11,2,13,0)

      Rules += sam~(0,1,13,0) | (Street ~> sam)~(13,0,11,2)
      Rules += sam~(0,1,3,0) | Street~(13,0,11,2) | sam~(0,1,13,0) | sam~(13,0,11,2)
      Rules += sam~(0,1,3,0) | sam~(13,0,11,2) | sam~(0,1,13,0) | sam~(13,0,11,2)

      // continuation
      Rules += sam~(2,11,2,0) | (Street ~> sam)~WE
      Rules += sam~(2,13,2,0) | (Street ~> sam)~WE
      Rules += sam~(2,11,2,0) | (Street ~> sam)~WC
      Rules += sam~(2,13,2,0) | (Street ~> sam)~WC

      Rules += sam~(11,2,2,0) | (Street ~> sam)~WE
      Rules += sam~(13,2,2,0) | (Street ~> sam)~WE
      Rules += sam~(11,2,2,0) | (Street ~> sam)~WC
      Rules += sam~(13,2,2,0) | (Street ~> sam)~WC

      Rules += sam~(0,2,2,11) | (Street ~> sam)~WE
      Rules += sam~(0,2,2,13) | (Street ~> sam)~WE
      Rules += sam~(0,2,2,11) | (Street ~> sam)~WC
      Rules += sam~(0,2,2,13) | (Street ~> sam)~WC

      Rules += sam~(2,11,2,0) | (Street ~> sam)~WE
      Rules += sam~(2,13,2,0) | (Street ~> sam)~WE
      Rules += sam~(2,11,2,0) | (Street ~> sam)~WC
      Rules += sam~(2,13,2,0) | (Street ~> sam)~WC

      Rules += sam~(11,11,2,0) | (Street ~> sam)~WE
      Rules += sam~(13,13,2,0) | (Street ~> sam)~WE
      Rules += sam~(11,11,2,0) | (Street ~> sam)~WC
      Rules += sam~(13,13,2,0) | (Street ~> sam)~WC
	  
      Rules += sam~(2,2,11,0) | (Street ~> sam)~(11,3,0,0)
      Rules += sam~(2,0,11,2) | (Street ~> sam)~(11,3,0,0)
      Rules += sam~(0,2,11,2) | (Street ~> sam)~(11,3,0,0)
      Rules += sam~(2,2,11,0) | Street~(1,3,0,0) | % | sam~(11,3,0,0)
      Rules += sam~(2,0,11,2) | Street~(1,3,0,0) | % | sam~(11,3,0,0)
      Rules += sam~(0,2,11,2) | Street~(1,3,0,0) | % | sam~(11,3,0,0)
      Rules += sam~(2,2,11,0) | sam~(1,3,0,0) | % | sam~(11,3,0,0)
      Rules += sam~(2,0,11,2) | sam~(1,3,0,0) | % | sam~(11,3,0,0)
      Rules += sam~(0,2,11,2) | sam~(1,3,0,0) | % | sam~(11,3,0,0)

      Rules += sam~(2,11,11,0) | (Street ~> sam)~(11,3,0,0)
      Rules += sam~(2,0,11,11) | (Street ~> sam)~(11,3,0,0)
      Rules += sam~(0,11,11,2) | (Street ~> sam)~(11,3,0,0)
      Rules += sam~(2,11,11,0) | Street~(1,3,0,0) | % | sam~(11,3,0,0)
      Rules += sam~(2,0,11,11) | Street~(1,3,0,0) | % | sam~(11,3,0,0)
      Rules += sam~(0,11,11,2) | Street~(1,3,0,0) | % | sam~(11,3,0,0)
      Rules += sam~(2,11,11,0) | sam~(1,3,0,0) | % | sam~(11,3,0,0)
      Rules += sam~(2,0,11,11) | sam~(1,3,0,0) | % | sam~(11,3,0,0)
      Rules += sam~(0,11,11,2) | sam~(1,3,0,0) | % | sam~(11,3,0,0)

      // +-intersections
      // from ortho
      Rules += sam~WE | (Street ~> sam)~(2,11,2,2)
      Rules += sam~WE | (Street ~> sam)~(2,13,2,2)
      Rules += sam~WE | (Street ~> sam)~(2,2,11,2)
      // from orth-diag top
      Rules += sam~(0,0,11,3) | (Street ~> sam)~(11,2,2,2)
      Rules += sam~(0,0,1,3) | Street~(11,2,2,2) | sam~(0,0,11,3) | sam~(11,2,2,2)
      // continuation
      Rules += sam~(2,11,2,2) | (Street ~> sam)~WE
      Rules += sam~(2,13,2,2) | (Street ~> sam)~WE
      Rules += sam~(2,11,2,2) | (Street ~> sam)~WC
      Rules += sam~(2,13,2,2) | (Street ~> sam)~WC
      Rules += sam~(2,2,11,2) | (Street ~> sam)~(11,3,0,0)
      Rules += sam~(2,2,11,2) | Street~(1,3,0,0) | % | sam~(11,3,0,0)
      Rules += sam~(2,2,11,2) | sam~(1,3,0,0) | % | sam~(11,3,0,0)
      Rules += sam~(11,2,2,2) | (Street ~> sam)~WE
      Rules += sam~(11,2,2,2) | (Street ~> sam)~WC

      // temp for OxD Ts until proper T-half is added
      Rules += sam~WE & sam~SE | (Street ~> sam)~NW //initial
      Rules += sam~NS & sam~NE | (Street ~> sam)~WS
	  
      // Street Roundabouts
      Rules += sam~WE | (Street ~> sam)~(2,102,102,0) // orth into roundabout x street

      Rules += sam~(0,0,102,102) | (Street ~> sam)~(102,0,0,102) // Street roundabout to Street roundabout
      Rules += sam~(0,0,102,102) | (Street ~> sam)~(102,2,0,102) // Street roundabout to roundabout x street 1
      Rules += sam~(0,0,102,102) | (Street ~> sam)~(102,0,2,102) // Street roundabout to roundabout x street 2
      Rules += sam~(0,2,102,102) | (Street ~> sam)~(102,0,0,102) // roundabout x street to Street roundabout 1
      Rules += sam~(2,0,102,102) | (Street ~> sam)~(102,0,0,102) // roundabout x street to Street roundabout 2
      Rules += sam~(0,2,102,102) | (Street ~> sam)~(102,2,0,102) // roundabout x street to roundabout x street 1a
      Rules += sam~(0,2,102,102) | (Street ~> sam)~(102,0,2,102) // roundabout x street to roundabout x street 1b
      Rules += sam~(2,0,102,102) | (Street ~> sam)~(102,2,0,102) // roundabout x street to roundabout x street 2a
      Rules += sam~(2,0,102,102) | (Street ~> sam)~(102,0,2,102) // roundabout x street to roundabout x street 2b
      Rules += sam~(0,102,102,2) | (Street ~> sam)~(102,102,2,0) // roundabout x street to roundabout x street 3a
      Rules += sam~(0,102,102,2) | (Street ~> sam)~(102,102,0,2) // roundabout x street to roundabout x street 3b
      Rules += sam~(2,102,102,0) | (Street ~> sam)~(102,102,2,0) // roundabout x street to roundabout x street 4a
      Rules += sam~(2,102,102,0) | (Street ~> sam)~(102,102,0,2) // roundabout x street to roundabout x street 4b

      Rules += sam~(0,0,102,102) | (Street ~> sam)~(102,0,0,102) & Road~(0,2,0,0) // Street roundabout to roundabout x road 1
      Rules += sam~(0,0,102,102) | (Street ~> sam)~(102,0,0,102) & Road~(0,0,2,0) // Street roundabout to roundabout x road 2
      Rules += sam~(0,0,102,102) & Road~(0,2,0,0) | (Street ~> sam)~(102,0,0,102) // roundabout x road to Street roundabout 1
      Rules += sam~(0,0,102,102) & Road~(2,0,0,0) | (Street ~> sam)~(102,0,0,102) // roundabout x road to Street roundabout 2

      Rules += sam~(0,2,102,102) | (Street ~> sam)~(102,0,0,102) & Road~(0,2,0,0) // Street roundabout x street to roundabout x road 1a
      Rules += sam~(2,0,102,102) | (Street ~> sam)~(102,0,0,102) & Road~(0,2,0,0) // Street roundabout x street to roundabout x road 1b
      Rules += sam~(0,2,102,102) | (Street ~> sam)~(102,0,0,102) & Road~(0,0,2,0) // Street roundabout x street to roundabout x road 2a
      Rules += sam~(2,0,102,102) | (Street ~> sam)~(102,0,0,102) & Road~(0,0,2,0) // Street roundabout x street to roundabout x road 2b
      Rules += sam~(0,0,102,102) & Road~(0,2,0,0) | (Street ~> sam)~(102,2,0,102) // roundabout x road to roundabout x street 1a
      Rules += sam~(0,0,102,102) & Road~(2,0,0,0) | (Street ~> sam)~(102,2,0,102) // roundabout x road to roundabout x street 1b
      Rules += sam~(0,0,102,102) & Road~(0,2,0,0) | (Street ~> sam)~(102,0,2,102) // roundabout x road to roundabout x street 2a
      Rules += sam~(0,0,102,102) & Road~(2,0,0,0) | (Street ~> sam)~(102,0,2,102) // roundabout x road to roundabout x street 2b
      Rules += sam~(0,102,102,0) & Road~(0,0,0,2) | (Street ~> sam)~(102,102,2,0) // roundabout x road to roundabout x street 3a
      Rules += sam~(0,102,102,0) & Road~(0,0,0,2) | (Street ~> sam)~(102,102,0,2) // roundabout x road to roundabout x street 3b
      Rules += sam~(0,102,102,0) & Road~(2,0,0,0) | (Street ~> sam)~(102,102,2,0) // roundabout x road to roundabout x street 4a
      Rules += sam~(0,102,102,0) & Road~(2,0,0,0) | (Street ~> sam)~(102,102,0,2) // roundabout x road to roundabout x street 4b

      Rules += sam~(0,0,102,102) & Road~(0,2,0,0) | (Street ~> sam)~(102,0,0,102) & Road~(0,2,0,0) // roundabout x road to roundabout x road 1a
      Rules += sam~(0,0,102,102) & Road~(2,0,0,0) | (Street ~> sam)~(102,0,0,102) & Road~(0,2,0,0) // roundabout x road to roundabout x road 1b
      Rules += sam~(0,0,102,102) & Road~(0,2,0,0) | (Street ~> sam)~(102,0,0,102) & Road~(0,0,2,0) // roundabout x road to roundabout x road 2a
      Rules += sam~(0,0,102,102) & Road~(2,0,0,0) | (Street ~> sam)~(102,0,0,102) & Road~(0,0,2,0) // roundabout x road to roundabout x road 2b
      Rules += sam~(0,102,102,0) & Road~(0,0,0,2) | (Street ~> sam)~(102,102,0,0) & Road~(0,0,2,0) // roundabout x road to roundabout x road 3a
      Rules += sam~(0,102,102,0) & Road~(0,0,0,2) | (Street ~> sam)~(102,102,0,0) & Road~(0,0,0,2) // roundabout x road to roundabout x road 3b
      Rules += sam~(0,102,102,0) & Road~(2,0,0,0) | (Street ~> sam)~(102,102,0,0) & Road~(0,0,2,0) // roundabout x road to roundabout x road 4a
      Rules += sam~(0,102,102,0) & Road~(2,0,0,0) | (Street ~> sam)~(102,102,0,0) & Road~(0,0,0,2) // roundabout x road to roundabout x road 4b

      Rules += sam~(0,0,102,102) & Road~(0,2,0,0) | (Street ~> sam)~(102,0,0,102) & Onewayroad~(0,2,0,0) // roundabout x road to roundabout x onewayroad 1a
      Rules += sam~(0,0,102,102) & Road~(2,0,0,0) | (Street ~> sam)~(102,0,0,102) & Onewayroad~(0,2,0,0) // roundabout x road to roundabout x onewayroad 1b
      Rules += sam~(0,0,102,102) & Road~(0,2,0,0) | (Street ~> sam)~(102,0,0,102) & Onewayroad~(0,0,2,0) // roundabout x road to roundabout x onewayroad 2a
      Rules += sam~(0,0,102,102) & Road~(2,0,0,0) | (Street ~> sam)~(102,0,0,102) & Onewayroad~(0,0,2,0) // roundabout x road to roundabout x onewayroad 2b
      Rules += sam~(0,102,102,0) & Road~(0,0,0,2) | (Street ~> sam)~(102,102,0,0) & Onewayroad~(0,0,2,0) // roundabout x road to roundabout x onewayroad 3a
      Rules += sam~(0,102,102,0) & Road~(0,0,0,2) | (Street ~> sam)~(102,102,0,0) & Onewayroad~(0,0,0,2) // roundabout x road to roundabout x onewayroad 3b
      Rules += sam~(0,102,102,0) & Road~(2,0,0,0) | (Street ~> sam)~(102,102,0,0) & Onewayroad~(0,0,2,0) // roundabout x road to roundabout x onewayroad 4a
      Rules += sam~(0,102,102,0) & Road~(2,0,0,0) | (Street ~> sam)~(102,102,0,0) & Onewayroad~(0,0,0,2) // roundabout x road to roundabout x onewayroad 4b

      Rules += sam~(0,0,102,102) | (Street ~> sam)~(102,0,0,102) & Onewayroad~(0,2,0,0) // Street roundabout to roundabout x onewayroad 1
      Rules += sam~(0,0,102,102) | (Street ~> sam)~(102,0,0,102) & Onewayroad~(0,0,2,0) // Street roundabout to roundabout x onewayroad 2
      Rules += sam~(0,0,102,102) & Onewayroad~(0,2,0,0) | (Street ~> sam)~(102,0,0,102) // roundabout x onewayroad to Street roundabout 1
      Rules += sam~(0,0,102,102) & Onewayroad~(2,0,0,0) | (Street ~> sam)~(102,0,0,102) // roundabout x onewayroad to Street roundabout 2

      Rules += sam~(0,2,102,102) | (Street ~> sam)~(102,0,0,102) & Onewayroad~(0,2,0,0) // Street roundabout x street to roundabout x onewayroad 1a
      Rules += sam~(2,0,102,102) | (Street ~> sam)~(102,0,0,102) & Onewayroad~(0,2,0,0) // Street roundabout x street to roundabout x onewayroad 1b
      Rules += sam~(0,2,102,102) | (Street ~> sam)~(102,0,0,102) & Onewayroad~(0,0,2,0) // Street roundabout x street to roundabout x onewayroad 2a
      Rules += sam~(2,0,102,102) | (Street ~> sam)~(102,0,0,102) & Onewayroad~(0,0,2,0) // Street roundabout x street to roundabout x onewayroad 2b
      Rules += sam~(0,0,102,102) & Onewayroad~(0,2,0,0) | (Street ~> sam)~(102,2,0,102) // roundabout x onewayroad to roundabout x street 1a
      Rules += sam~(0,0,102,102) & Onewayroad~(2,0,0,0) | (Street ~> sam)~(102,2,0,102) // roundabout x onewayroad to roundabout x street 1b
      Rules += sam~(0,0,102,102) & Onewayroad~(0,2,0,0) | (Street ~> sam)~(102,0,2,102) // roundabout x onewayroad to roundabout x street 2a
      Rules += sam~(0,0,102,102) & Onewayroad~(2,0,0,0) | (Street ~> sam)~(102,0,2,102) // roundabout x onewayroad to roundabout x street 2b
      Rules += sam~(0,0,102,102) & Onewayroad~(0,2,0,0) | (Street ~> sam)~(102,0,0,102) & Road~(0,2,0,0) // roundabout x onewayroad to roundabout x road 1a
      Rules += sam~(0,0,102,102) & Onewayroad~(2,0,0,0) | (Street ~> sam)~(102,0,0,102) & Road~(0,2,0,0) // roundabout x onewayroad to roundabout x road 1b
      Rules += sam~(0,0,102,102) & Onewayroad~(0,2,0,0) | (Street ~> sam)~(102,0,0,102) & Road~(0,0,2,0) // roundabout x onewayroad to roundabout x road 2a
      Rules += sam~(0,0,102,102) & Onewayroad~(2,0,0,0) | (Street ~> sam)~(102,0,0,102) & Road~(0,0,2,0) // roundabout x onewayroad to roundabout x road 2b
      Rules += sam~(0,102,102,0) & Onewayroad~(0,0,0,2) | (Street ~> sam)~(102,102,0,0) & Road~(0,0,2,0) // roundabout x onewayroad to roundabout x road 3a
      Rules += sam~(0,102,102,0) & Onewayroad~(0,0,0,2) | (Street ~> sam)~(102,102,0,0) & Road~(0,0,0,2) // roundabout x onewayroad to roundabout x road 3b
      Rules += sam~(0,102,102,0) & Onewayroad~(2,0,0,0) | (Street ~> sam)~(102,102,0,0) & Road~(0,0,2,0) // roundabout x onewayroad to roundabout x road 4a
      Rules += sam~(0,102,102,0) & Onewayroad~(2,0,0,0) | (Street ~> sam)~(102,102,0,0) & Road~(0,0,0,2) // roundabout x onewayroad to roundabout x road 4b

      Rules += sam~(0,0,102,102) & Onewayroad~(0,2,0,0) | (Street ~> sam)~(102,0,0,102) & Onewayroad~(0,2,0,0) // roundabout x onewayroad to roundabout x onewayroad 1a
      Rules += sam~(0,0,102,102) & Onewayroad~(2,0,0,0) | (Street ~> sam)~(102,0,0,102) & Onewayroad~(0,2,0,0) // roundabout x onewayroad to roundabout x onewayroad 1b
      Rules += sam~(0,0,102,102) & Onewayroad~(0,2,0,0) | (Street ~> sam)~(102,0,0,102) & Onewayroad~(0,0,2,0) // roundabout x onewayroad to roundabout x onewayroad 2a
      Rules += sam~(0,0,102,102) & Onewayroad~(2,0,0,0) | (Street ~> sam)~(102,0,0,102) & Onewayroad~(0,0,2,0) // roundabout x onewayroad to roundabout x onewayroad 2b
      Rules += sam~(0,102,102,0) & Onewayroad~(0,0,0,2) | (Street ~> sam)~(102,102,0,0) & Onewayroad~(0,0,2,0) // roundabout x onewayroad to roundabout x onewayroad 3a
      Rules += sam~(0,102,102,0) & Onewayroad~(0,0,0,2) | (Street ~> sam)~(102,102,0,0) & Onewayroad~(0,0,0,2) // roundabout x onewayroad to roundabout x onewayroad 3b
      Rules += sam~(0,102,102,0) & Onewayroad~(2,0,0,0) | (Street ~> sam)~(102,102,0,0) & Onewayroad~(0,0,2,0) // roundabout x onewayroad to roundabout x onewayroad 4a
      Rules += sam~(0,102,102,0) & Onewayroad~(2,0,0,0) | (Street ~> sam)~(102,102,0,0) & Onewayroad~(0,0,0,2) // roundabout x onewayroad to roundabout x onewayroad 4b

      Rules += sam~(102,0,2,102) | (Street ~> sam)~WE
      Rules += sam~(102,0,2,102) | (Street ~> sam)~WC
      Rules += sam~(102,0,2,102) | (Street ~> sam)~(2,0,0,2)
      Rules += sam~(102,0,2,102) | (Street ~> sam)~(2,2,0,0)
      Rules += sam~(102,0,2,102) | (Street ~> sam)~(2,0,11,0)
      Rules += sam~(102,0,2,102) | (Street ~> sam)~(2,0,13,0)
      Rules += sam~(102,0,2,102) | (Street ~> sam)~WE & (Street ~> sam)~NS
      Rules += sam~(102,0,2,102) | (Street ~> sam)~WE & (Street ~> sam)~NC
      Rules += sam~(102,0,2,102) | (Street ~> sam)~NS & (Street ~> sam)~WC
      Rules += sam~(102,0,2,102) | (Street ~> sam)~(2,0,131,0)
      Rules += sam~(102,0,2,102) | (Street ~> sam)~(2,0,131,2)
      Rules += sam~(102,0,2,102) | (Street ~> sam)~(2,0,133,0)
      Rules += sam~(102,0,2,102) | (Street ~> sam)~(2,2,133,0)
      Rules += sam~(102,0,2,102) | (Street ~> sam)~(2,0,151,0)
      Rules += sam~(102,0,2,102) | (Street ~> sam)~(2,2,153,0)
      Rules += sam~(102,0,2,102) | (Street ~> sam)~(2,0,153,0)
      Rules += sam~(102,0,2,102) | (Street ~> sam)~(2,0,151,2)

	  
      for (minor <- CrossNetworks) {
        
        /*if (!minor.isNwm) {  // can't do NWM until more SAM x NWM intersection tiles are defined
          createAdjacentIntersections(sam, Street, minor)
        }*/

        if (isSingleTile(minor)) {
          // OxO
          Rules += sam~WE | (Street ~> sam)~WE & minor~NS~SN          // OxO from orth
          Rules += sam~(0,0,2,2) | (Street ~> sam)~WE & minor~NS~SN   // OxO from sharp-90
          Rules += sam~(11,0,2,0) | (Street ~> sam)~WE & minor~NS~SN  // OxO from orth-diag
          Rules += sam~WE & minor~NS~SN | (Street ~> sam)~WE          // OxO continue
          Rules += sam~WE & minor~NS~SN | (Street ~> sam)~WC          // OxO continue stub
          Rules += sam~WE & minor~NS~SN | (Street ~> sam)~(2,2,0,0)   // OxO continue sharp-90
          Rules += sam~WE & minor~NS~SN | (Street ~> sam)~(2,0,11,0)  // OxO continue orth-diag

          // OxD (to do: consider asymmetrical)
          Rules += sam~WE | (Street ~> sam)~WE & minor~ES~SE                // OxD
          Rules += sam~WE & minor~ES~SE | (Street ~> sam)~WE & minor~WN~NW  // OxD Tile 2
          Rules += sam~WE & minor~WN~NW | (Street ~> sam)~WE                // OxD continue
          Rules += sam~WE & minor~WN~NW | (Street ~> sam)~WC                // OxD stub
          if (!minor.isNwm) { // can't do NWM until more SAM x NWM intersection tiles are defined
            // OxD T
            // Rules += sam~WE & minor~WN | Street~WN | % | sam~WC & sam~WN   // OxD T

            // DxO
            Rules += sam~ES | (Street ~> sam)~NW & minor~NS             // DxO
            Rules += sam~NE & minor~WE | (Street ~> sam)~WS & minor~WE  // DxO Tile 2
            Rules += sam~ES & minor~NS | (Street ~> sam)~NW             // DxO continue
						
            // Rules += sam~ES & minor~WE | minor~WN | % | sam~WC & minor~WN       // DxO T

            // DxD
            Rules += sam~ES | (Street ~> sam)~NW & minor~EN             // DxD
            Rules += sam~SE & minor~EN | (Street ~> sam)~NW & minor~WS  // DxD Tile 2
            Rules += sam~SE & minor~WS | (Street ~> sam)~NW             // DxD continue
          }
        }

        if (minor == Road || minor == Onewayroad/*|| minor == Dirtroad || minor == Rhw3 || minor == Mis || minor == Rhw4 || minor == Tla3 || minor == Ave2 || minor == Ard3 || minor == Owr1 || minor == Nrd4 */) { 
          // OxO T (sam thru)
          Rules += sam~WE | (Street ~> sam)~WE & minor~NC~CN
          Rules += sam~WE | (Street ~> sam)~WE & minor~CS~SC
          // continue
          Rules += sam~WE & minor~NC~CN | (Street ~> sam)~WE
          Rules += sam~WE & minor~CS~SC | (Street ~> sam)~WE
          // continue stub
          Rules += sam~WE & minor~NC~CN | (Street ~> sam)~WC
          Rules += sam~WE & minor~CS~SC | (Street ~> sam)~WC
		  
          // OxD T (sam thru)
          Rules += sam~WE | (Street ~> sam)~WE & minor~CWN
          // continue
          Rules += sam~WE & minor~CWN | (Street ~> sam)~WE
          // continue stub
          Rules += sam~WE & minor~CWN | (Street ~> sam)~WC

          // DxO T (sam thru)
          Rules += sam~SE | (Street ~> sam)~WN & minor~CE

          // continue
          Rules += sam~NE & minor~CS | (Street ~> sam)~SW
          Rules += sam~NE & minor~CS | (Street ~> sam)~CSW

          // DxD T (sam thru)
          Rules += sam~ES | (Street ~> sam)~WN & minor~CNE  // DxD T (Thru)
          // continue
          Rules += sam~ES & minor~CSW | (Street ~> sam)~WN  // Diagonal Continuation off DxD T-Thru
          Rules += sam~NE & minor~CES | (Street ~> sam)~WS  // Temp Diagonal Continuation off other side of DxD T-Thru
          // Rules += sam~NE & minor~CES | (Street ~> sam)~WS & minor~CNW  //Eventual DxD T-Thru Tile 2
		  
        }

        if (minor == Road || minor == Onewayroad/*|| minor == Dirtroad || minor == Rhw3 || minor == Mis || minor == Rhw4 || isNwm(minor)*/) { 
          // OxO T (sam end)
          Rules += sam~WE | (Street ~> sam)~WC & minor~NS
          // OxD T (sam thru)
          // DxO T (sam end)
          Rules += sam~ES | Street~CNW & minor~NS | sam~(0,0,11,3) | sam~CNW & minor~NS               // DxO T from Diag
          Rules += sam~(0,0,11,3) | Street~CNW & minor~NS | sam~(0,0,11,3) | sam~CNW & minor~NS       // DxO T from Orth-Diag Top
          Rules += sam~ES | Street~WC & minor~NS | sam~(0,0,11,3) | sam~CNW & minor~NS                // DxO T from Diag for Old-Style Diag Streets
          Rules += sam~(0,0,11,3) | Street~WC & minor~NS | sam~(0,0,11,3) | sam~CNW & minor~NS        // DxO T from Orth-Diag Top for Old-Style Diag Streets
          Rules += sam~CEN & minor~WE | sam~CWS & minor~WE | sam~NE & minor~WE | sam~SW & minor~WE    // DxO Ts into DxO + for Old-Style
          Rules += sam~CEN & minor~WE | Street~CWS & minor~WE | sam~NE & minor~WE | sam~SW & minor~WE // DxO Ts into DxO + for Old-Style
          Rules += sam~NE & minor~WE | sam~CWS & minor~WE | sam~NE & minor~WE | sam~SW & minor~WE     // DxO Ts into DxO + for Old-Style
          Rules += sam~NE & minor~WE | Street~CWS & minor~WE | sam~NE & minor~WE | sam~SW & minor~WE  // DxO Ts into DxO + for Old-Style

          // DxD T (sam end)
          Rules += sam~ES | (Street ~> sam)~CNW & minor~EN  // DxD T (End)
        }
		
        // non-standard two-network + and T ints
        if (minor == Road || minor == Onewayroad) {
          // intersections
          Rules += sam~WE | (Street ~> sam)~(2,2,0,0) & minor~(0,0,1,3)
          // Rules += sam~WE | (Street ~> sam)~(2,0,0,2) & minor~(0,1,3,0)
          Rules += sam~WE | (Street ~> sam)~(2,0,0,0) & minor~(0,2,2,2)
          Rules += sam~WE | (Street ~> sam)~(2,2,0,0) & minor~(0,0,2,2)
          Rules += sam~WE | (Street ~> sam)~(2,2,2,0) & minor~(0,0,0,2)
          Rules += sam~WE | (Street ~> sam)~(2,2,0,2) & minor~(0,0,2,0)
          Rules += sam~WE | (Street ~> sam)~(2,0,0,0) & minor~(0,2,2,0)
          Rules += sam~WE | (Street ~> sam)~(2,2,0,0) & minor~(0,0,2,0)
          Rules += sam~WE | (Street ~> sam)~(2,0,0,2) & minor~(0,2,0,0)
          // continuations
          Rules += sam~(0,0,2,2) & minor~(1,3,0,0) | (Street ~> sam)~WE
          Rules += sam~(0,0,2,2) & minor~(2,2,0,0) | (Street ~> sam)~WE
          Rules += sam~(0,2,2,2) & minor~(2,0,0,0) | (Street ~> sam)~WE
          Rules += sam~(2,2,2,0) & minor~(0,0,0,2) | (Street ~> sam)~WE
          Rules += sam~(0,0,2,2) & minor~(2,0,0,0) | (Street ~> sam)~WE
          Rules += sam~(0,0,2,2) & minor~(0,2,0,0) | (Street ~> sam)~WE
        }		

        if (minor.typ == AvenueLike) {
          // OxO
          Rules += sam~WE | (Street ~> sam)~WE & minor~NS             // OxO
          Rules += sam~WE & minor~NS | (Street ~> sam)~WE & minor~SN  // OxO far side
          Rules += sam~WE & minor~SN | (Street ~> sam)~WE             // OxO continue
          Rules += sam~WE & minor~SN | (Street ~> sam)~WC             // OxO stub
          // OxD
          Rules += sam~WE | (Street ~> sam)~WC & minor~ES                           // OxD Short-T
          Rules += sam~WE | (Street ~> sam)~WE & minor~ES                           // OxD start
          Rules += sam~WE & minor~ES | (Street ~> sam)~WE & minor~SharedDiagRight   // OxD middle
          Rules += sam~WE & minor~SharedDiagRight | (Street ~> sam)~WE & minor~NW   // OxD end
          Rules += sam~WE & minor~NW | (Street ~> sam)~WE                           // OxD continue
          Rules += sam~WE & minor~NW | (Street ~> sam)~WC                           // OxD continue stub
          // DxO
          Rules += sam~ES | (Street ~> sam)~NW & minor~NS             // DxO start
          Rules += sam~EN & minor~EW | (Street ~> sam)~SW & minor~EW  // DxO middle 1
          Rules += sam~ES & minor~NS | (Street ~> sam)~NW & minor~SN  // DxO middle 2
          Rules += sam~EN & minor~WE | (Street ~> sam)~SW & minor~WE  // DxO end
          Rules += sam~ES & minor~SN | (Street ~> sam)~NW             // DxO continue
          Rules += sam~ES & minor~SN | (Street ~> sam)~(3,0,0,0)      // DxO continue stub
          // DxD
          Rules += sam~ES | (Street ~> sam)~NW & minor~NE                          // DxD start
          Rules += sam~EN & minor~ES | (Street ~> sam)~SW & minor~SharedDiagRight  // DxD middle
          Rules += sam~ES & minor~SharedDiagLeft | (Street ~> sam)~NW & minor~SW   // DxD end
          Rules += sam~ES & minor~SW | (Street ~> sam)~NW                          // DxD continue
          Rules += sam~ES & minor~SW | (Street ~> sam)~CNW                   // DxD continue stub

          // T-intersections
          // OxO
          Rules += sam~WE | (Street ~> sam)~WC & minor~NS             // OxO Short T
          Rules += sam~WE & minor~NS | (Street ~> sam)~WC & minor~SN  // OxO Long T
		  
          if (minor == Avenue) {
            Rules += sam~WE | (Street ~> sam)~WE & minor~NC             // OxO End T Tile 1
            Rules += sam~WE & minor~NC | (Street ~> sam)~WE & minor~CN  // OxO End T Tile 2

            Rules += sam~WE | (Street ~> sam)~(2,2,0,2) & Avenue~CE
            Rules += sam~WE | (Street ~> sam)~(2,0,2,2) & Avenue~NC
            Rules += sam~WE | (Street ~> sam)~(2,0,0,0) & Avenue~(0,2,4,0)
            // continuation in case of Avenue ending at 3 SAMs
            Rules += sam~(2,0,2,2) & Avenue~NC | (Street ~> sam)~WE & Avenue~CN
            Rules += sam~(2,0,2,2) & Avenue~NC | (Street ~> sam)~(2,0,2,2) & Avenue~CN
            Rules += sam~WE & Avenue~CS | (Street ~> sam)~(2,2,2,0) & Avenue~SC

            // SAM continuations
            Rules += sam~WE & minor~CN | (Street ~> sam)~WE
            Rules += sam~(2,0,2,2) & minor~CN | (Street ~> sam)~WE
            Rules += sam~WE & minor~CN | (Street ~> sam)~WC
            Rules += sam~(2,0,2,2) & minor~CN | (Street ~> sam)~WC
          }
        }
		
        for (minor2 <- CrossNetworks if minor2 != sam) {
          // the following block needs work, produces bad rules when minor or minor2 is avenue
          if (hasRightShoulder(minor2)) {
            Rules += sam~WE & minor~SN~NS | (Street ~> sam)~WE & minor2~NS~SN   // OxO | OxO adj
            Rules += sam~WE & minor~WN~NW | (Street ~> sam)~WE & minor2~NS~SN   // OxD | OxO adjacencies
            Rules += sam~WE & minor~NS~SN | (Street ~> sam)~WE & minor2~ES~SE   // OxO | OxD adjacencies
            Rules += sam~ES & minor~NS~SN | (Street ~> sam)~NW & minor2~NS~SN   // DxO | DxO continue
            Rules += sam~ES & minor~NS~SN | (Street ~> sam)~NW & minor2~EN~NE   // DxO | DxD continue
            Rules += sam~ES & minor~SW~WS | (Street ~> sam)~NW & minor2~NS~SN   // DxD | DxO continue
            Rules += sam~ES & minor~SW~WS | (Street ~> sam)~NW & minor2~EN~NE   // DxO | DxO continue
          }
        }

        //for SAM x SAM intersection adjacencies
        Rules += sam~WE & minor~SN~NS | (Street ~> sam)~WE & (Street ~> sam)~NS~SN  // OxO | OxO adj
        Rules += sam~WE & minor~WN~NW | (Street ~> sam)~WE & (Street ~> sam)~NS~SN  // OxD | OxO adjacencies
        Rules += sam~WE & minor~NS~SN | (Street ~> sam)~WE & (Street ~> sam)~ES~SE  // OxO | OxD adjacencies
        Rules += sam~ES & minor~NS~SN | (Street ~> sam)~NW & (Street ~> sam)~NS~SN  // DxO | DxO continue
        Rules += sam~ES & minor~NS~SN | (Street ~> sam)~NW & (Street ~> sam)~EN~NE  // DxO | DxD continue
        Rules += sam~ES & minor~SW~WS | (Street ~> sam)~NW & (Street ~> sam)~NS~SN  // DxD | DxO continue
        Rules += sam~ES & minor~SW~WS | (Street ~> sam)~NW & (Street ~> sam)~EN~NE  // DxO | DxO continue

        Rules += sam~WE & sam~SN~NS | (Street ~> sam)~WE & minor~NS~SN  // OxO | OxO adj
        Rules += sam~WE & sam~WN~NW | (Street ~> sam)~WE & minor~NS~SN  // OxD | OxO adjacencies
        Rules += sam~WE & sam~NS~SN | (Street ~> sam)~WE & minor~ES~SE  // OxO | OxD adjacencies
        Rules += sam~ES & sam~NS~SN | (Street ~> sam)~NW & minor~NS~SN  // DxO | DxO continue
        Rules += sam~ES & sam~NS~SN | (Street ~> sam)~NW & minor~EN~NE  // DxO | DxD continue
        Rules += sam~ES & sam~SW~WS | (Street ~> sam)~NW & minor~NS~SN  // DxD | DxO continue
        Rules += sam~ES & sam~SW~WS | (Street ~> sam)~NW & minor~EN~NE  // DxO | DxO continue

        Rules += sam~WE & sam~SN~NS | (Street ~> sam)~WE & (Street ~> sam)~NS~SN  // OxO | OxO adj
        Rules += sam~WE & sam~WN~NW | (Street ~> sam)~WE & (Street ~> sam)~NS~SN  // OxD | OxO adjacencies
        Rules += sam~WE & sam~NS~SN | (Street ~> sam)~WE & (Street ~> sam)~ES~SE  // OxO | OxD adjacencies
        Rules += sam~ES & sam~NS~SN | (Street ~> sam)~NW & (Street ~> sam)~NS~SN  // DxO | DxO continue
        Rules += sam~ES & sam~NS~SN | (Street ~> sam)~NW & (Street ~> sam)~EN~NE  // DxO | DxD continue
        Rules += sam~ES & sam~SW~WS | (Street ~> sam)~NW & (Street ~> sam)~NS~SN  // DxD | DxO continue
        Rules += sam~ES & sam~SW~WS | (Street ~> sam)~NW & (Street ~> sam)~EN~NE  // DxO | DxO continue
		
        // Transitions
        if (minor == Road || minor == Onewayroad) {
          Rules += sam~WE | (Street ~> sam)~WC & minor~CE   // orth transition
          Rules += sam~SE | (Street ~> sam)~CNW & Road~CWN  // diag transition // (change Road to minor when OWR version is added)
          Rules += sam~WE | (Street ~> sam)~WC & minor~CS   // bending transition
          Rules += sam~WE | (Street ~> sam)~WC & minor~CWN  // bending transition 1
          Rules += sam~WE | (Street ~> sam)~WC & minor~CES  // bending transition 2
        }
        // Avenue-specific Transitions
        Rules += sam~WE | (Street ~> sam)~WC & Avenue~(0,0,2,4)
      }	
    }
    createRules()
  }
}

// Compile individually with `sbt "runMain metarules.module.CompileSamCode"`.
object CompileSamCode extends AbstractMain {
  lazy val resolve: IdResolver = new SamResolver orElse new RealRailwayResolver orElse new MiscResolver orElse new NwmResolver
  val generator = new SamRuleGenerator(_)
  lazy val file = new java.io.File("target/Sec9_SAM_MetaGenerated_MANAGED.txt")
}