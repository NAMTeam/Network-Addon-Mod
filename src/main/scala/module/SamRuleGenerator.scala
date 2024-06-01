package com.sc4nam.module

import io.github.memo33.metarules.meta._
import syntax._, Network._, Flags._, Flag._, RotFlip._, Implicits._, group.SymGroup._
import NetworkProperties._


class SamRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Adjacencies with Stability {

  private def reflections(rule: Rule[SymTile]): Seq[Rule[SymTile]] = {
    Seq(
      Rule(rule(2), rule(1), rule(2), rule(3)),
      Rule(rule(3) * R2F0, rule(0) * R2F0, rule(3) * R2F0, rule(2) * R2F0)
    )
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

    // create continuation rules for the three exit points (curve end, curve end w/ t-int, t-int)
    for (t <- Seq((131,0,2,0), (131,2,2,0), (0,131,2,2))) {
        Rules += sam~t | (Street ~> sam)~WE
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
        Rules += sam~t | (Street ~> sam)~WE
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
    // *
    // *        diag tile
    // *            |
    // *            v
    // *       ,---------,---------,
    // *       |    1    |         |
    // *   0   |        3|3        |
    // *       |         |   152   |
    // *       '---------;---------;---------,
    // *                 |   152   |         |
    // *   1             |      154|154      |
    // *                 |    1    |    1    |
    // *                 ;---------;---------;
    // *                 |    1    |    1    |
    // *   2             |      154|154      |
    // *                 |         |   152   |
    // *                 '---------;---------;---------,
    // *                           |   152   |         |
    // *   3                       |        3|3        | <- diag tile
    // *                           |         |    1    |
    // *                           '---------'---------'
    // *
    // *            0         1         2         3
    // *

    Rules ++= reflections((Street ~> sam)~NE | (Street ~> sam)~(3,0,0,152))
    Rules ++= reflections((Street ~> sam)~(0,0,152,3) | (Street ~> sam)~(152,154,1,0))
    Rules ++= reflections((Street ~> sam)~(0,152,154,1) | (Street ~> sam)~(154,0,0,1))
    Rules ++= reflections((Street ~> sam)~(152,154,1,0) | (Street ~> sam)~(1,154,0,0))
    createRules()
  }


  private def createSamRoundabout(sam: Network): Unit = {
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

    createRules()
  }


  def start(): Unit = {

    val SamNetworks = List(Sam2, Sam3, Sam4, Sam5, Sam6, Sam7, Sam8, Sam9, Sam10, Sam11)

    val CrossNetworks = List(
      Road, Avenue, Onewayroad,
      Rail, L1Dtr, L2Dtr, Lightrail, Monorail, Glr1, Glr2/*, Str*/,
      Dirtroad/*, Rhw3, Mis, Rhw4, Rhw6s, Rhw8sm, Rhw8s, Rhw10s, Rhw12s, Rhw6cm, Rhw6c, Rhw8c,
      L1Rhw2, L1Rhw3, L1Mis, L1Rhw4, L1Rhw6s, L1Rhw8sm, L1Rhw8s, L1Rhw10s, L1Rhw12s, L1Rhw6cm, L1Rhw6c, L1Rhw8c,
      L2Rhw2, L2Rhw3, L2Mis, L2Rhw4, L2Rhw6s, L2Rhw8sm, L2Rhw8s, L2Rhw10s, L2Rhw12s, L2Rhw6cm, L2Rhw6c, L2Rhw8c,
      L3Mis, L3Rhw4, L3Rhw6s, L4Mis, L4Rhw4, L4Rhw6s*/,
      Tla3, Ave2, Ard3, Owr1, Owr3, Nrd4,
      Tla5, Owr4, Owr5, Rd4, Rd6,
      Ave6, Tla7m, Ave6m,
      )

    for (sam <- SamNetworks) {

      // base ortho, diagonal, & stubs
      Rules += sam~WE | (Street ~> sam)~WE   // ortho
      Rules += sam~WE | (Street ~> sam)~CW   // ortho stub
      Rules += sam~SE | (Street ~> sam)~WN   // diagonal
      Rules += sam~SE | (Street ~> sam)~CNW  // diagonal stub

      // sharp 90
      Rules += sam~WE | (Street ~> sam)~(2,2,0,0)           // ortho to sharp 90
      Rules += sam~(0,0,2,2) | (Street ~> sam)~WE           // sharp 90 to ortho

      // sharp diagonal 90 degree curve
      Rules += sam~ES | (Street ~> sam)~WS

      // orth-diag
      Rules += sam~WE | (Street ~> sam)~(2,0,11,0)          // from orth to bottom
      Rules += sam~(2,0,11,0) | (Street ~> sam)~(11,3,0,0)  // bottom to top
      Rules += sam~(0,11,3,0) | (Street ~> sam)~WS          // top to diag
      Rules += sam~ES | (Street ~> sam)~(1,13,0,0)          // from diag to top
      Rules += sam~(0,1,13,0) | (Street ~> sam)~(13,0,2,0)  // top to bottom
      Rules += sam~(13,0,2,0) | (Street ~> sam)~WE          // to orth

      // orth-diag mini diagonal s-curve
      Rules ++= reflections((Street ~> sam)~(0,0,11,3) | (Street ~> sam)~(11,0,11,0))

      // orth-diag sharp cutback
      Rules ++= reflections((Street ~> sam)~WE | (Street ~> sam)~(2,11,0,0))
      Rules ++= reflections((Street ~> sam)~(0,0,11,3) | (Street ~> sam)~(11,0,0,2))

      // diagonal 90 degree curve blend tile
      Rules ++= reflections((Street ~> sam)~(0,0,11,3) | (Street ~> sam)~(11,0,13,0))

      // smoother 3x3 circle support
      Rules ++= reflections((Street ~> sam)~(11,2,13,0) | (Street ~> sam)~(13,0,0,11))
      Rules ++= reflections((Street ~> sam)~(11,0,13,0) | (Street ~> sam)~(13,0,0,11))
      Rules ++= reflections((Street ~> sam)~(2,2,13,0) | (Street ~> sam)~(13,0,0,11))
      Rules ++= reflections((Street ~> sam)~(2,0,13,0) | (Street ~> sam)~(13,0,0,11))

      // complex curves
      createSamLarge45Curve(sam)
      createSam2x290Curve(sam)
      createSamSCurve(sam)
      createSamLarge90Curve(sam)
      createSamDiagSCurve(sam)

      // roundabouts
      createSamRoundabout(sam)

      // standard self-intersections

      // OxO
      Rules += sam~WE | (Street ~> sam)~WE & (Street ~> sam)~NS           // OxO from orth
      Rules += sam~WE & sam~NS | (Street ~> sam)~WE                       // OxO continue

      // OxO T
      Rules += sam~WE | (Street ~> sam)~WE & (Street ~> sam)~NC           // OxO T Thru-Side from orth
      Rules += sam~WE & sam~NC | (Street ~> sam)~WE                       // OxO T Thru-Side continue
      Rules += sam~WE | (Street ~> sam)~WC & (Street ~> sam)~NS           // OxO T End-Side from orth
      Rules += sam~CE & sam~NS | (Street ~> sam)~WE                       // OxO T End-Side continue

      // OxD & DxO
      Rules += sam~WE | (Street ~> sam)~WE & (Street ~> sam)~ES           // OxD from orth
      Rules += sam~WE & sam~NW | (Street ~> sam)~WE                       // OxD orth continue
      Rules += sam~ES | (Street ~> sam)~NW & (Street ~> sam)~NS           // DxO from diag
      Rules += sam~ES & sam~NS | (Street ~> sam)~NW                       // DxO diag continue
      Rules += sam~WE & sam~ES | (Street ~> sam)~WE & (Street ~> sam)~NW  // OxD / DxO across

      // OxD T (orth thru, diag Terminating)
      Rules += sam~(0,0,11,3) | (Street ~> sam)~(11,2,0,2)  // OxD T from orth-diag top
      Rules += sam~WE | (Street ~> sam)~(2,11,2,0)          // OxD T from orth 1
      Rules += sam~WE | (Street ~> sam)~(2,13,2,0)          // OxD T from orth 2
      Rules += sam~(2,11,2,0) | (Street ~> sam)~WE          // OxD T to orth 1
      Rules += sam~(2,13,2,0) | (Street ~> sam)~WE          // OxD T to orth 2
      Rules += sam~(0,2,11,2) | (Street ~> sam)~(11,3,0,0)  // OxD T to orth-diag top

      // DxO T (diag thru, orth Terminating)
      Rules += sam~WE | (Street ~> sam)~WC & (Street ~> sam)~ES           // DxO T from orth
      Rules += sam~ES | (Street ~> sam)~NW & (Street ~> sam)~CS           // DxO T from diag
      Rules += sam~ES | (Street ~> sam)~NW & (Street ~> sam)~CN           // DxO T from diag to aux tile
      Rules += sam~ES & sam~CW | (Street ~> sam)~WN & (Street ~> sam)~CW  // DxO T across
      Rules += sam~ES & sam~CE | (Street ~> sam)~WN & (Street ~> sam)~CE  // DxO T across
      Rules += sam~ES & sam~NC | (Street ~> sam)~WN                       // DxO T to diag
      Rules += sam~ES & sam~SC | (Street ~> sam)~WN                       // DxO T to diag from aux tile
      Rules += sam~SW & sam~EC | (Street ~> sam)~WE                       // DxO T to orth

      // DxD
      Rules += sam~ES | (Street ~> sam)~NW & (Street ~> sam)~EN               // DxD from diag
      Rules += sam~ES & sam~EN | (Street ~> sam)~WS & (Street ~> sam)~WN      // DxD across
      Rules += sam~ES & sam~WS | (Street ~> sam)~WN                           // DxD to diag

      // DxD T
      Rules += sam~NE | (Street ~> sam)~CSW & (Street ~> sam)~ES              // DxD T from end diag
      Rules += sam~ES | (Street ~> sam)~NW & (Street ~> sam)~CNE              // DxD T from thru diag
      Rules += sam~ES | (Street ~> sam)~NW & (Street ~> sam)~CEN              // DxD T from thru diag (aux tile)
      Rules += sam~CEN & sam~ES | (Street ~> sam)~CSW & (Street ~> sam)~NW    // DxD T across 1
      Rules += sam~CNE & sam~ES | (Street ~> sam)~CWS & (Street ~> sam)~NW    // DxD T across 2
      Rules += sam~CNE & sam~NW | (Street ~> sam)~WS                          // DxD T to diag from end side
      Rules += sam~CSW & sam~ES | (Street ~> sam)~NW                          // DxD T to diag from thru side
      Rules += sam~CWS & sam~ES | (Street ~> sam)~NW                          // DxD T to diag from aux tile

      // intersections involving the orth-diag curve

      // from ortho
      val orthDiagIntsFromOrtho = Seq(
        // T ints
        (2,11,2,0), (2,0,2,11),
        (2,2,11,0), (2,11,0,2),
        (2,0,11,2), (2,2,0,11),
        (2,11,11,0),
        (2,13,13,0),
        (2,13,0,11),
        // (2,11,0,13), // does not exist in SAM
        // (2,11,0,11), // does not exist in SAM
        // (2,13,0,13), // does not exist in SAM
        // + Ints
        (2,11,2,2), (2,13,2,2), (2,2,11,2)
      )

      for (t <- orthDiagIntsFromOrtho) {
        Rules ++= reflections((Street ~> sam)~WE | (Street ~> sam)~t)
      }

      // from orth-diag top
      val orthDiagIntsFromDiag = Seq(
        // T ints
        (11,2,0,2),
        (11,0,2,2),
        (11,2,2,0),
        (11,11,0,2), (11,0,2,11),
        (11,2,0,11), (11,11,2,0),
        (11,2,13,0),
        // + ints
        (11,2,2,2)
      )

      for (t <- orthDiagIntsFromDiag) {
        Rules ++= reflections((Street ~> sam)~(0,0,11,3) | (Street ~> sam)~t)
      }

      // special intersections with road and onewayroad
      for (minor <- Seq(Road, Onewayroad)) {
        // intersections
        // dual ortho SAM intersecting diagonal road/owr
        Rules ++= reflections((Street ~> sam)~WE | (Street ~> sam)~(2,2,0,0) & minor~ES)

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

        // transitions
        Rules += sam~WE | (Street ~> sam)~WC & minor~CE   // orth transition
        Rules += sam~SE | (Street ~> sam)~CNW & Road~CWN  // diag transition (TODO: change Road to minor when OWR version is added)
        Rules += sam~WE | (Street ~> sam)~WC & minor~CS   // bending transition
        Rules += sam~WE | (Street ~> sam)~WC & minor~CWN  // bending transition 1
        Rules += sam~WE | (Street ~> sam)~WC & minor~CES  // bending transition 2
      }

      // avenue-specific Transitions
      Rules += sam~WE | (Street ~> sam)~WC & Avenue~(0,0,2,4)

      // special avenue configurations
        // OxO Avenue ending at SAM
        Rules += sam~WE | (Street ~> sam)~WE & Avenue~NC              // OxO End T Tile 1
        Rules += sam~WE & Avenue~NC | (Street ~> sam)~WE & Avenue~CN  // OxO End T Tile 2
        // ???
        Rules += sam~WE | (Street ~> sam)~(2,2,0,2) & Avenue~CE
        Rules += sam~WE | (Street ~> sam)~(2,0,2,2) & Avenue~NC
        Rules += sam~WE | (Street ~> sam)~(2,0,0,0) & Avenue~(0,2,4,0)
        // continuation in case of Avenue ending at 3 SAMs
        Rules += sam~(2,0,2,2) & Avenue~NC | (Street ~> sam)~WE & Avenue~CN
        Rules += sam~(2,0,2,2) & Avenue~NC | (Street ~> sam)~(2,0,2,2) & Avenue~CN
        Rules += sam~WE & Avenue~CS | (Street ~> sam)~(2,2,2,0) & Avenue~SC
        // SAM continuations
        Rules += sam~WE & Avenue~CN | (Street ~> sam)~WE
        Rules += sam~(2,0,2,2) & Avenue~CN | (Street ~> sam)~WE

      // standard intersections with other networks
      for (minor <- CrossNetworks) {

        if (isSingleTile(minor)) {
          // OxO
          Rules += sam~WE | (Street ~> sam)~WE & minor~NS~SN          // OxO from orth
          Rules += sam~WE & minor~NS~SN | (Street ~> sam)~WE          // OxO continue

          // OxD (to do: consider asymmetrical)
          Rules += sam~WE | (Street ~> sam)~WE & minor~ES~SE                // OxD
          Rules += sam~WE & minor~ES~SE | (Street ~> sam)~WE & minor~WN~NW  // OxD Tile 2
          Rules += sam~WE & minor~WN~NW | (Street ~> sam)~WE                // OxD continue

          // DxO & DxD
          if (!minor.isNwm) {
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

          // SAM end T intersections
          if (minor == Road || minor == Onewayroad || minor == Avenue || minor.isNwm) {
            // OxO T (sam end)
            Rules += sam~WE | (Street ~> sam)~WC & minor~NS~SN
          }

          if (minor == Road || minor == Onewayroad) {
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
        }

        if (isDoubleTile(minor)) {
          // OxO
          Rules ++= reflections((Street ~> sam)~WE | (Street ~> sam)~WE & minor~NS)
          Rules += sam~WE & minor~NS | (Street ~> sam)~WE & minor~SN
            // OxO T-intersections
            Rules += sam~WE | (Street ~> sam)~WC & minor~NS             // OxO Short T
            // Rules += sam~WE & minor~NS | (Street ~> sam)~WC & minor~SN  // OxO Long T

          if (minor.typ == AvenueLike && minor == Avenue) { // other possibilities like Owr4 not supported yet
            // OxD
            Rules += sam~WE | (Street ~> sam)~WC & minor~ES                           // OxD Short-T
            Rules += sam~WE | (Street ~> sam)~WE & minor~ES                           // OxD start
            Rules += sam~WE & minor~ES | (Street ~> sam)~WE & minor~SharedDiagRight   // OxD middle
            Rules += sam~WE & minor~SharedDiagRight | (Street ~> sam)~WE & minor~NW   // OxD end
            Rules += sam~WE & minor~NW | (Street ~> sam)~WE                           // OxD continue

            // DxO
            Rules += sam~ES | (Street ~> sam)~NW & minor~NS             // DxO start
            Rules += sam~EN & minor~EW | (Street ~> sam)~SW & minor~EW  // DxO middle 1
            Rules += sam~ES & minor~NS | (Street ~> sam)~NW & minor~SN  // DxO middle 2
            Rules += sam~EN & minor~WE | (Street ~> sam)~SW & minor~WE  // DxO end
            Rules += sam~ES & minor~SN | (Street ~> sam)~NW             // DxO continue

            // DxD
            Rules += sam~ES | (Street ~> sam)~NW & minor~NE                          // DxD start
            Rules += sam~EN & minor~ES | (Street ~> sam)~SW & minor~SharedDiagRight  // DxD middle
            Rules += sam~ES & minor~SharedDiagLeft | (Street ~> sam)~NW & minor~SW   // DxD end
            Rules += sam~ES & minor~SW | (Street ~> sam)~NW                          // DxD continue

          } else { // not shared-tile diagonals (none yet)
            // OxD

            // DxO

            // DxD
          }
        }

        if (isTripleTile(minor)) {
          // OxO
          if (hasRightShoulder(minor)) { // outer tile rule
            Rules ++= reflections((Street ~> sam)~WE | (Street ~> sam)~WE & minor~NS)
            for (median <- Seq(Ave6m, Tla7m)) {
              Rules ++= reflections((Street ~> sam)~WE & minor~NS | (Street ~> sam)~WE & median~NS) // outer to inner / inner to outer
            }
          }
          // OxO short T (SAM end)
          if (hasRightShoulder(minor)) { // no short Ts on median tiles
            Rules ++= reflections((Street ~> sam)~WE | (Street ~> sam)~WC & minor~NS)
          }
        }


      }

      // Thru SAM T-intersections
      val supportsSamThruTs = List(Road, Onewayroad, Tla3, Ave2, Ard3, Nrd4, Owr1)

      for (minor <- supportsSamThruTs) {

        // OxO T (sam thru)
        Rules += sam~WE | (Street ~> sam)~WE & minor~NC~CN
        Rules += sam~WE | (Street ~> sam)~WE & minor~CS~SC
        // continue
        Rules += sam~WE & minor~NC~CN | (Street ~> sam)~WE
        Rules += sam~WE & minor~CS~SC | (Street ~> sam)~WE

        if (! minor.isNwm) {
          // OxD T (sam thru)
          Rules += sam~WE | (Street ~> sam)~WE & minor~CWN
          // continue
          Rules += sam~WE & minor~CWN | (Street ~> sam)~WE

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
      }

    createRules()
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
