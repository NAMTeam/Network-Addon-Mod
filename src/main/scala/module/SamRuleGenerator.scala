package com.sc4nam.module

import io.github.memo33.metarules.meta._
import syntax._, Network._, Flags._, Flag._, RotFlip._, Implicits._, group.SymGroup._
import NetworkProperties._


class SamRuleGenerator(var context: RuleTransducer.Context) extends RuleGenerator with Adjacencies {

  def start(): Unit = {
    /*
    Generate OxO rules by iteration over list of supported crossings
    */
    val SamNetworks = List(Sam2, Sam3, Sam4, Sam5, Sam6, Sam7, Sam8, Sam9, Sam10, Sam11)

    val CrossNetworks = List(Road, Avenue, Onewayroad,
    Rail, Lightrail, Monorail, Glr1, Glr2/*, Str*/, Dirtroad/*, Rhw3, Mis, Rhw4, Rhw6s, Rhw8sm, Rhw8s, Rhw10s, Rhw12s, Rhw6cm,
    Rhw6c, Rhw8c, L1Rhw2, L1Rhw3, L1Mis, L1Rhw4, L1Rhw6s, L1Rhw8sm, L1Rhw8s, L1Rhw10s, L1Rhw12s, L1Rhw6cm,
    L1Rhw6c, L1Rhw8c, L2Rhw2, L2Rhw3, L2Mis, L2Rhw4, L2Rhw6s, L2Rhw8sm, L2Rhw8s, L2Rhw10s, L2Rhw12s, L2Rhw6cm,
    L2Rhw6c, L2Rhw8c, L3Mis, L3Rhw4, L3Rhw6s, L4Mis, L4Rhw4, L4Rhw6s*/, Tla3, Ave2, Ard3, Owr1, Owr3, Nrd4/*, Tla5, Owr4, 
	Owr5, Rd4, Rd6, Ave6, Tla7m, Ave6m*/)


    for (main <- SamNetworks; base <- main.base) {

      Rules += main~WE | (base ~> main)~WE~EW      // ortho
      Rules += main~WE | base~CW | % | main~CW  // ortho stub
      Rules += main~SE~ES | (base ~> main)~WN~NW   // diagonal
      Rules += main~SE~ES | (base ~> main)~CNW       // diagonal stub
	  
	  Rules += main~WE | (base ~> main)~(2,2,0,0)   // ortho to sharp 90
	  Rules += main~(0,0,2,2) | (base ~> main)~WE   //sharp 90 to ortho
	  Rules += main~(0,0,2,2) | (base ~> main)~WC   //sharp 90 to ortho stub
	  Rules += main~(0,0,2,2) | (base ~> main)~(2,0,0,2) //sharp 90 to sharp 90 1
	  Rules += main~(0,0,2,2) | (base ~> main)~(2,2,0,0) //sharp 90 to sharp 90 2
	  Rules += main~(0,0,2,2) | (base ~> main)~(2,0,131,0) //sharp 90 to 2x2 90
	  Rules += main~(0,0,2,2) | (base ~> main)~(2,0,131,2) //sharp 90 to 2x2 90 T
	  Rules += main~(0,0,2,2) | (base ~> main)~(2,0,151,0) //sharp 90 to 3x2 S
	  Rules += main~(0,0,2,2) | (base ~> main)~(2,2,151,0) //sharp 90 to 3x2 S T
      Rules += main~(0,0,2,2) | (base ~> main)~(2,0,11,0)	//sharp 90 to orth-diag bottom
	   
	   
      Rules += main~WE | (base ~> main)~(2,0,11,0)	// orth to orth-diag bottom
      Rules += main~(2,0,11,0) | (base ~> main)~(11,3,0,0) // orth-diag bottom to orth-diag top
      Rules += main~(2,0,11,0) | base~(1,3,0,0) | main~(2,0,11,0) | main~(11,3,0,0) // orth-diag bottom to orth-diag top fix 1
      Rules += main~WE | base~(11,3,0,0) | main~(2,0,11,0) | main~(11,3,0,0) // orth-diag bottom to orth-diag top fix 2
      Rules += main~WE | base~(1,3,0,0) | main~(2,0,11,0) | main~(11,3,0,0) // orth-diag bottom to orth-diag top fix 3
	  Rules += main~(2,0,11,0) | base~(2,13,0,0) | % | main~(11,3,0,0) // orth-diag bottom to orth-diag top fix 4
      Rules += main~(0,0,1,13) | (base ~> main)~NW~WN // orth-diag top to diag
	  // Rules += main~(0,0,1,13) | base~(11,3,0,0) | % | main~(1,3,0,0) // orth-diag top to diag fix
	  Rules += main~(0,11,3,0) | base~(13,0,0,1) | % | main~(3,0,0,1) // orth-diag top to diag fix
	  Rules += main~(0,11,3,0) | main~(13,0,0,1) | % | main~(3,0,0,1) // orth-diag top to diag fix
	  Rules += main~(0,1,3,0) | main~(13,0,0,1) | % | main~(3,0,0,1) // diag top to diag fix
	  Rules += main~(0,0,1,3) | base~(2,13,0,0) | % | main~(1,3,0,0) // diag top to diag fix
	  // Rules += main~(0,0,1,3) | main~(2,13,0,0) | % | main~(3,0,0,1) // diag top to diag fix
      Rules += main~(0,0,1,13) | (base ~> main)~CNW // orth-diag top to diag stub
	  Rules += main~(0,0,1,13) | (base ~> main)~(0,11,3,0) //orth-diag top to orth-diag top
	     
      Rules += main~SE~ES | (base ~> main)~(1,13,0,0) //diag to orth-diag top
      Rules += main~(0,0,11,3) | (base ~> main)~(11,0,2,0) //orth-diag top to orth-diag bottom
      Rules += main~(0,0,1,3) | base~(11,0,2,0) | main~(0,0,11,3) | main~(11,0,2,0) //orth-diag top to orth-diag bottom fix 1
      Rules += main~(0,0,11,3) | base~WE | main~(0,0,11,3) | main~(11,0,2,0) //orth-diag top to orth-diag bottom fix 1
      Rules += main~(0,0,1,3) | base~WE | main~(0,0,11,3) | main~(11,0,2,0) //orth-diag top to orth-diag bottom fix 3
      Rules += main~(11,0,2,0) | (base ~> main)~WE //orth-diag bottom to orth
      Rules += main~(11,0,2,0) | (base ~> main)~WC //orth-diag bottom to orth stub
	  Rules += main~(11,0,2,0) | (base ~> main)~(2,0,11,0) //orth-diag bottom to orth-diag bottom 1	
	  Rules += main~(11,0,2,0) | (base ~> main)~(2,0,13,0) //orth-diag bottom to orth-diag bottom 2	
 	  Rules += main~(13,0,2,0) | (base ~> main)~(2,0,11,0) //orth-diag bottom to orth-diag bottom 3	
	  // Rules += main~WE | (base ~> main)~(2,2,2,2) //alternate attempt at OxO +
	  // Rules += main~(2,2,2,2) | (base ~> main)~WE //alternate attempt at OxO continue
	  // Rules += main~(2,2,2,2) | (base ~> main)~WC //alternate attempt at OxO continue stub
	  
	  //curves
	  //2x2 90
	  Rules += main~WE | (base ~> main)~(2,0,131,0)
	  Rules += main~WE | (base ~> main)~(2,0,131,2) // T-int
	  Rules += main~(0,2,0,131) | (base ~> main)~(143,0,0,141)
	  Rules += main~(2,2,0,131) | (base ~> main)~(143,0,0,141) // T-int
	  Rules += main~(2,0,131,0) | (base ~> main)~(131,133,0,0)
	  Rules += main~(2,0,131,2) | (base ~> main)~(131,133,0,0) // T-int
	  Rules += main~(2,0,131,0) | (base ~> main)~(131,133,0,0) & Street~(0,0,131,133) // Diverter (Half-SAM/Half Street)
	  Rules += main~(2,0,131,2) | (base ~> main)~(131,133,0,0) & Street~(0,0,131,133) // T-int into Diverter (Half-SAM/Half Street)
	  
	  Rules += main~(2,0,131,0) | Street~(131,133,0,0) & main~(0,0,131,133) | main~(2,0,131,0) | main~(131,133,131,133) // Diverter (Full SAM)
	  Rules += main~(2,0,131,2) | Street~(131,133,0,0) & main~(0,0,131,133) | main~(2,0,131,2) | main~(131,133,131,133) // T-int into Diverter (Full SAM)
	  Rules += main~(0,131,133,0) | (base ~> main)~(133,0,2,0)
	  Rules += main~(0,131,133,0) | (base ~> main)~(133,0,2,2) // T-int
	  Rules += main~(0,131,133,0) & Street~(133,0,0,131) | (base ~> main)~(133,0,2,0) // Diverter (Full SAM)
	  Rules += main~(0,131,133,0) & Street~(133,0,0,131) | (base ~> main)~(133,0,2,2) // Diverter into T-int (Full SAM)
	  Rules += main~(133,131,133,131) | (base ~> main)~(133,0,2,0) // Diverter (Full SAM)
	  Rules += main~(133,131,133,131) | (base ~> main)~(133,0,2,2) // Diverter into T-int (Full SAM)
	  //extension out
	  Rules += main~(133,0,2,0) | (base ~> main)~WE
	  Rules += main~(133,0,2,0) | (base ~> main)~WC
	  Rules += main~(133,0,2,0) | (base ~> main)~(2,2,0,0)
	  Rules += main~(133,0,2,0) | (base ~> main)~(2,0,0,2)
	  Rules += main~(133,0,2,0) | (base ~> main)~(2,0,11,0)
	  Rules += main~(133,0,2,0) | (base ~> main)~(2,0,13,0)
	  Rules += main~(133,0,2,0) | (base ~> main)~WE & (base ~> main)~NS
	  Rules += main~(133,0,2,0) | (base ~> main)~WE & (base ~> main)~NC
	  Rules += main~(133,0,2,0) | (base ~> main)~WE & (base ~> main)~CS
	  Rules += main~(133,0,2,0) | (base ~> main)~WC & (base ~> main)~NS
	  Rules += main~(133,0,2,0) | (base ~> main)~(2,0,131,0)
	  Rules += main~(133,0,2,0) | (base ~> main)~(2,0,131,2)
	  Rules += main~(133,0,2,0) | (base ~> main)~(2,0,151,0)
	  Rules += main~(133,0,2,0) | (base ~> main)~(2,2,151,0)
	  
	  //extension out thru end of 2x2 90 Ts
	  Rules += main~(133,0,2,2) | (base ~> main)~WE // T-int
	  Rules += main~(133,0,2,2) | (base ~> main)~WC // T-int
	  Rules += main~(133,0,2,2) | (base ~> main)~(2,2,0,0)
	  Rules += main~(133,0,2,2) | (base ~> main)~(2,0,0,2)
	  Rules += main~(133,0,2,2) | (base ~> main)~(2,0,11,0)
	  Rules += main~(133,0,2,2) | (base ~> main)~(2,0,13,0)
	  Rules += main~(133,0,2,2) | (base ~> main)~WE & (base ~> main)~NS
	  Rules += main~(133,0,2,2) | (base ~> main)~WE & (base ~> main)~NC
	  Rules += main~(133,0,2,2) | (base ~> main)~WE & (base ~> main)~CS
	  Rules += main~(133,0,2,2) | (base ~> main)~WC & (base ~> main)~NS
	  Rules += main~(133,0,2,2) | (base ~> main)~(2,0,131,0)
	  Rules += main~(133,0,2,2) | (base ~> main)~(2,0,131,2)
	  Rules += main~(133,0,2,2) | (base ~> main)~(2,0,151,0)
	  Rules += main~(133,0,2,2) | (base ~> main)~(2,2,151,0)

	  //extension out end of 2x2 90 Ts
	  Rules += main~(0,131,2,2)	| (base ~> main)~WE 
	  Rules += main~(0,131,2,2)	| (base ~> main)~WC
	  Rules += main~(0,131,2,2)	| (base ~> main)~WE & (base ~> main)~NS
	  Rules += main~(0,131,2,2)	| (base ~> main)~WE & (base ~> main)~NC
	  Rules += main~(0,131,2,2)	| (base ~> main)~WE & (base ~> main)~SC
	  Rules += main~(0,131,2,2)	| (base ~> main)~WC & (base ~> main)~NS
	  Rules += main~(0,131,2,2)	| (base ~> main)~(2,2,0,0)
	  Rules += main~(0,131,2,2)	| (base ~> main)~(2,0,0,2)
	  Rules += main~(0,131,2,2)	| (base ~> main)~(2,0,11,0)
	  Rules += main~(0,131,2,2)	| (base ~> main)~(2,0,13,0)
	  Rules += main~(0,131,2,2)	| (base ~> main)~(2,0,131,0)
	  Rules += main~(0,131,2,2)	| (base ~> main)~(2,0,131,2)  
	  Rules += main~(0,131,2,2)	| (base ~> main)~(2,0,151,0)
	  Rules += main~(0,131,2,2)	| (base ~> main)~(2,2,151,0)
	  
	  //3x2 S
	  Rules += main~WE | (base ~> main)~(2,0,151,0)
	  Rules += main~WE | (base ~> main)~(2,2,151,0)	// T-int
	  Rules += main~(2,0,151,0) | (base ~> main)~(151,0,0,161)	  
	  Rules += main~(2,2,151,0) | (base ~> main)~(151,0,0,161)	// T-int
	  Rules += main~(151,0,0,161) | (base ~> main)~(171,0,0,181)	
	  Rules += main~(0,0,161,151) | (base ~> main)~(161,151,0,0)
	  Rules += main~(0,0,181,171) | (base ~> main)~(181,171,0,0)
	  Rules += main~(0,161,151,0) | (base ~> main)~(151,0,2,0)
	  Rules += main~(0,161,151,0) | (base ~> main)~(151,0,2,2) // T-int
	  
	  //extension out
	  Rules += main~(151,0,2,0) | (base ~> main)~WE
	  Rules += main~(151,0,2,0) | (base ~> main)~WC
	  Rules += main~(151,0,2,0) | (base ~> main)~(2,2,0,0)
	  Rules += main~(151,0,2,0) | (base ~> main)~(2,0,0,2)
	  Rules += main~(151,0,2,0) | (base ~> main)~(2,0,11,0)
	  Rules += main~(151,0,2,0) | (base ~> main)~(2,0,13,0)
	  Rules += main~(151,0,2,0) | (base ~> main)~WE & (base ~> main)~NS
	  Rules += main~(151,0,2,0) | (base ~> main)~WE & (base ~> main)~NC
	  Rules += main~(151,0,2,0) | (base ~> main)~WE & (base ~> main)~CS
	  Rules += main~(151,0,2,0) | (base ~> main)~WC & (base ~> main)~NS
	  Rules += main~(151,0,2,0) | (base ~> main)~(2,0,131,0)
	  Rules += main~(151,0,2,0) | (base ~> main)~(2,0,131,2)
	  Rules += main~(151,0,2,0) | (base ~> main)~(2,0,151,0)
	  Rules += main~(151,0,2,0) | (base ~> main)~(2,2,151,0)
	  
	  //extension out thru end of 2x2 90 Ts
	  Rules += main~(151,0,2,2) | (base ~> main)~WE // T-int
	  Rules += main~(151,0,2,2) | (base ~> main)~WC // T-int
	  Rules += main~(151,0,2,2) | (base ~> main)~(2,2,0,0)
	  Rules += main~(151,0,2,2) | (base ~> main)~(2,0,0,2)
	  Rules += main~(151,0,2,2) | (base ~> main)~(2,0,11,0)
	  Rules += main~(151,0,2,2) | (base ~> main)~(2,0,13,0)
	  Rules += main~(151,0,2,2) | (base ~> main)~WE & (base ~> main)~NS
	  Rules += main~(151,0,2,2) | (base ~> main)~WE & (base ~> main)~NC
	  Rules += main~(151,0,2,2) | (base ~> main)~WE & (base ~> main)~CS
	  Rules += main~(151,0,2,2) | (base ~> main)~WC & (base ~> main)~NS
	  Rules += main~(151,0,2,2) | (base ~> main)~(2,0,131,0)
	  Rules += main~(151,0,2,2) | (base ~> main)~(2,0,131,2)
	  Rules += main~(151,0,2,2) | (base ~> main)~(2,0,151,0)
	  Rules += main~(151,0,2,2) | (base ~> main)~(2,2,151,0)

	  //extension out end of 3x2 S Ts
	  Rules += main~(0,2,2,151)	| (base ~> main)~WE
	  Rules += main~(0,2,2,151)	| (base ~> main)~WC
	  Rules += main~(0,2,2,151) | (base ~> main)~(2,2,0,0)
	  Rules += main~(0,2,2,151) | (base ~> main)~(2,0,0,2)
	  Rules += main~(0,2,2,151) | (base ~> main)~(2,0,11,0)
	  Rules += main~(0,2,2,151) | (base ~> main)~(2,0,13,0)
	  Rules += main~(0,2,2,151) | (base ~> main)~WE & (base ~> main)~NS
	  Rules += main~(0,2,2,151) | (base ~> main)~WE & (base ~> main)~NC
	  Rules += main~(0,2,2,151) | (base ~> main)~WE & (base ~> main)~CS
	  Rules += main~(0,2,2,151) | (base ~> main)~WC & (base ~> main)~NS
	  Rules += main~(0,2,2,151) | (base ~> main)~(2,0,131,0)
	  Rules += main~(0,2,2,151) | (base ~> main)~(2,0,131,2)
	  Rules += main~(0,2,2,151) | (base ~> main)~(2,0,151,0)
	  Rules += main~(0,2,2,151) | (base ~> main)~(2,2,151,0)
	  
	  //large 45 curve (4x3)
	  //Orthogonal to Diagonal
	  Rules += main~WE | (base ~> main)~(2,0,111,0)
	  Rules += main~(2,0,111,0) | (base ~> main)~(111,0,11,0)
		//stability of above
	  Rules += main~(2,0,2,0) | base~(111,0,11,0) | main~(2,0,111,0) | main~(111,0,11,0)
	  Rules += main~(2,0,2,0) | main~(111,0,11,0) | main~(2,0,111,0) | main~(111,0,11,0)
		
	  Rules += main~(111,0,11,0) | (base ~> main)~(11,113,0,0)
		//stability of above
		Rules += main~(111,0,11,0) | base~(11,3,0,0) | % | main~(11,113,0,0)
		Rules += main~(111,0,11,0) | main~(11,3,0,0) | % | main~(11,113,0,0)
		Rules += main~(111,0,11,0) | base~(2,13,0,0) | % | main~(11,113,0,0)
	  Rules += main~(0,111,0,11) | (base ~> main)~(14,0,0,14)
		//stability of above
		Rules += main~(0,2,0,11) | base~(0,0,0,0) | main~(0,111,0,11) | main~(14,0,0,14)
		Rules += main~(0,111,0,11) | base~(0,0,0,0) | % | main~(14,0,0,14)
		Rules += main~(0,2,0,11) | base~(14,0,0,14) | main~(0,111,0,11) | main~(14,0,0,14)
		Rules += main~(0,2,0,11) | main~(14,0,0,14) | main~(0,111,0,11) | main~(14,0,0,14)
	  Rules += main~(0,11,113,0) | (base ~> main)~(113,0,0,1)
	  Rules += main~(0,11,113,0) | base~(13,0,0,1) | % | main~(113,0,0,1) //stability of line above
	  Rules += main~(0,11,113,0) | base~(3,0,0,1) | % | main~(113,0,0,1) //stability of line above
	  Rules += main~(0,11,113,0) | main~(13,0,0,1) | % | main~(113,0,0,1) //stability of line above
	  Rules += main~(0,11,113,0) | main~(3,0,0,1) | % | main~(113,0,0,1) //stability of line above
	  Rules += main~(0,0,1,113) | (base ~> main)~(1,3,0,0)
  	  Rules += main~(0,0,1,113) | (base ~> main)~(1,3,0,0)
	  Rules += main~(0,0,1,113) | IdTile(0x5F591F00,0,0,noSymmetries) | % | main~(1,3,0,0)
	  //Diagonal to Orthogonal
	  //(add)
	  
	  //large 90 curve
	  Rules += main~WE | (base ~> main)~(2,0,181,0)
	  Rules += main~WE | (base ~> main)~(2,0,181,2) //T-int
	  
	  Rules += main~(2,0,181,0) | (base ~> main)~(181,11,191,0)
	  Rules += main~(2,0,181,2) | (base ~> main)~(181,11,191,0) //from T-int
	  Rules += main~(2,0,181,0) | (base ~> main)~(181,11,191,2) //into T-int
	  Rules += main~(2,0,181,2) | (base ~> main)~(181,11,191,2) //from T-int into T-int

	  Rules += main~(181,11,191,0) | (base ~> main)~(191,193,0,0)
	  Rules += main~(181,11,191,2) | (base ~> main)~(191,193,0,0) //from T-int
	  
	  Rules += main~(0,181,11,191) | (base ~> main)~(11,0,0,82)
	  Rules += main~(2,181,11,191) | (base ~> main)~(11,0,0,82) // from T-int
	  
	  Rules += main~(0,0,82,11) | (base ~> main)~(82,82,0,0)
	  
	  Rules += main~(0,82,82,0) | (base ~> main)~(82,0,0,13)
	  Rules += main~(0,0,13,82) | (base ~> main)~(13,183,0,193)  
	  Rules += main~(0,0,13,82) | (base ~> main)~(13,183,2,193) // T-int
	  // needs work below
	  Rules += main~(193,13,183,0) | (base ~> main)~(193,0,0,191)
 	  Rules += main~(193,13,183,2) | (base ~> main)~(193,0,0,191) // T-int
 
 	  Rules += main~(193,13,183,0) | (base ~> main)~(183,0,2,0) //
  	  Rules += main~(193,13,183,0) | (base ~> main)~(183,0,2,2) // T-int
 	  Rules += main~(193,13,183,2) | (base ~> main)~(183,0,2,0) //
 	  Rules += main~(193,13,183,2) | (base ~> main)~(183,0,2,2) // T-int
	  
	  //continuations
	  Rules += main~(183,0,2,0) | (base ~> main)~WE
	  Rules += main~(183,0,2,2) | (base ~> main)~WE
	  Rules += main~(183,0,2,0) | (base ~> main)~WC
	  Rules += main~(183,0,2,2) | (base ~> main)~WC
	  
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,2,0,0)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,0,11,0)
	  Rules += main~(183,0,2,0) | (base ~> main)~WE & (base ~> main)~NS
	  Rules += main~(183,0,2,0) | (base ~> main)~WC & (base ~> main)~NS
	  Rules += main~(183,0,2,0) | (base ~> main)~WE & (base ~> main)~NC
	  Rules += main~(183,0,2,0) | (base ~> main)~WE & (base ~> main)~SE
	  Rules += main~(183,0,2,0) | (base ~> main)~WC & (base ~> main)~SE
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,11,2,2)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,11,2,0)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,2,11,0)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,2,13,0)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,11,11,0)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,13,13,0)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,2,0,11)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,2,0,13)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,13,0,11)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,102,102,0)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,0,102,102)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,0,111,0)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,0,131,0)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,0,151,0)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,0,181,0)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,0,131,2)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,2,151,0)
	  Rules += main~(183,0,2,0) | (base ~> main)~(2,0,181,2)
	  
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,2,0,0)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,0,11,0)
	  Rules += main~(183,0,2,2) | (base ~> main)~WE & (base ~> main)~NS
	  Rules += main~(183,0,2,2) | (base ~> main)~WC & (base ~> main)~NS
	  Rules += main~(183,0,2,2) | (base ~> main)~WE & (base ~> main)~NC
	  Rules += main~(183,0,2,2) | (base ~> main)~WE & (base ~> main)~SE
	  Rules += main~(183,0,2,2) | (base ~> main)~WC & (base ~> main)~SE
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,11,2,2)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,11,2,0)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,2,11,0)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,2,13,0)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,11,11,0)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,13,13,0)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,2,0,11)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,2,0,13)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,13,0,11)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,102,102,0)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,0,102,102)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,0,111,0)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,0,131,0)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,0,151,0)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,0,181,0)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,0,131,2)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,2,151,0)
	  Rules += main~(183,0,2,2) | (base ~> main)~(2,0,181,2)
	  
	  //off T-ints
	  Rules += main~(0,181,2,2) | (base ~> main)~WE
	  Rules += main~(0,181,2,2) | (base ~> main)~WC

	  Rules += main~(0,181,2,2) | (base ~> main)~(2,2,0,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,0,2)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,11,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,13,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~WE & (base ~> main)~NS
	  Rules += main~(0,181,2,2) | (base ~> main)~WC & (base ~> main)~NS
	  Rules += main~(0,181,2,2) | (base ~> main)~WE & (base ~> main)~NC
	  Rules += main~(0,181,2,2) | (base ~> main)~WE & (base ~> main)~CS
	  Rules += main~(0,181,2,2) | (base ~> main)~WE & (base ~> main)~SE
	  Rules += main~(0,181,2,2) | (base ~> main)~WC & (base ~> main)~SE
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,11,2,2)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,2,2,11)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,2,11,2)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,2,13,2)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,2,11)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,11,2,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,2,11,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,2,13,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,11,11,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,13,13,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,2,0,11)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,2,0,13)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,13,0,11)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,102,102,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,102,102)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,111,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,131,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,151,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,181,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,113,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,133,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,153,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,183,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,131,2)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,2,151,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,181,2)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,2,133,0)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,0,153,2)
	  Rules += main~(0,181,2,2) | (base ~> main)~(2,2,183,0)  
	  
 	  Rules += main~(11,191,2,181) | (base ~> main)~WE
	  Rules += main~(11,191,2,181) | (base ~> main)~WC
 
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,2,0,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,0,0,2)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,0,11,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~WE & (base ~> main)~NS
	  Rules += main~(11,191,2,181) | (base ~> main)~WC & (base ~> main)~NS
	  Rules += main~(11,191,2,181) | (base ~> main)~WE & (base ~> main)~NC
	  Rules += main~(11,191,2,181) | (base ~> main)~WE & (base ~> main)~CS
	  Rules += main~(11,191,2,181) | (base ~> main)~WE & (base ~> main)~SE
	  Rules += main~(11,191,2,181) | (base ~> main)~WC & (base ~> main)~SE
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,11,2,2)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,2,11,2)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,0,2,11)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,2,11,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,2,13,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,11,11,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,13,13,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,2,0,11)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,2,0,13)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,13,0,11)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,102,102,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,0,102,102)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,0,111,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,0,131,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,0,151,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,0,181,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,0,113,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,0,133,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,0,153,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,0,183,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,0,131,2)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,2,151,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,0,181,2)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,2,133,0)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,0,153,2)
	  Rules += main~(11,191,2,181) | (base ~> main)~(2,2,183,0)
	  
	  //main & main intersections
	  Rules += main~WE | (base ~> main)~WE & (base ~> main)~NS  		// OxO from orth
	  Rules += main~(0,0,2,2) | (base ~> main)~WE & (base ~> main)~NS  	// OxO from 90-bend
	  Rules += main~(11,0,2,0) | (base ~> main)~WE & (base ~> main)~NS  // OxO from orth-diag
      Rules += main~WE & main~NS | (base ~> main)~WE          		// OxO continue
      Rules += main~WE & main~NS | (base ~> main)~WC          		// OxO continue stub
      Rules += main~WE & main~NS | (base ~> main)~(2,2,0,0)         // OxO continue 90-bend
	  Rules += main~WE & main~NS | (base ~> main)~(2,0,11,0)        // OxO continue orth-diag
	  Rules += main~WE & main~NS | (base ~> main)~WE & (base ~> main)~NS        // OxO continue +
	  Rules += main~WE & main~NS | (base ~> main)~WE & (base ~> main)~NC        // OxO continue T-Thru Side
	  Rules += main~WE & main~NS | (base ~> main)~WC & (base ~> main)~NS        // OxO continue T-End Side
	  Rules += main~WE & main~NS | (base ~> main)~WE & (base ~> main)~ES 		// OxO to OxD	  
		  
      Rules += main~WE | (base ~> main)~WE & (base ~> main)~NC		// OxO T Thru-Side from orth
      Rules += main~(0,0,2,2) | (base ~> main)~WE & (base ~> main)~NC		// OxO T Thru-Side from 90-bend
      Rules += main~(11,0,2,0) | (base ~> main)~WE & (base ~> main)~NC		// OxO T Thru-Side from orth-diag
      Rules += main~WE & main~NC | (base ~> main)~WE          		// OxO T Thru-Side continue orth 1
      Rules += main~WE & main~CS | (base ~> main)~WE          		// OxO T Thru-Side continue orth 2
      Rules += main~WE & main~NC | (base ~> main)~WC          		// OxO T Thru-Side continue stub orth stub 1
      Rules += main~WE & main~CS | (base ~> main)~WC          		// OxO T Thru-Side continue stub orth stub 2
      Rules += main~WE & main~NC | (base ~> main)~(2,2,0,0)          		// OxO T Thru-Side continue sharp-90 1
      Rules += main~WE & main~CS | (base ~> main)~(2,2,0,0)          		// OxO T Thru-Side continue sharp-90 2
      Rules += main~WE & main~NC | (base ~> main)~(2,0,11,0)          		// OxO T Thru-Side continue orth-diag 1
      Rules += main~WE & main~CS | (base ~> main)~(2,0,11,0)          		// OxO T Thru-Side continue orth-diag 2
      Rules += main~WE & main~NC | (base ~> main)~WE & (base ~> main)~NS          		// OxO T Thru-Side + 1
      Rules += main~WE & main~CS | (base ~> main)~WE & (base ~> main)~NS          		// OxO T Thru-Side + 2
      Rules += main~WE & main~NC | (base ~> main)~WE & (base ~> main)~NC          		// OxO T Thru-Side to Thru-Side 1a
      Rules += main~WE & main~NC | (base ~> main)~WE & (base ~> main)~CS          		// OxO T Thru-Side to Thru-Side 1b
      Rules += main~WE & main~CS | (base ~> main)~WE & (base ~> main)~NC          		// OxO T Thru-Side to Thru-Side 2a
      Rules += main~WE & main~CS | (base ~> main)~WE & (base ~> main)~CS          		// OxO T Thru-Side to Thru-Side 2b
      Rules += main~WE & main~NC | (base ~> main)~WC & (base ~> main)~NS          		// OxO T Thru-Side to End-Side 1
      Rules += main~WE & main~CS | (base ~> main)~WC & (base ~> main)~NS          		// OxO T Thru-Side to End-Side 2	

      Rules += main~WE | (base ~> main)~WC & (base ~> main)~NS				// OxO T End-Side from orth
      Rules += main~(0,0,2,2) | (base ~> main)~WC & (base ~> main)~NS		// OxO T End-Side from 90-bend
      Rules += main~(11,0,2,0) | (base ~> main)~WC & (base ~> main)~NS		// OxO T End-Side from orth-diag
      Rules += main~CE & main~NS | (base ~> main)~WE          		// OxO T End-Side continue orth 1
      Rules += main~CE & main~NS | (base ~> main)~WC          		// OxO T End-Side continue stub orth stub 1
      Rules += main~CE & main~NS | (base ~> main)~(2,2,0,0)          		// OxO T End-Side continue sharp-90
      Rules += main~CE & main~NS | (base ~> main)~(2,0,11,0)          		// OxO T End-Side continue orth-diag
      Rules += main~CE & main~NS | (base ~> main)~WE & (base ~> main)~NS          		// OxO T End-Side + 1
      Rules += main~CE & main~NS | (base ~> main)~WE & (base ~> main)~NC          		// OxO T End-Side to Thru-Side 1a
      Rules += main~CE & main~NS | (base ~> main)~WE & (base ~> main)~CS          		// OxO T End-Side to Thru-Side 2a
      Rules += main~CE & main~NS | (base ~> main)~WC & (base ~> main)~NS          		// OxO T End-Side to End-Side 1
	  
	  Rules += main~WE | (base ~> main)~WE & (base ~> main)~ES 		// OxD from orth
	  Rules += main~WE | (base ~> main)~WC & (base ~> main)~ES 		// OxD T from orth

	  Rules += main~WE & main~ES | (base ~> main)~WE & (base ~> main)~NW	    // OxD Tile 2
	  Rules += main~(0,0,2,2) | (base ~> main)~WE & (base ~> main)~ES 		// OxD from 90-bend
	  Rules += main~(11,0,2,0) | (base ~> main)~WE & (base ~> main)~ES 		// OxD from orth-diag
      Rules += main~WE & main~NW | (base ~> main)~WE          	// OxD orth continue
      Rules += main~WE & main~NW | (base ~> main)~WC          	// OxD orth continue stub
      Rules += main~WE & main~NW | (base ~> main)~(2,2,0,0)      // OxD orth continue 90-bend
      Rules += main~WE & main~NW | (base ~> main)~(2,0,11,0)     // OxD orth continue orth-diag
	  Rules += main~WE & main~NW | (base ~> main)~WE & (base ~> main)~NS          	// OxD to OxO + continue
      Rules += main~WE & main~NW | (base ~> main)~WE & (base ~> main)~NC		// OxD continue to OxO T Thru-Side from orth
      Rules += main~WE & main~NW | (base ~> main)~WC & (base ~> main)~NS		// OxD continue to OxO T End-Side from orth
	  Rules += main~WE & main~NW | (base ~> main)~WE & (base ~> main)~ES 		// OxD to OxD + continue
	  	  
	  Rules += main~WE & main~ES | base~WN | main~WC & main~ES | main~WN & main~WC				// OxD Tile 2 T
	  Rules += main~WE & main~ES | main~WN | main~WC & main~ES | main~WN & main~WC				// OxD Tile 2 T
	  Rules += main~WE & main~ES | base~WN & base~WC | main~WC & main~ES | main~WN & main~WC	// OxD Tile 2 T
	  Rules += main~WC & main~ES | (base ~> main)~WN & (base ~> main)~WC						// OxD Tile 2 T
	  Rules += main~WC & main~ES | base~WN | % | main~WN & main~WC				// OxD Tile 2 T
	  Rules += main~WC & main~ES | main~WN | % | main~WN & main~WC				// OxD Tile 2 T
	  Rules += main~NE | base~SW & base~WE | main~NE & main~EC | main~SW & main~EC // OxD T from Tile 2 to Tile 1
	  Rules += main~NE | base~SW & base~EC | main~NE & main~EC | main~SW & main~EC // OxD T from Tile 2 to Tile 1
	  Rules += main~NE & main~EC | base~SW & base~WE | main~NE & main~EC | main~SW & main~EC // OxD T from Tile 2 to Tile 1
	  Rules += main~NE & main~EC | (base ~> main)~SW & (base ~> main)~EC // OxD T from Tile 2 to Tile 1
	  Rules += main~SW & main~EC | (base ~> main)~WE          	// OxD T orth continue
      Rules += main~SW & main~EC | (base ~> main)~WC          	// OxD T orth continue stub
      Rules += main~SW & main~EC | (base ~> main)~(2,2,0,0)      // OxD T orth continue 90-bend
      Rules += main~SW & main~EC | (base ~> main)~(2,0,11,0)     // OxD T orth continue orth-diag

	  
	  Rules += main~(0,0,11,3) | base~CNW & base~NS | main~(0,0,11,3) | main~CNW & main~NS   // DxO T from Orth-Diag Top
	  Rules += main~ES | base~WC & base~NS | main~(0,0,11,3) | main~CNW & main~NS   // DxO T from Diag for Old-Style Diag Streets
	  Rules += main~(0,0,11,3) | base~WC & base~NS | main~(0,0,11,3) | main~CNW & main~NS   // DxO T from Orth-Diag Top for Old-Style Diag Streets

	  
	  Rules += main~NE & main~NC | (base ~> main)~SW
	  Rules += main~NE | (base ~> main)~SW & (base ~> main)~CS

	  
	  Rules += main~ES | (base ~> main)~NW & (base ~> main)~NS      // DxO
	  Rules += main~ES | (base ~> main)~NW & (base ~> main)~CS      // DxO T
	  Rules += main~ES & main~NS | (base ~> main)~NW       			// DxO diag continue
	  Rules += main~ES & main~NS | (base ~> main)~CNW       		// DxO diag stub continue
	  Rules += main~ES & main~NC | (base ~> main)~NW       			// DxO T diag continue
	  Rules += main~ES & main~NC | (base ~> main)~CNW       		// DxO T diag stub continue
		  
      Rules += main~ES | (base ~> main)~NW & (base ~> main)~EN      // DxD
      Rules += main~ES | (base ~> main)~CNW & (base ~> main)~EN      // DxD T (End)
      Rules += main~ES | (base ~> main)~NW & (base ~> main)~CNE      // DxD T (Thru 1)
      Rules += main~NE | (base ~> main)~(3,0,0,1) & (base ~> main)~(1,0,0,0)      // DxD T (Thru 2)
	  
	  //Orth-Diag into Diag Fixes
      Rules += main~(0,0,11,3) | base~NW & base~EN | main~ES | main~NW & main~EN      // DxD
      Rules += main~(0,0,11,3) | base~CNW & base~EN | main~ES | main~CNW & main~EN     // DxD T (End)
      Rules += main~(0,0,11,3) | base~NW & base~CNE | main~ES | main~NW & main~CNE    // DxD T (Thru 1)
      Rules += main~(0,1,13,0) | base~(3,0,0,1) & base~(1,0,0,0) | main~NE | main~(3,0,0,1) & main~(1,0,0,0)     // DxD T (Thru 2)
      Rules += main~(0,0,11,3) | main~NW & main~EN | main~ES | main~NW & main~EN      // DxD
      Rules += main~(0,0,11,3) | main~CNW & main~EN | main~ES | main~CNW & main~EN     // DxD T (End)
      Rules += main~(0,0,11,3) | main~NW & main~CNE | main~ES | main~NW & main~CNE    // DxD T (Thru 1)
      Rules += main~(0,1,13,0) | main~(3,0,0,1) & main~(1,0,0,0) | main~NE | main~(3,0,0,1) & main~(1,0,0,0)     // DxD T (Thru 2)
	  
	  Rules += main~SE & main~EN | (base ~> main)~NW & (base ~> main)~WS      // DxD Tile 2
	  Rules += main~SE & main~EN | main~(2,13,0,11) | % |  main~NW & main~WS     // DxD Tile 2 Fix 1
	  Rules += main~(0,11,2,13) | main~(2,13,0,11) | main~SE & main~EN |  main~NW & main~WS     // DxD Tile 2 Fix 2
	  Rules += main~SE & main~EN | base~WN | main~SE & main~CEN | main~(1,3,0,0) & main~(0,1,0,0)	// DxD Tile 2 T
	  Rules += main~SE & main~EN | main~WN | main~SE & main~CEN | main~(1,3,0,0) & main~(0,1,0,0)	// DxD Tile 2 T
	  Rules += main~SE & main~CEN | base~WN | main~SE & main~CEN | main~(1,3,0,0) & main~(0,1,0,0)	// DxD Tile 2 T
	  Rules += main~SE & main~CEN | main~WN | main~SE & main~CEN | main~(1,3,0,0) & main~(0,1,0,0)	// DxD Tile 2 T
	  Rules += main~SE & main~EN | base~(1,3,0,0) & base~(0,1,0,0) | main~SE & main~CEN | main~(1,3,0,0) & main~(0,1,0,0)	// DxD Tile 2 T
	  Rules += main~SE & main~CEN | base~(1,3,0,0) & base~(0,1,0,0) | main~SE & main~CEN | main~(1,3,0,0) & main~(0,1,0,0)	// DxD Tile 2 T
	  Rules += main~(0,0,1,3) & main~(0,0,0,1) | (base ~> main)~NW & (base ~> main)~CWS // DxD Tile 1 T from Tile 2
	  
      Rules += main~SE & main~WS | (base ~> main)~NW       			// DxD continue
      Rules += main~SE & main~WS | base~(11,3,0,0) | % | main~NW    // DxD continue fix
      Rules += main~SE & main~WS | (base ~> main)~CNW       		// DxD continue stub
      Rules += main~CSE & main~WS | (base ~> main)~NW       			// DxD T (End Side) continue
      Rules += main~CSE & main~WS | (base ~> main)~CNW       			// DxD T (End Side) continue stub
      Rules += main~CNW & main~NE | (base ~> main)~WS       			// DxD T (Thru Side 1) continue
      Rules += main~CNW & main~NE | (base ~> main)~CWS       			// DxD T (Thru Side 1) continue stub
      Rules += main~(0,1,3,0) & main~(0,0,1,0) | (base ~> main)~WS       			// DxD T (Thru Side 2) continue
      Rules += main~(0,1,3,0) & main~(0,0,1,0) | (base ~> main)~CWS       			// DxD T (Thru Side 2) continue stub
	  


      /* Rules += main~SE & main~EN | base~NW | % | main~NW & main~CWS	// DxD Tile 2 T
	  Rules += main~SE & main~EN | main~NW | % | main~NW & main~CWS	// DxD Tile 2 T
	  Rules += main~SE & main~EN | (base ~> main)~NW & (base ~> main)~CWS		// DxD Tile 2 T
	  Rules += main~SE & main~CEN | (base ~> main)~NW & (base ~> main)~CWS		// DxD Tile 2 T
      Rules += main~SE & main~CEN | base~NW | % | main~NW & main~CWS	// DxD Tile 2 T
      Rules += main~SE & main~CEN | main~NW | % | main~NW & main~CWS	// DxD Tile 2 T
	  Rules += main~NE & main~CNW | (base ~> main)~SW
	  Rules += main~NE | (base ~> main)~SW & (base ~> main)~CSE */
	  
	  
	  // non-standard self-intersections
	  //T-ints
      //from ortho
      Rules += main~WE | (base ~> main)~(2,11,2,0)
      Rules += main~WE | (base ~> main)~(2,13,2,0)
	  
	  Rules += main~WE | (base ~> main)~(2,2,11,0)
	  Rules += main~WE | (base ~> main)~(2,2,13,0)
	  
	  Rules += main~WE | (base ~> main)~(2,11,0,2)	  
	  Rules += main~WE | (base ~> main)~(2,13,0,2)

      Rules += main~WE | (base ~> main)~(2,11,11,0)
      Rules += main~WE | (base ~> main)~(2,13,13,0)	  

	  //Rules += main~WE | (base ~> main)~(2,11,0,13)
	  Rules += main~WE | (base ~> main)~(2,13,0,11)	 	  
	  //from orth-diag top
	  Rules += main~(0,0,11,3) | (base ~> main)~(11,2,0,2)
	  Rules += main~(0,0,11,3) | (base ~> main)~(11,0,2,2)
	  Rules += main~(0,0,11,3) | (base ~> main)~(11,2,2,0)
	  Rules += main~(0,0,1,3) | base~(11,2,0,2) | main~(0,0,11,3) | %
	  Rules += main~(0,0,1,3) | base~(11,0,2,2) | main~(0,0,11,3) | %
	  Rules += main~(0,0,1,3) | base~(11,2,2,0) | main~(0,0,11,3) | %
	  Rules += main~(0,0,1,3) | main~(11,2,0,2) | main~(0,0,11,3) | %
	  Rules += main~(0,0,1,3) | main~(11,0,2,2) | main~(0,0,11,3) | %
	  Rules += main~(0,0,1,3) | main~(11,2,2,0) | main~(0,0,11,3) | %
	  
	  Rules += main~(0,1,13,0) | (base ~> main)~(13,2,0,2)
	  Rules += main~(0,1,13,0) | (base ~> main)~(13,0,2,2)
	  Rules += main~(0,1,13,0) | (base ~> main)~(13,2,2,0)
	  Rules += main~(0,1,13,0) | base~(13,2,0,2) | main~(0,1,13,0) | %
	  Rules += main~(0,1,13,0) | base~(13,0,2,2) | main~(0,1,13,0) | %
	  Rules += main~(0,1,13,0) | base~(13,2,2,0) | main~(0,1,13,0) | %

	  Rules += main~(0,0,11,3) | (base ~> main)~(11,2,0,11)
	  Rules += main~(0,0,11,3) | (base ~> main)~(11,0,2,11)
	  Rules += main~(0,0,11,3) | (base ~> main)~(11,11,2,0)
	  Rules += main~(0,0,1,3) | base~(11,2,0,11) | main~(0,0,11,3) | main~(11,2,0,11)
	  Rules += main~(0,0,1,3) | base~(11,0,2,11) | main~(0,0,11,3) | main~(11,2,0,11)
	  Rules += main~(0,0,1,3) | base~(11,11,2,0) | main~(0,0,11,3) | main~(11,11,0,2)
	  Rules += main~(0,0,1,3) | main~(11,2,0,11) | main~(0,0,11,3) | main~(11,2,0,11)
	  Rules += main~(0,0,1,3) | main~(11,0,2,11) | main~(0,0,11,3) | main~(11,2,0,11)
	  Rules += main~(0,0,1,3) | main~(11,11,2,0) | main~(0,0,11,3) | main~(11,11,0,2)
	  
	  Rules += main~(0,1,13,0) | (base ~> main)~(13,2,0,13)
	  Rules += main~(0,1,13,0) | (base ~> main)~(13,0,2,13)
	  Rules += main~(0,1,13,0) | (base ~> main)~(13,13,2,0)
	  Rules += main~(0,1,3,0) | base~(13,2,0,13) | main~(0,1,13,0) | main~(13,2,0,13)
	  Rules += main~(0,1,3,0) | base~(13,0,2,13) | main~(0,1,13,0) | main~(13,2,0,13)
	  Rules += main~(0,1,3,0) | base~(13,13,2,0) | main~(0,1,13,0) | main~(13,13,0,2)
	  Rules += main~(0,1,3,0) | main~(13,2,0,13) | main~(0,1,13,0) | main~(13,2,0,13)
	  Rules += main~(0,1,3,0) | main~(13,0,2,13) | main~(0,1,13,0) | main~(13,2,0,13)
	  Rules += main~(0,1,3,0) | main~(13,13,2,0) | main~(0,1,13,0) | main~(13,13,0,2)

	  Rules += main~(0,0,11,3) | (base ~> main)~(11,2,13,0)
	  Rules += main~(0,0,1,3) | base~(11,2,13,0) | main~(0,0,11,3) | main~(11,2,13,0)
	  Rules += main~(0,0,1,3) | main~(11,2,13,0) | main~(0,0,11,3) | main~(11,2,13,0)
	  
	  Rules += main~(0,1,13,0) | (base ~> main)~(13,0,11,2)
	  Rules += main~(0,1,3,0) | base~(13,0,11,2) | main~(0,1,13,0) | main~(13,0,11,2)
	  Rules += main~(0,1,3,0) | main~(13,0,11,2) | main~(0,1,13,0) | main~(13,0,11,2)

      //continuation
      Rules += main~(2,11,2,0) | (base ~> main)~WE
      Rules += main~(2,13,2,0) | (base ~> main)~WE
      Rules += main~(2,11,2,0) | (base ~> main)~WC
      Rules += main~(2,13,2,0) | (base ~> main)~WC
 
      Rules += main~(11,2,2,0) | (base ~> main)~WE
      Rules += main~(13,2,2,0) | (base ~> main)~WE
      Rules += main~(11,2,2,0) | (base ~> main)~WC
      Rules += main~(13,2,2,0) | (base ~> main)~WC
	  
	  Rules += main~(0,2,2,11) | (base ~> main)~WE
      Rules += main~(0,2,2,13) | (base ~> main)~WE
      Rules += main~(0,2,2,11) | (base ~> main)~WC
      Rules += main~(0,2,2,13) | (base ~> main)~WC

      Rules += main~(2,11,2,0) | (base ~> main)~WE
      Rules += main~(2,13,2,0) | (base ~> main)~WE
      Rules += main~(2,11,2,0) | (base ~> main)~WC
      Rules += main~(2,13,2,0) | (base ~> main)~WC
	  
      Rules += main~(11,11,2,0) | (base ~> main)~WE
      Rules += main~(13,13,2,0) | (base ~> main)~WE
      Rules += main~(11,11,2,0) | (base ~> main)~WC
      Rules += main~(13,13,2,0) | (base ~> main)~WC
	  
	  Rules += main~(2,2,11,0) | (base ~> main)~(11,3,0,0)
	  Rules += main~(2,0,11,2) | (base ~> main)~(11,3,0,0)
	  Rules += main~(0,2,11,2) | (base ~> main)~(11,3,0,0)
	  Rules += main~(2,2,11,0) | base~(1,3,0,0) | % | main~(11,3,0,0)
	  Rules += main~(2,0,11,2) | base~(1,3,0,0) | % | main~(11,3,0,0)
	  Rules += main~(0,2,11,2) | base~(1,3,0,0) | % | main~(11,3,0,0)
	  Rules += main~(2,2,11,0) | main~(1,3,0,0) | % | main~(11,3,0,0)
	  Rules += main~(2,0,11,2) | main~(1,3,0,0) | % | main~(11,3,0,0)
	  Rules += main~(0,2,11,2) | main~(1,3,0,0) | % | main~(11,3,0,0)
	  
	  Rules += main~(2,11,11,0) | (base ~> main)~(11,3,0,0)
	  Rules += main~(2,0,11,11) | (base ~> main)~(11,3,0,0)
	  Rules += main~(0,11,11,2) | (base ~> main)~(11,3,0,0)
	  Rules += main~(2,11,11,0) | base~(1,3,0,0) | % | main~(11,3,0,0)
	  Rules += main~(2,0,11,11) | base~(1,3,0,0) | % | main~(11,3,0,0)
	  Rules += main~(0,11,11,2) | base~(1,3,0,0) | % | main~(11,3,0,0)
	  Rules += main~(2,11,11,0) | main~(1,3,0,0) | % | main~(11,3,0,0)
	  Rules += main~(2,0,11,11) | main~(1,3,0,0) | % | main~(11,3,0,0)
	  Rules += main~(0,11,11,2) | main~(1,3,0,0) | % | main~(11,3,0,0)

	  // +-intersections
      //from ortho
      Rules += main~WE | (base ~> main)~(2,11,2,2)
      Rules += main~WE | (base ~> main)~(2,13,2,2)
	  Rules += main~WE | (base ~> main)~(2,2,11,2)
	  //from orth-diag top
	  Rules += main~(0,0,11,3) | (base ~> main)~(11,2,2,2)
	  Rules += main~(0,0,1,3) | base~(11,2,2,2) | main~(0,0,11,3) | main~(11,2,2,2)
      //continuation
      Rules += main~(2,11,2,2) | (base ~> main)~WE
      Rules += main~(2,13,2,2) | (base ~> main)~WE
      Rules += main~(2,11,2,2) | (base ~> main)~WC
      Rules += main~(2,13,2,2) | (base ~> main)~WC
	  Rules += main~(2,2,11,2) | (base ~> main)~(11,3,0,0)
	  Rules += main~(2,2,11,2) | base~(1,3,0,0) | % | main~(11,3,0,0)
	  Rules += main~(2,2,11,2) | main~(1,3,0,0) | % | main~(11,3,0,0)
      Rules += main~(11,2,2,2) | (base ~> main)~WE
      Rules += main~(11,2,2,2) | (base ~> main)~WC

	  //temp for OxD Ts until proper T-half is added
	  Rules += main~WE & main~SE | (base ~> main)~NW //initial
	  Rules += main~NS & main~NE | (base ~> main)~WS
	  
	  //Street Roundabouts
	  Rules += main~WE | (base ~> main)~(2,102,102,0) // orth into roundabout x street
	  
	  Rules += main~(0,0,102,102) | (base ~> main)~(102,0,0,102) // base roundabout to base roundabout
	  Rules += main~(0,0,102,102) | (base ~> main)~(102,2,0,102) // base roundabout to roundabout x street 1
	  Rules += main~(0,0,102,102) | (base ~> main)~(102,0,2,102) // base roundabout to roundabout x street 2
	  Rules += main~(0,2,102,102) | (base ~> main)~(102,0,0,102) // roundabout x street to base roundabout 1
	  Rules += main~(2,0,102,102) | (base ~> main)~(102,0,0,102) // roundabout x street to base roundabout 2
	  Rules += main~(0,2,102,102) | (base ~> main)~(102,2,0,102) // roundabout x street to roundabout x street 1a
	  Rules += main~(0,2,102,102) | (base ~> main)~(102,0,2,102) // roundabout x street to roundabout x street 1b
	  Rules += main~(2,0,102,102) | (base ~> main)~(102,2,0,102) // roundabout x street to roundabout x street 2a
	  Rules += main~(2,0,102,102) | (base ~> main)~(102,0,2,102) // roundabout x street to roundabout x street 2b
	  Rules += main~(0,102,102,2) | (base ~> main)~(102,102,2,0) // roundabout x street to roundabout x street 3a
	  Rules += main~(0,102,102,2) | (base ~> main)~(102,102,0,2) // roundabout x street to roundabout x street 3b
	  Rules += main~(2,102,102,0) | (base ~> main)~(102,102,2,0) // roundabout x street to roundabout x street 4a
	  Rules += main~(2,102,102,0) | (base ~> main)~(102,102,0,2) // roundabout x street to roundabout x street 4b
	  
	  Rules += main~(0,0,102,102) | (base ~> main)~(102,0,0,102) & Road~(0,2,0,0) // base roundabout to roundabout x road 1
	  Rules += main~(0,0,102,102) | (base ~> main)~(102,0,0,102) & Road~(0,0,2,0)  // base roundabout to roundabout x road 2
	  Rules += main~(0,0,102,102) & Road~(0,2,0,0) | (base ~> main)~(102,0,0,102) // roundabout x road to base roundabout 1
	  Rules += main~(0,0,102,102) & Road~(2,0,0,0) | (base ~> main)~(102,0,0,102) // roundabout x road to base roundabout 2

	  Rules += main~(0,2,102,102) | (base ~> main)~(102,0,0,102) & Road~(0,2,0,0) // base roundabout x street to roundabout x road 1a
	  Rules += main~(2,0,102,102) | (base ~> main)~(102,0,0,102) & Road~(0,2,0,0)  // base roundabout x street to roundabout x road 1b
	  Rules += main~(0,2,102,102) | (base ~> main)~(102,0,0,102) & Road~(0,0,2,0)  // base roundabout x street to roundabout x road 2a
	  Rules += main~(2,0,102,102) | (base ~> main)~(102,0,0,102) & Road~(0,0,2,0)  // base roundabout x street to roundabout x road 2b
	  Rules += main~(0,0,102,102) & Road~(0,2,0,0) | (base ~> main)~(102,2,0,102) // roundabout x road to roundabout x street 1a
	  Rules += main~(0,0,102,102) & Road~(2,0,0,0) | (base ~> main)~(102,2,0,102) // roundabout x road to roundabout x street 1b
	  Rules += main~(0,0,102,102) & Road~(0,2,0,0) | (base ~> main)~(102,0,2,102) // roundabout x road to roundabout x street 2a
	  Rules += main~(0,0,102,102) & Road~(2,0,0,0) | (base ~> main)~(102,0,2,102) // roundabout x road to roundabout x street 2b
	  Rules += main~(0,102,102,0) & Road~(0,0,0,2) | (base ~> main)~(102,102,2,0) // roundabout x road to roundabout x street 3a
	  Rules += main~(0,102,102,0) & Road~(0,0,0,2) | (base ~> main)~(102,102,0,2) // roundabout x road to roundabout x street 3b
	  Rules += main~(0,102,102,0) & Road~(2,0,0,0) | (base ~> main)~(102,102,2,0) // roundabout x road to roundabout x street 4a
	  Rules += main~(0,102,102,0) & Road~(2,0,0,0) | (base ~> main)~(102,102,0,2) // roundabout x road to roundabout x street 4b

	  Rules += main~(0,0,102,102) & Road~(0,2,0,0) | (base ~> main)~(102,0,0,102) & Road~(0,2,0,0) // roundabout x road to roundabout x road 1a
	  Rules += main~(0,0,102,102) & Road~(2,0,0,0) | (base ~> main)~(102,0,0,102) & Road~(0,2,0,0) // roundabout x road to roundabout x road 1b
  	  Rules += main~(0,0,102,102) & Road~(0,2,0,0) | (base ~> main)~(102,0,0,102) & Road~(0,0,2,0) // roundabout x road to roundabout x road 2a
	  Rules += main~(0,0,102,102) & Road~(2,0,0,0) | (base ~> main)~(102,0,0,102) & Road~(0,0,2,0) // roundabout x road to roundabout x road 2b
	  Rules += main~(0,102,102,0) & Road~(0,0,0,2) | (base ~> main)~(102,102,0,0) & Road~(0,0,2,0) // roundabout x road to roundabout x road 3a
	  Rules += main~(0,102,102,0) & Road~(0,0,0,2) | (base ~> main)~(102,102,0,0) & Road~(0,0,0,2) // roundabout x road to roundabout x road 3b
  	  Rules += main~(0,102,102,0) & Road~(2,0,0,0) | (base ~> main)~(102,102,0,0) & Road~(0,0,2,0) // roundabout x road to roundabout x road 4a
	  Rules += main~(0,102,102,0) & Road~(2,0,0,0) | (base ~> main)~(102,102,0,0) & Road~(0,0,0,2) // roundabout x road to roundabout x road 4b

	  Rules += main~(0,0,102,102) & Road~(0,2,0,0) | (base ~> main)~(102,0,0,102) & Onewayroad~(0,2,0,0) // roundabout x road to roundabout x onewayroad 1a
	  Rules += main~(0,0,102,102) & Road~(2,0,0,0) | (base ~> main)~(102,0,0,102) & Onewayroad~(0,2,0,0) // roundabout x road to roundabout x onewayroad 1b
  	  Rules += main~(0,0,102,102) & Road~(0,2,0,0) | (base ~> main)~(102,0,0,102) & Onewayroad~(0,0,2,0) // roundabout x road to roundabout x onewayroad 2a
	  Rules += main~(0,0,102,102) & Road~(2,0,0,0) | (base ~> main)~(102,0,0,102) & Onewayroad~(0,0,2,0) // roundabout x road to roundabout x onewayroad 2b
	  Rules += main~(0,102,102,0) & Road~(0,0,0,2) | (base ~> main)~(102,102,0,0) & Onewayroad~(0,0,2,0) // roundabout x road to roundabout x onewayroad 3a
	  Rules += main~(0,102,102,0) & Road~(0,0,0,2) | (base ~> main)~(102,102,0,0) & Onewayroad~(0,0,0,2) // roundabout x road to roundabout x onewayroad 3b
  	  Rules += main~(0,102,102,0) & Road~(2,0,0,0) | (base ~> main)~(102,102,0,0) & Onewayroad~(0,0,2,0) // roundabout x road to roundabout x onewayroad 4a
	  Rules += main~(0,102,102,0) & Road~(2,0,0,0) | (base ~> main)~(102,102,0,0) & Onewayroad~(0,0,0,2) // roundabout x road to roundabout x onewayroad 4b

	  Rules += main~(0,0,102,102) | (base ~> main)~(102,0,0,102) & Onewayroad~(0,2,0,0) // base roundabout to roundabout x onewayroad 1
	  Rules += main~(0,0,102,102) | (base ~> main)~(102,0,0,102) & Onewayroad~(0,0,2,0)  // base roundabout to roundabout x onewayroad 2
	  Rules += main~(0,0,102,102) & Onewayroad~(0,2,0,0) | (base ~> main)~(102,0,0,102) // roundabout x onewayroad to base roundabout 1
	  Rules += main~(0,0,102,102) & Onewayroad~(2,0,0,0) | (base ~> main)~(102,0,0,102) // roundabout x onewayroad to base roundabout 2

	  Rules += main~(0,2,102,102) | (base ~> main)~(102,0,0,102) & Onewayroad~(0,2,0,0) // base roundabout x street to roundabout x onewayroad 1a
	  Rules += main~(2,0,102,102) | (base ~> main)~(102,0,0,102) & Onewayroad~(0,2,0,0)  // base roundabout x street to roundabout x onewayroad 1b
	  Rules += main~(0,2,102,102) | (base ~> main)~(102,0,0,102) & Onewayroad~(0,0,2,0)  // base roundabout x street to roundabout x onewayroad 2a
	  Rules += main~(2,0,102,102) | (base ~> main)~(102,0,0,102) & Onewayroad~(0,0,2,0)  // base roundabout x street to roundabout x onewayroad 2b
	  Rules += main~(0,0,102,102) & Onewayroad~(0,2,0,0) | (base ~> main)~(102,2,0,102) // roundabout x onewayroad to roundabout x street 1a
	  Rules += main~(0,0,102,102) & Onewayroad~(2,0,0,0) | (base ~> main)~(102,2,0,102) // roundabout x onewayroad to roundabout x street 1b
	  Rules += main~(0,0,102,102) & Onewayroad~(0,2,0,0) | (base ~> main)~(102,0,2,102) // roundabout x onewayroad to roundabout x street 2a
	  Rules += main~(0,0,102,102) & Onewayroad~(2,0,0,0) | (base ~> main)~(102,0,2,102) // roundabout x onewayroad to roundabout x street 2b
	  Rules += main~(0,0,102,102) & Onewayroad~(0,2,0,0) | (base ~> main)~(102,0,0,102) & Road~(0,2,0,0) // roundabout x onewayroad to roundabout x road 1a
	  Rules += main~(0,0,102,102) & Onewayroad~(2,0,0,0) | (base ~> main)~(102,0,0,102) & Road~(0,2,0,0) // roundabout x onewayroad to roundabout x road 1b
  	  Rules += main~(0,0,102,102) & Onewayroad~(0,2,0,0) | (base ~> main)~(102,0,0,102) & Road~(0,0,2,0) // roundabout x onewayroad to roundabout x road 2a
	  Rules += main~(0,0,102,102) & Onewayroad~(2,0,0,0) | (base ~> main)~(102,0,0,102) & Road~(0,0,2,0) // roundabout x onewayroad to roundabout x road 2b
	  Rules += main~(0,102,102,0) & Onewayroad~(0,0,0,2) | (base ~> main)~(102,102,0,0) & Road~(0,0,2,0) // roundabout x onewayroad to roundabout x road 3a
	  Rules += main~(0,102,102,0) & Onewayroad~(0,0,0,2) | (base ~> main)~(102,102,0,0) & Road~(0,0,0,2) // roundabout x onewayroad to roundabout x road 3b
  	  Rules += main~(0,102,102,0) & Onewayroad~(2,0,0,0) | (base ~> main)~(102,102,0,0) & Road~(0,0,2,0) // roundabout x onewayroad to roundabout x road 4a
	  Rules += main~(0,102,102,0) & Onewayroad~(2,0,0,0) | (base ~> main)~(102,102,0,0) & Road~(0,0,0,2) // roundabout x onewayroad to roundabout x road 4b

	  Rules += main~(0,0,102,102) & Onewayroad~(0,2,0,0) | (base ~> main)~(102,0,0,102) & Onewayroad~(0,2,0,0) // roundabout x onewayroad to roundabout x onewayroad 1a
	  Rules += main~(0,0,102,102) & Onewayroad~(2,0,0,0) | (base ~> main)~(102,0,0,102) & Onewayroad~(0,2,0,0) // roundabout x onewayroad to roundabout x onewayroad 1b
  	  Rules += main~(0,0,102,102) & Onewayroad~(0,2,0,0) | (base ~> main)~(102,0,0,102) & Onewayroad~(0,0,2,0) // roundabout x onewayroad to roundabout x onewayroad 2a
	  Rules += main~(0,0,102,102) & Onewayroad~(2,0,0,0) | (base ~> main)~(102,0,0,102) & Onewayroad~(0,0,2,0) // roundabout x onewayroad to roundabout x onewayroad 2b
	  Rules += main~(0,102,102,0) & Onewayroad~(0,0,0,2) | (base ~> main)~(102,102,0,0) & Onewayroad~(0,0,2,0) // roundabout x onewayroad to roundabout x onewayroad 3a
	  Rules += main~(0,102,102,0) & Onewayroad~(0,0,0,2) | (base ~> main)~(102,102,0,0) & Onewayroad~(0,0,0,2) // roundabout x onewayroad to roundabout x onewayroad 3b
  	  Rules += main~(0,102,102,0) & Onewayroad~(2,0,0,0) | (base ~> main)~(102,102,0,0) & Onewayroad~(0,0,2,0) // roundabout x onewayroad to roundabout x onewayroad 4a
	  Rules += main~(0,102,102,0) & Onewayroad~(2,0,0,0) | (base ~> main)~(102,102,0,0) & Onewayroad~(0,0,0,2) // roundabout x onewayroad to roundabout x onewayroad 4b
	  	  
	  Rules += main~(102,0,2,102) | (base ~> main)~WE
	  Rules += main~(102,0,2,102) | (base ~> main)~WC
	  Rules += main~(102,0,2,102) | (base ~> main)~(2,0,0,2)
	  Rules += main~(102,0,2,102) | (base ~> main)~(2,2,0,0)
      Rules += main~(102,0,2,102) | (base ~> main)~(2,0,11,0)
	  Rules += main~(102,0,2,102) | (base ~> main)~(2,0,13,0)
      Rules += main~(102,0,2,102) | (base ~> main)~WE & (base ~> main)~NS
      Rules += main~(102,0,2,102) | (base ~> main)~WE & (base ~> main)~NC
      Rules += main~(102,0,2,102) | (base ~> main)~NS & (base ~> main)~WC
	  Rules += main~(102,0,2,102) | (base ~> main)~(2,0,131,0)
	  Rules += main~(102,0,2,102) | (base ~> main)~(2,0,131,2)
	  Rules += main~(102,0,2,102) | (base ~> main)~(2,0,133,0)
	  Rules += main~(102,0,2,102) | (base ~> main)~(2,2,133,0)
	  Rules += main~(102,0,2,102) | (base ~> main)~(2,0,151,0)
	  Rules += main~(102,0,2,102) | (base ~> main)~(2,2,151,0)
	  Rules += main~(102,0,2,102) | (base ~> main)~(2,0,153,0)
	  Rules += main~(102,0,2,102) | (base ~> main)~(2,0,153,2)


	  
      for (minor <- CrossNetworks) {
        
        /*if (!minor.isNwm) {  // can't do NWM until more SAM x NWM intersection tiles are defined
          createAdjacentIntersections(main, base, minor)
        }*/
      
        if (base == Street) {
		  
        }

        if (isSingleTile(minor)) {
          // OxO
          Rules += main~WE | (base ~> main)~WE & minor~NS~SN          // OxO from orth
          Rules += main~(0,0,2,2) | (base ~> main)~WE & minor~NS~SN          // OxO from sharp-90
          Rules += main~(11,0,2,0) | (base ~> main)~WE & minor~NS~SN          // OxO from orth-diag
          Rules += main~WE & minor~NS~SN | (base ~> main)~WE          // OxO continue
          Rules += main~WE & minor~NS~SN | (base ~> main)~WC          // OxO continue stub
          Rules += main~WE & minor~NS~SN | (base ~> main)~(2,2,0,0)   // OxO continue sharp-90
          Rules += main~WE & minor~NS~SN | (base ~> main)~(2,0,11,0)  // OxO continue orth-diag

          // OxD (to do: consider asymmetrical)
          Rules += main~WE | (base ~> main)~WE & minor~ES~SE       // OxD
		  Rules += main~WE & minor~ES~SE | (base ~> main)~WE & minor~WN~NW	// OxD Tile 2
          Rules += main~WE & minor~WN~NW | (base ~> main)~WE       // OxD continue
          Rules += main~WE & minor~WN~NW | (base ~> main)~WC       // OxD stub
          if (!minor.isNwm) { // can't do NWM until more SAM x NWM intersection tiles are defined
		    // OxD T
			// Rules += main~WE & minor~WN | base~WN | % | main~WC & main~WN       // OxD T
			
            // DxO
            Rules += main~ES | (base ~> main)~NW & minor~NS       		// DxO
			Rules += main~NE & minor~WE | (base ~> main)~WS & minor~WE // DxO Tile 2
            Rules += main~ES & minor~NS | (base ~> main)~NW       		// DxO continue
						
			// Rules += main~ES & minor~WE | minor~WN | % | main~WC & minor~WN       // DxO T

            // DxD
            Rules += main~ES | (base ~> main)~NW & minor~EN       // DxD
			Rules += main~SE & minor~EN | (base ~> main)~NW & minor~WS // DxD Tile 2
            Rules += main~SE & minor~WS | (base ~> main)~NW       // DxD continue
          }
        }

        if (minor == Road || minor == Onewayroad/*|| minor == Dirtroad || minor == Rhw3 || minor == Mis || minor == Rhw4 || minor == Tla3 || minor == Ave2 || minor == Ard3 || minor == Owr1 || minor == Nrd4 */) { 
          //OxO T (main thru)
          Rules += main~WE | (base ~> main)~WE & minor~NC~CN
          Rules += main~WE | (base ~> main)~WE & minor~CS~SC
          //continue
          Rules += main~WE & minor~NC~CN | (base ~> main)~WE
          Rules += main~WE & minor~CS~SC | (base ~> main)~WE
          //continue stub
          Rules += main~WE & minor~NC~CN | (base ~> main)~WC
          Rules += main~WE & minor~CS~SC | (base ~> main)~WC
		  
		  //OxD T (main thru)
		  Rules += main~WE | (base ~> main)~WE & minor~CWN
		  //continue
		  Rules += main~WE & minor~CWN | (base ~> main)~WE
		  //continue stub
		  Rules += main~WE & minor~CWN | (base ~> main)~WC
		  
		  //DxO T (main thru)
		  Rules += main~SE | (base ~> main)~WN & minor~CE
		  
		  //continue
		  Rules += main~NE & minor~CS | (base ~> main)~SW
		  Rules += main~NE & minor~CS | (base ~> main)~CSW
		  
		  //DxD T (main thru)
		  Rules += main~ES | (base ~> main)~WN & minor~CNE	  // DxD T (Thru)
		  //continue
		  Rules += main~ES & minor~CSW | (base ~> main)~WN	  // Diagonal Continuation off DxD T-Thru
		  Rules += main~NE & minor~CES | (base ~> main)~WS	  // Temp Diagonal Continuation off other side of DxD T-Thru
		  // Rules += main~NE & minor~CES | (base ~> main)~WS & minor~CNW //Eventual DxD T-Thru Tile 2
		  
        }

        if(minor == Road || minor == Onewayroad/*|| minor == Dirtroad || minor == Rhw3 || minor == Mis || minor == Rhw4 || isNwm(minor)*/) { 
          //OxO T (main end)
          Rules += main~WE | (base ~> main)~WC & minor~NS
		  //OxD T (main thru)
		  //DxO T (main end)
			Rules += main~ES | base~CNW & minor~NS | main~(0,0,11,3) | main~CNW & minor~NS   // DxO T from Diag
			Rules += main~(0,0,11,3) | base~CNW & minor~NS | main~(0,0,11,3) | main~CNW & minor~NS   // DxO T from Orth-Diag Top
			Rules += main~ES | base~WC & minor~NS | main~(0,0,11,3) | main~CNW & minor~NS   // DxO T from Diag for Old-Style Diag Streets
			Rules += main~(0,0,11,3) | base~WC & minor~NS | main~(0,0,11,3) | main~CNW & minor~NS   // DxO T from Orth-Diag Top for Old-Style Diag Streets
			Rules += main~CEN & minor~WE | main~CWS & minor~WE | main~NE & minor~WE | main~SW & minor~WE // DxO Ts into DxO + for Old-Style
			Rules += main~CEN & minor~WE | base~CWS & minor~WE | main~NE & minor~WE | main~SW & minor~WE // DxO Ts into DxO + for Old-Style
			Rules += main~NE & minor~WE | main~CWS & minor~WE | main~NE & minor~WE | main~SW & minor~WE // DxO Ts into DxO + for Old-Style
			Rules += main~NE & minor~WE | base~CWS & minor~WE | main~NE & minor~WE | main~SW & minor~WE // DxO Ts into DxO + for Old-Style
			
		  // DxD T (main end)
		    Rules += main~ES | (base ~> main)~CNW & minor~EN      // DxD T (End)
        }
		
		//non-standard two-network + and T ints
		if(minor == Road || minor == Onewayroad) {
			// intersections
			Rules += main~WE | (base ~> main)~(2,2,0,0) & minor~(0,0,1,3)
			// Rules += main~WE | (base ~> main)~(2,0,0,2) & minor~(0,1,3,0)
			Rules += main~WE | (base ~> main)~(2,0,0,0) & minor~(0,2,2,2)
			Rules += main~WE | (base ~> main)~(2,2,0,0) & minor~(0,0,2,2)
			Rules += main~WE | (base ~> main)~(2,2,2,0) & minor~(0,0,0,2)
			Rules += main~WE | (base ~> main)~(2,2,0,2) & minor~(0,0,2,0)
			Rules += main~WE | (base ~> main)~(2,0,0,0) & minor~(0,2,2,0)
			Rules += main~WE | (base ~> main)~(2,2,0,0) & minor~(0,0,2,0)
			Rules += main~WE | (base ~> main)~(2,0,0,2) & minor~(0,2,0,0)	
			// continuations
			Rules += main~(0,0,2,2) & minor~(1,3,0,0) | (base ~> main)~WE
			Rules += main~(0,0,2,2) & minor~(2,2,0,0) | (base ~> main)~WE
			Rules += main~(0,2,2,2) & minor~(2,0,0,0) | (base ~> main)~WE
			Rules += main~(2,2,2,0) & minor~(0,0,0,2) | (base ~> main)~WE
			Rules += main~(0,0,2,2) & minor~(2,0,0,0) | (base ~> main)~WE
			Rules += main~(0,0,2,2) & minor~(0,2,0,0) | (base ~> main)~WE	
		}		

        if (minor.typ == AvenueLike) {
          // OxO
          Rules += main~WE | (base ~> main)~WE & minor~NS             // OxO
          Rules += main~WE & minor~NS | (base ~> main)~WE & minor~SN  // OxO far side
          Rules += main~WE & minor~SN | (base ~> main)~WE             // OxO continue
          Rules += main~WE & minor~SN | (base ~> main)~WC             // OxO stub
          // OxD
		  Rules += main~WE | (base ~> main)~WC & minor~ES                           // OxD Short-T
          Rules += main~WE | (base ~> main)~WE & minor~ES                           // OxD start
          Rules += main~WE & minor~ES | (base ~> main)~WE & minor~SharedDiagRight   // OxD middle
          Rules += main~WE & minor~SharedDiagRight | (base ~> main)~WE & minor~NW   // OxD end
          Rules += main~WE & minor~NW | (base ~> main)~WE                           // OxD continue
          Rules += main~WE & minor~NW | (base ~> main)~WC                           // OxD continue stub
          // DxO
          Rules += main~ES | (base ~> main)~NW & minor~NS             // DxO start
          Rules += main~EN & minor~EW | (base ~> main)~SW & minor~EW  // DxO middle 1
          Rules += main~ES & minor~NS | (base ~> main)~NW & minor~SN  // DxO middle 2
          Rules += main~EN & minor~WE | (base ~> main)~SW & minor~WE  // DxO end
          Rules += main~ES & minor~SN | (base ~> main)~NW             // DxO continue
          Rules += main~ES & minor~SN | (base ~> main)~(3,0,0,0)      // DxO continue stub
          // DxD
          Rules += main~ES | (base ~> main)~NW & minor~NE                          // DxD start
          Rules += main~EN & minor~ES | (base ~> main)~SW & minor~SharedDiagRight  // DxD middle
          Rules += main~ES & minor~SharedDiagLeft | (base ~> main)~NW & minor~SW   // DxD end
          Rules += main~ES & minor~SW | (base ~> main)~NW                          // DxD continue
          Rules += main~ES & minor~SW | (base ~> main)~(3,0,0,0)                   // DxD continue stub
		  
		  //T-intersections
		  // OxO
		  Rules += main~WE | (base ~> main)~WC & minor~NS 				//OxO Short T
		  Rules += main~WE & minor~NS | (base ~> main)~WC & minor~SN 	//OxO Long T
		  
		  if (minor == Avenue) {
				Rules += main~WE | (base ~> main)~WE & minor~NC			// OxO End T Tile 1
				Rules += main~WE & minor~NC | (base ~> main)~WE & minor~CN // OxO End T Tile 2
				
				Rules += main~WE | (base ~> main)~(2,2,0,2) & Avenue~CE
				Rules += main~WE | (base ~> main)~(2,0,2,2) & Avenue~NC
				Rules += main~WE | (base ~> main)~(2,0,0,0) & Avenue~(0,2,4,0)
				//continuation in case of Avenue ending at 3 SAMs
				Rules += main~(2,0,2,2) & Avenue~NC | (base ~> main)~WE & Avenue~CN
				Rules += main~(2,0,2,2) & Avenue~NC | (base ~> main)~(2,0,2,2) & Avenue~CN
				Rules += main~WE & Avenue~CS | (base ~> main)~(2,2,2,0) & Avenue~SC
				
				//SAM continuations
				Rules += main~WE & minor~CN | (base ~> main)~WE
				Rules += main~(2,0,2,2) & minor~CN | (base ~> main)~WE
				Rules += main~WE & minor~CN | (base ~> main)~WC
				Rules += main~(2,0,2,2) & minor~CN | (base ~> main)~WC
		  }
        }
		
		for(minor2 <- CrossNetworks if minor2 != main) {
          if (hasRightShoulder(minor2)) {
			Rules += main~WE & minor~SN~NS | (base ~> main)~WE & minor2~NS~SN   // OxO | OxO adj
		    Rules += main~WE & minor~WN~NW | (base ~> main)~WE & minor2~NS~SN 	// OxD | OxO adjacencies
		    Rules += main~WE & minor~NS~SN | (base ~> main)~WE & minor2~ES~SE 	// OxO | OxD adjacencies
			Rules += main~ES & minor~NS~SN | (base ~> main)~NW & minor2~NS~SN 	// DxO | DxO continue
			Rules += main~ES & minor~NS~SN | (base ~> main)~NW & minor2~EN~NE 	// DxO | DxD continue
			Rules += main~ES & minor~SW~WS | (base ~> main)~NW & minor2~NS~SN 	// DxD | DxO continue
			Rules += main~ES & minor~SW~WS | (base ~> main)~NW & minor2~EN~NE 	// DxO | DxO continue
          }
        }		
		//for SAM x SAM intersection adjacencies
		Rules += main~WE & minor~SN~NS | (base ~> main)~WE & (base ~> main)~NS~SN   // OxO | OxO adj
		Rules += main~WE & minor~WN~NW | (base ~> main)~WE & (base ~> main)~NS~SN 	// OxD | OxO adjacencies
		Rules += main~WE & minor~NS~SN | (base ~> main)~WE & (base ~> main)~ES~SE 	// OxO | OxD adjacencies
		Rules += main~ES & minor~NS~SN | (base ~> main)~NW & (base ~> main)~NS~SN 	// DxO | DxO continue
		Rules += main~ES & minor~NS~SN | (base ~> main)~NW & (base ~> main)~EN~NE 	// DxO | DxD continue
		Rules += main~ES & minor~SW~WS | (base ~> main)~NW & (base ~> main)~NS~SN 	// DxD | DxO continue
		Rules += main~ES & minor~SW~WS | (base ~> main)~NW & (base ~> main)~EN~NE 	// DxO | DxO continue

		Rules += main~WE & main~SN~NS | (base ~> main)~WE & minor~NS~SN   	// OxO | OxO adj
		Rules += main~WE & main~WN~NW | (base ~> main)~WE & minor~NS~SN 	// OxD | OxO adjacencies
		Rules += main~WE & main~NS~SN | (base ~> main)~WE & minor~ES~SE 	// OxO | OxD adjacencies
		Rules += main~ES & main~NS~SN | (base ~> main)~NW & minor~NS~SN 	// DxO | DxO continue
		Rules += main~ES & main~NS~SN | (base ~> main)~NW & minor~EN~NE 	// DxO | DxD continue
		Rules += main~ES & main~SW~WS | (base ~> main)~NW & minor~NS~SN 	// DxD | DxO continue
		Rules += main~ES & main~SW~WS | (base ~> main)~NW & minor~EN~NE 	// DxO | DxO continue
		
		Rules += main~WE & main~SN~NS | (base ~> main)~WE & (base ~> main)~NS~SN   	// OxO | OxO adj
		Rules += main~WE & main~WN~NW | (base ~> main)~WE & (base ~> main)~NS~SN 	// OxD | OxO adjacencies
		Rules += main~WE & main~NS~SN | (base ~> main)~WE & (base ~> main)~ES~SE 	// OxO | OxD adjacencies
		Rules += main~ES & main~NS~SN | (base ~> main)~NW & (base ~> main)~NS~SN 	// DxO | DxO continue
		Rules += main~ES & main~NS~SN | (base ~> main)~NW & (base ~> main)~EN~NE 	// DxO | DxD continue
		Rules += main~ES & main~SW~WS | (base ~> main)~NW & (base ~> main)~NS~SN 	// DxD | DxO continue
		Rules += main~ES & main~SW~WS | (base ~> main)~NW & (base ~> main)~EN~NE 	// DxO | DxO continue
		
		//Transitions
		if(minor == Road || minor == Onewayroad) {
			Rules += main~WE | (base ~> main)~WC & minor~CE	// orth transition
			Rules += main~SE | (base ~> main)~CNW & Road~CWN // diag transition // (change Road to minor when OWR version is added)
			Rules += main~WE | (base ~> main)~WC & minor~CS // bending transition
			Rules += main~WE | (base ~> main)~WC & minor~CWN // bending transition 1
			Rules += main~WE | (base ~> main)~WC & minor~CES // bending transition 2
		}
		//Avenue-specific Transitions
		Rules += main~WE | (base ~> main)~WC & Avenue~(0,0,2,4)
      }
	  		
    }
    createRules()
  }
}

// Compile individually with `sbt "runMain metarules.module.CompileSamCode"`.
object CompileSamCode extends AbstractMain {
  lazy val resolve: IdResolver = new SamResolver orElse new MiscResolver orElse new NwmResolver
  val generator = new SamRuleGenerator(_)
  lazy val file = new java.io.File("target/Sec9_SAM_MetaGenerated_MANAGED.txt")
}