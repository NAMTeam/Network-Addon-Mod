package metarules.module

import metarules.meta._
import Network._, Flags._, Flag._, RotFlip._, Implicits._
import scala.collection.mutable.Buffer
import NetworkProperties._


class RealRailwayRuleGenerator(val resolver: IdResolver) extends RuleGenerator {
  def start(): Unit = {
    /*
    Generate OxO rules by iteration over list of supported crossings
    */
    val RrwNetworks = List(L1Dtr, L2Dtr)
    val CrossNetworks = List(Road, L1Road, L2Road)
    
    for (main <- RrwNetworks; base <- main.base) {
      Rules += main~WE | (base ~> main)~WE      // ortho
      for (minor <- CrossNetworks if minor.height != main.height) {
        Rules += main~WE | (base ~> main)~WE & minor~NS // OxO
        Rules += main~WE & minor~NS | (base ~> main)~WE // OxO continue
        for(minor2 <- CrossNetworks if minor2.height != main.height) {
          Rules += main~WE & minor~NS | (base ~> main)~WE & minor2~NS // OxO | OxO adjacencies
        }
        
        Rules += main~WE | (base ~> main)~WE & minor~SE // OxD
        Rules += main~EW & minor~SW | (base ~> main)~WE // OxD continue
        for(minor2 <- CrossNetworks if minor2.height != main.height) {
          Rules += main~WE & minor~WN | (base ~> main)~WE & minor2~NS // OxD | OxO adjacencies
        }
      }
    }
    createRules()
  }
}