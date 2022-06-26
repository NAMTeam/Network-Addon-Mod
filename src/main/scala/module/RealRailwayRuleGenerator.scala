package metarules.module

import metarules.meta._
import Network._, Flags._, Flag._, RotFlip._, Implicits._
import scala.collection.mutable.Buffer
import NetworkProperties._


class RealRailwayRuleGenerator(val resolver: IdResolver) extends RuleGenerator {
  def start(): Unit = {
    Rules += Str~WE | (Rail ~> Str)~WE & Road~NS
    createRules()
  }
}