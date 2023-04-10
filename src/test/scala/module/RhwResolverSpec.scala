package metarules.module

import org.scalatest.{WordSpec, Matchers}
import metarules.meta._
import syntax._, Implicits._, RotFlip._, Network._, Flags._

class RhwResolverSpec extends WordSpec with Matchers {

  "RHW networks" should {
    "have correctly initialized IIDs" in {
      assert {
        Network.values.forall { n =>
          (RhwResolver.rhwPieceId.contains(n) || !RhwResolver.rhwRangeId.contains(n)) &&
            (RhwResolver.rhwPieceId.get(n) forall { id => id / 0x10 % 0x10 == n.height }) &&
            (RhwResolver.rhwRangeId.get(n) forall { id => id / 0x100000 % 0x10 == n.height && id / 0x10000000 == 5 })
        }
      }
    }
  }
}
