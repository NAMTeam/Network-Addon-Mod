package metarules.module

import org.scalatest.{WordSpec, Matchers}
import metarules.meta._
import syntax._, Implicits._, RotFlip._, Network._, Flags._

class NwmResolverSpec extends WordSpec with Matchers {

  "NwmResolver" should {
    val resolve = new NwmResolver
    "be compatible with original O×O IID scheme" in {
      for (rf <- RotFlip.values) {
        resolve((Ard3~SN & Rd4~EW) * rf).rf shouldBe rf
        if (!rf.flipped) {
          resolve((Ave2~SN & Rd4~EW) * rf).rf shouldBe rf
          resolve((Rail~SN & Rd4~EW) * rf).rf shouldBe rf
          resolve((Ard3~SN & Rail~EW) * rf).rf shouldBe rf
        }
      }
    }
  }
}
