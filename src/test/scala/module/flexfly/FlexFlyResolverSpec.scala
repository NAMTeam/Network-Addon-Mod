package com.sc4nam.module
package flexfly

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class FlexFlyResolverSpec extends AnyWordSpec with Matchers {
  "FlexFlyResolver" should {
    "be a 1-to-1 correspondence (bijective)" in {
      val resolve = new FlexFlyResolver
      val reverseTileMap = resolve.tileMap.map { case (a, b) => (b, a) }
      for ((tile, idTile) <- resolve.tileMap) {
        reverseTileMap(idTile).shouldBe(tile)
      }
    }
  }
}
