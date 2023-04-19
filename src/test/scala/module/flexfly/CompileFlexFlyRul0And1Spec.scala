package metarules
package module.flexfly

import module.syntax.Tile
import CompileFlexFlyRul0And1._
import resource._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class CompileFlexFlyRul0And1Spec extends AnyWordSpec with Matchers {

  "FlexFly RUL1 falsies" should {
    "be permanent" in {
      for (scanner <- managed(new java.util.Scanner(getClass.getResourceAsStream("/FlexFlyRUL1.txt")))) {
        val previousFalsies = collection.JavaConversions.asScalaIterator(scanner).filter(_.nonEmpty)

        val resolve = new FlexFlyResolver
        val currentFalsies = for {
          seg <- flexFlySegs.iterator
          idTile = resolve(Tile(seg))
          falsie = convertVirtualTile(seg)
          line <- collection.JavaConversions.asScalaIterator(rul1Entry(falsie, idTile.id, seg.toString).lines.iterator)
          if line.nonEmpty
        } yield line

        assert(previousFalsies sameElements currentFalsies,
          "FlexFly falsies were different from specification, but they should NEVER change!")
      }
    }
  }

}
