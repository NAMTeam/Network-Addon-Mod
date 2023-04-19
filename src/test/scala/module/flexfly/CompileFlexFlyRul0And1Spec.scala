package com.sc4nam.module
package flexfly

import syntax.Tile
import CompileFlexFlyRul0And1._
import resource._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class CompileFlexFlyRul0And1Spec extends AnyWordSpec with Matchers {

  "FlexFly RUL1 falsies" should {
    "be permanent" in {
      for (scanner <- managed(new java.util.Scanner(getClass.getResourceAsStream("/FlexFlyRUL1.txt")))) {
        import scala.jdk.CollectionConverters._
        val previousFalsies = scanner.asScala.filter(_.nonEmpty).toSeq

        val resolve = new FlexFlyResolver
        val currentFalsies = (for {
          seg <- flexFlySegs.iterator
          idTile = resolve(Tile(seg))
          falsie = convertVirtualTile(seg)
          line <- rul1Entry(falsie, idTile.id, seg.toString).linesIterator
          if line.nonEmpty
        } yield line).toSeq

        assert(previousFalsies sameElements currentFalsies,
          "FlexFly falsies were different from specification, but they should NEVER change!")
      }
    }
  }

}
