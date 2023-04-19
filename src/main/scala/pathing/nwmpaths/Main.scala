package com.sc4nam.pathing.nwmpaths

import io.github.memo33.scdbpf
import com.sc4nam.module._

object Main extends App {

  implicit val resolver = new RhwResolver orElse new MiscResolver orElse new NwmResolver
  val entries = PathCreator.generateNwmPaths
  val file = new java.io.File("target/nwmTurnPaths.dat")
  import scdbpf.strategy.throwExceptions
  scdbpf.DbpfFile.write(entries, file)

}
