package metarules.pathing.nwmpaths

import metarules._
import module._

object Main extends App {

  implicit val resolver = new RhwResolver orElse new MiscResolver orElse new NwmResolver
  val entries = PathCreator.generateNwmPaths
  val file = new java.io.File("target/nwmTurnPaths.dat")
  import rapture.core.strategy.throwExceptions
  scdbpf.DbpfFile.write(entries, file)

}
