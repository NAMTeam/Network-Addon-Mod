package metarules.module

import java.io.File
import metarules.meta.RotFlip
import syntax.{RuleGenerator, IdResolver, RuleTransducer}

/** This is the main class that invokes the compilation of all metarules. New
  * metarule code generators need to be added to the list of invocations in this
  * file. Make sure the generated files end in "_MANAGED" to keep them from
  * getting tracked.
  */
object CompileAllMetarules {

  def main(args: Array[String]): Unit = {
    LOGGER.info("compiling FlexFly RUL0 and RUL1")
    flexfly.CompileFlexFlyRul0And1.main(Array.empty)
    // Generate FlexFly models and paths (requires some .dat files, see comment in that file).
    flexfly.CompileFlexFlyResources.main(Array.empty)

    // Compilation of metarule code.
    RegenerateTileOrientationCache.withCache().acquireFor(compileMetarulesOnce)

    // For the time being, INRUL compilation is disabled as the INRULs have been
    // merged into single files again.
    // CompileInruls.main(Array.empty)

    // Compile paths for diagonal NWM crossings
    LOGGER.info("compiling diagonal NWM paths")
    metarules.pathing.nwmpaths.Main.main(Array.empty)
  }

  /** Add additional rule generators here.
    */
  def compileMetarulesOnce(tileOrientationCache: collection.mutable.Map[Int, Set[RotFlip]]): Unit = {
    LOGGER.info("compiling FlexFly metarule code")
    flexfly.CompileFlexFlyCode.start(tileOrientationCache = tileOrientationCache)
    LOGGER.info("compiling RRW metarule code")
    CompileRealRailwayCode.start(tileOrientationCache = tileOrientationCache)
    LOGGER.info("compiling RHW metarule code")
    CompileRhwCode.start(tileOrientationCache = tileOrientationCache)
    LOGGER.info("compiling SAM metarule code")
    CompileSamCode.start(tileOrientationCache = tileOrientationCache)
    LOGGER.info("compiling Onslope metarule code")
    CompileOnslopeCode.start(tileOrientationCache = tileOrientationCache)
  }
}

// Compile individually with `sbt "runMain metarules.module.CompileRhwCode"`.
object CompileRhwCode extends AbstractMain {
  lazy val resolve: IdResolver = new MiscResolver orElse new RhwResolver orElse new NwmResolver
  val generator = new RhwRuleGenerator(_)
  lazy val file = new File("target/RhwMetaGenerated_MANAGED.txt")
}

