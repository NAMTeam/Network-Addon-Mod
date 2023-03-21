package metarules.module

import java.io.File
import metarules.meta.{RuleGenerator, IdResolver}

/** This is the main class that invokes the compilation of all metarules. New
  * metarule code generators need to be added to the list of invocations in this
  * file. Make sure the generated files end in "_MANAGED" to keep them from
  * getting tracked.
  */
object CompileAllMetarules {

  def main(args: Array[String]): Unit = {
    flexfly.CompileFlexFlyRul0And1.main(Array.empty)
    flexfly.CompileFlexFlyCode.main(Array.empty)

    // Generate FlexFly models and paths (requires some .dat files, see comment in that file).
    flexfly.CompileFlexFlyResources.main(Array.empty)

    // temporarily disabled due to errors
    // CompileRealRailwayCode.main(Array.empty)

    CompileRhwCode.main(Array.empty)

    // For the time being, INRUL compilation is disabled as the INRULs have been
    // merged into single files again.
    // CompileInruls.main(Array.empty)

    // Compile paths for diagonal NWM crossings
    metarules.pathing.nwmpaths.Main.main(Array.empty)
  }

}

// Compile individually with `sbt "runMain metarules.module.CompileRhwCode"`.
object CompileRhwCode extends AbstractMain {
  lazy val resolve: IdResolver = new MiscResolver orElse new RhwResolver orElse new NwmResolver
  lazy val generator: RuleGenerator = new RhwRuleGenerator(resolve)
  lazy val file = new File("target/RhwMetaGenerated_MANAGED.txt")
}
