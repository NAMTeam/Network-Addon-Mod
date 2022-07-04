package metarules.module

/** This is the main class that invokes the compilation of all metarules. New
  * metarule code generators need to be added to the list of invocations in this
  * file. Make sure the generated files end in "_MANAGED" to keep them from
  * getting tracked.
  */
object CompileAllMetarules {

  def main(args: Array[String]): Unit = {
    flexfly.CompileFlexFlyRul0And1.main(Array.empty)
    flexfly.CompileFlexFlyCode.main(Array.empty)

    // Uncomment the next line to generate models and paths (requires some .dat files, see comment in that file).
    flexfly.CompileFlexFlyResources.main(Array.empty)

    // For the time being, INRUL compilation is disabled as the INRULs have been
    // merged into single files again.
    // CompileInruls.main(Array.empty)
  }

}
