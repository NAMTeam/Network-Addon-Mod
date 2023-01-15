package networkaddonmod.localization

import java.io.{File, FileInputStream}
import resource._
import rapture.core.strategy.throwExceptions
import scdbpf._
import org.yaml.snakeyaml.Yaml

/** Run
  *
  * {{{
  * sbt "runMain networkaddonmod.localization.GenerateLocales"
  * }}}
  *
  * to generate the locale .dat files from the .yaml translation files contained
  * in the `ltext/` directory. The generated .dat files can be found in the
  * directory `target/locale/`.
  */
object GenerateLocales {

  // add additional languages here
  val languageOffset = Map(
    "de" -> 3)

  def parseTgi(s: String): Tgi = {
    val arr = s.trim().split("-").map(id => Integer.parseInt(id, 16))
    Tgi(arr(0), arr(1), arr(2))
  }

  def main(args: Array[String]): Unit = {
    val yaml = new Yaml()
    val targetDir = new File("target/locale/")
    targetDir.mkdirs()
    for (lang <- languageOffset.keys) {
      val inputDir = new File(f"ltext/${lang}")
      val inputFiles = inputDir.listFiles.filter(_.isFile).toList.filter { file =>
        file.getName.endsWith(".yaml")}
      val translations = new java.util.HashMap[String, String]
      for (inputFile <- inputFiles) {
        translations.putAll(managed(new FileInputStream(inputFile)) acquireAndGet { stream =>
        yaml.load(stream): java.util.HashMap[String, String]
      })
      }
      import scala.collection.JavaConverters._
      val entries = translations.asScala.toSeq.map { case (key, text) =>
        val tgi = parseTgi(key)
        BufferedEntry(
          tgi = tgi.copy(gid = tgi.gid + languageOffset(lang)),
          content = LText(text),
          compressed = true)
      }
      DbpfFile.write(entries, new File(targetDir, f"NetworkAddonMod_Locale_${lang}.dat"))
    }
  }
}
