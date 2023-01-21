package networkaddonmod.localization

import java.io.{File, FileReader}
import resource._
import rapture.core.strategy.throwExceptions
import scdbpf._

/** Run
  *
  * {{{
  * sbt generateLocales
  * }}}
  *
  * to generate the locale .dat files from the .po translation files contained
  * in the `ltext/` directory. The generated .dat files can be found in the
  * directory `target/locale/`.
  *
  * Watch out for warnings about outdated translations.
  */
object GenerateLocales {

  // add additional languages here
  val languageOffset = Map(
    "de" -> 4)

  def parseTgi(s: String): Tgi = {
    val arr = s.trim().split("-").map(id => java.lang.Long.parseLong(id, 16).toInt)
    Tgi(arr(0), arr(1), arr(2))
  }

  def formatTgi(tgi: Tgi) = f"${tgi.tid}%08X-${tgi.gid}%08X-${tgi.iid}%08X"

  val logger = java.util.logging.Logger.getLogger("networkaddonmod.localization")

  /** Constructs a mapping from TGIs to `(original, translation)`.
    */
  def readTranslationFiles(inputDir: File, suffix: String): Map[Tgi, (String, String)] = {
    val m = scala.collection.mutable.Map.empty[Tgi, (String, String)]
    val inputFiles = inputDir.listFiles.filter(f => f.isFile && f.getName.endsWith(suffix))
    for (inputFile <- inputFiles) {
      val i18n = (managed(new FileReader(inputFile)) acquireAndGet { reader =>
        scaposer.Parser.parse(reader) match {
          case Left(e) => throw new UnsupportedOperationException(s"$e in $inputFile")
          case Right(x) => scaposer.I18n(x)
        }
      })
      for (((context, original), translations) <- i18n.ctxSingularToStrs) {
        if (!(context.isEmpty && original.isEmpty)) { // otherwise it is the meta-info header which is ignored
          if (context.isEmpty) {
            throw new UnsupportedOperationException(s"missing TGI context for $original in $inputFile")
          }
          val tgi = try (parseTgi(context)) catch { case _: NumberFormatException =>
            throw new UnsupportedOperationException(s"message context should be a TGI: $context in $inputFile")
          }
          if (translations.length != 1) {
            throw new UnsupportedOperationException(s"plural translations are not supported: $tgi in $inputFile")
          } else if (m.contains(tgi)) {
            throw new UnsupportedOperationException(s"duplicate definition for $tgi in $inputFile")
          }
          m(tgi) = (original, translations.head)
        }
      }
    }
    m.toMap
  }

  def main(args: Array[String]): Unit = {
    val targetDir = new File("target/locale/")
    targetDir.mkdirs()

    /* ---- English ---- */
    val templates: Map[Tgi, (String, String)] = readTranslationFiles(new File("ltext"), ".pot")
    val defaultLang = "en"
    val defaultEntries = templates.toSeq.map { case (tgi, (original, translation)) =>
      require(translation.isEmpty, s"msgstr should be empty in template files: $tgi")
      BufferedEntry(tgi, LText(original), compressed = true)
    }
    logger.info(s"generating locale $defaultLang")
    DbpfFile.write(defaultEntries, new File(targetDir, s"NetworkAddonMod_Locale_$defaultLang.dat"))

    /* ---- other languages ---- */
    for (lang <- languageOffset.keys) {
      val translations: Map[Tgi, (String, String)] = readTranslationFiles(new File(s"ltext/$lang"), ".po")
      require(translations.keySet.subsetOf(templates.keySet),
        s"the following TGIs have translations ($lang), " +
        s"but need corresponding entries in the .pot template files " +
        f"-- their TGIs may have changed:%n${translations.keySet.diff(templates.keySet).map(formatTgi)}")

      val outdated = translations.keySet.filter(tgi => translations(tgi)._1 != templates(tgi)._1)
      if (!outdated.isEmpty) {
        logger.warning(
          s"English LText templates for the following TGIs have changed, " +
          f"so outdated translations ($lang) need to be updated:%n${outdated.map(formatTgi)}")
      }

      val nontranslated = translations.keySet.filter(tgi => translations(tgi)._2.isEmpty)

      val entries = translations.toSeq.collect {
        case (tgi, (original, translation)) if !outdated(tgi) && !nontranslated(tgi) =>
          // Note that the outdated translations or nontranslated strings are not included in the .dat file
          BufferedEntry(tgi.copy(gid = tgi.gid + languageOffset(lang)), LText(translation), compressed = true)
      }
      val percentage = 100 * entries.length.toDouble / defaultEntries.length
      logger.info(f"generating locale $lang (${percentage.floor}%.0f%% complete)")
      DbpfFile.write(entries, new File(targetDir, f"NetworkAddonMod_Locale_$lang.dat"))
    }
  }
}
