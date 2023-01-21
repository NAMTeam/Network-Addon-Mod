package networkaddonmod.localization

import java.io.{File, FileReader, PrintWriter}
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

  def createDatFiles(): Unit = {
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
      if (outdated.nonEmpty) {
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

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      createDatFiles()  // primary use: convert .po to .dat
    } else {
      createPoFiles(args)  // secondary use: convert .dat to .po
    }
  }

  def escape(raw: String): String = {
    import scala.reflect.runtime.universe.{Literal, Constant}
    Literal(Constant(raw)).toString.replace(raw"\'", "'")  // add any other necessary character replacements here
  }

  def formatText(s: String): String = {
    var arr = s.split("(?<=(\n))")  // splits multiline strings after newlines
    assert(arr.length > 0)
    if (arr.length != 1) {  // multiline strings start on the following line, oneline strings on the same line
      arr = "" +: arr
    }
    arr.map(escape).mkString(System.lineSeparator())
  }

  /** Broad classification of LTexts into categories (not fully accurate).
    */
  def categorize(tgi: Tgi): String = {
    if (tgi.gid == 0x2A592FD1) {
      "puzzlepieces-" + ((tgi.iid >>> 24) match {
        case 0x50 => "road"
        case 0x51 => "NWM"
        case 0x52 | 0x5A => "highway"
        case 0x53 => "rail"
        case 0x54 => "avenue"
        case 0x55 => "street"
        case 0x57 | 0x5E => "RHW"
        case 0x58 => "lightrail"
        case 0x59 => "onewayroad"
        case 0x5B => "turninglanes"
        case 0x5C => "viaducts"
        case 0x5D => "monorail"
        case _ => "other"
      })
    } else tgi.gid match {
      case 0x123006AA | 0x123006BB | 0x2A3858E4 => "buttons"
      case 0x6A231EAA | 0xCBE084CB | 0xA82CA30F | 0xA92A02EA => "bridges"
      case _ => "stations"
    }
  }

  /* Run
   * {{{
   * sbt 'runMain networkaddonmod.localization.GenerateLocales 0 <english-dat-file>'
   * }}}
   * to generate .pot template files and
   * {{{
   * sbt 'runMain networkaddonmod.localization.GenerateLocales <language-offset> <english-dat-file> <foreign-dat-file>'
   * }}}
   * to generate .po files from existing translations
   * and then manually inspect the results for correctness.
   * The generated files are located at `target/ltext/`.
   */
  def createPoFiles(args: Array[String]): Unit = {
    val targetDir = new File("target/ltext/")
    targetDir.mkdirs()
    val offset = args(0).toInt
    val englishDat = new File(args(1))
    val translationDat = args.lift(2).map(new File(_))

    val entries = DbpfFile.read(englishDat).entries.sortBy(_.tgi).filter(_.tgi.matches(Tgi.LText))
    val translations: Option[Map[Tgi, DbpfEntry]] = translationDat.map(DbpfFile.read(_).tgiMap)
    def translate(tgi: Tgi): String = {
      translations.flatMap(_.get(tgi.copy(gid = tgi.gid + offset)))
        .map(_.toBufferedEntry.convert[LText].content.text)
        .getOrElse("")  // either a template file or the translation is missing
    }

    for ((category, categorizedEntries) <- entries.groupBy(e => categorize(e.tgi))) {
      val lang = if (offset == 0) "en" else languageOffset.find(_._2 == offset).get._1
      val outputFile = new File(targetDir, if (offset == 0) s"$category.pot" else s"$lang/$category.po")
      if (offset == 0 || categorizedEntries.iterator.exists(e => translate(e.tgi).nonEmpty)) {  // we only generate .po files that contain at least some translations already
        outputFile.getParentFile().mkdirs()
        for (printer <- managed(new PrintWriter(outputFile, "UTF-8"))) {
          printer.println(s"msgid ${escape("")}")
          printer.println(s"msgstr ${escape("")}")
          printer.println(escape("Project-Id-Version: \n"))
          printer.println(escape("PO-Revision-Date: \n"))
          printer.println(escape("Last-Translator: \n"))
          printer.println(escape("Language-Team: \n"))
          printer.println(escape(s"Language: $lang\n"))
          printer.println(escape("MIME-Version: 1.0\n"))
          printer.println(escape("Content-Type: text/plain; charset=UTF-8\n"))
          printer.println(escape("Content-Transfer-Encoding: 8bit\n"))

          for (e <- categorizedEntries) {
            val text = e.toBufferedEntry.convert[LText].content.text
            printer.println()
            printer.println(s"""msgctxt "${formatTgi(e.tgi)}"""")
            printer.println(s"""msgid ${formatText(text)}""")
            printer.println(s"""msgstr ${formatText(translate(e.tgi))}""")
          }
        }
      }
    }
  }
}
