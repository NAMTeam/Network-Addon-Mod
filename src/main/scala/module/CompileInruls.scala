package metarules.module

import java.io.{File, FileInputStream}
import resource._
import rapture.io._
import rapture.core.strategy.throwExceptions
import scdbpf._

/** Automates the process of compiling the INRUL files.
  */
object CompileInruls {

  def main(args: Array[String]): Unit = {
    val inrulDir = new File("Controller/INRULs")
    val targetDir = new File("target/INRULs")
    targetDir.mkdirs()

    // creates a RUL file from IID, parent folder name and list of source files
    def mkRul(id: Int, parent: String, fileNames: String*): DbpfEntry = {
      val srcDir = new File(inrulDir, parent)
      buildRul(id, fileNames map (new File(srcDir, _)))
    }

    // main INRUL file
    // for ease of testing, this currently contains all the INRULs
    // files marked "to be removed" are not present in the actual main INRUL file
    val allRuls = (Seq.newBuilder[DbpfEntry]
      += mkRul(0x01, "HighwayBasic", "RUL01_EMHWY_Basic.rul")
      += mkRul(0x02, "HighwayAdvanced", "RUL02_EMHWY_Advanced.rul")
      //+= mkRul(0x03, "SubwayBasic", "RUL03_Subway_Basic.rul")
      //+= mkRul(0x04, "SubwayAdvanced", "RUL04_Subway_Advanced.rul")
      += mkRul(0x05, "RailBasic", "RUL05_Rail_Basic.rul")
      += mkRul(0x06, "RailAdvanced", "RUL06_Rail_Advanced.rul")
      += mkRul(0x07, "RoadBasic", "RUL07_Road_Basic.rul")
      += mkRul(0x08, "RoadAdvanced",
        "Head.txt", "DiagonalIntersections.txt", "General.txt", "FAR3.txt", "FAR2.txt") // to be removed
      += mkRul(0x09, "StreetBasic", "RUL09_Street_Diagonal_Streets.rul") // to be removed
      += mkRul(0x0A, "StreetAdvanced", "Head.txt", "Roundabouts.txt", "WideRadiusCurves.txt") // to be removed
      //+= mkRul(0x0B, "PipeBasic", "RUL0B_Pipe_Basic.rul")
      //+= mkRul(0x0C, "PipeAdvanced", "RUL0C_Pipe_Advanced.rul")
      += mkRul(0x0D, "AvenueBasic", "RUL0D_Avenue_Basic.rul")
      += mkRul(0x0E, "AvenueAdvanced", "RUL0E_Avenue_Advanced.rul")
      += mkRul(0x0F, "LightrailBasic", "RUL0F_LightRail_Basic.rul") // to be removed
      += mkRul(0x10, "LightrailAdvanced", "RUL10_LightRail_Advanced.rul") // to be removed
      += mkRul(0x11, "OnewayroadBasic", "RUL11_OWR_Basic.rul")
      += mkRul(0x12, "OnewayroadAdvanced", "RUL12_OWR_Advanced.rul") // to be removed
      += mkRul(0x13, "DirtroadBasic", "RUL13_RHW_Basic.rul")
      += mkRul(0x14, "DirtroadAdvanced",
        "Head.txt", "MRC.txt", "DRIs_and_FlexRamps.txt", "FlexFly.txt",
        "DiagonalIntersections.txt", "Special.txt", "Archaic.txt")
      += mkRul(0x15, "MonorailBasic", "RUL15_Monorail_Basic.rul") // to be removed
      += mkRul(0x16, "MonorailAdvanced", "RUL16_Monorail_Advanced.rul") // to be removed
      += mkRul(0x17, "GroundhighwayBasic", "RUL17_GHWY_Basic.rul")
      += mkRul(0x18, "GroundhighwayAdvanced", "RUL18_GHWY_Advanced.rul")
    ).result
    DbpfFile.write(allRuls, new File(targetDir, "All_INRULs.dat"))

    // the following section is work-in-progress for the specialized INRUL files
    // of individual NAM plugins – to be done.

//    // FANs and WRCs
//    val fanRuls = (Seq.newBuilder[DbpfEntry]
//      += mkRul(0x08, "RoadAdvanced", "Head.txt", "DiagonalIntersections.txt", "General.txt",
//        "FAR3.txt", "FAR2.txt")
//      += mkRul(0x0A, "StreetAdvanced", "Head.txt", "WideRadiusCurves.txt")
//    ).result
//    DbpfFile.write(fanRuls, new File(targetDir, "NetworkAddonMod_FAN_WRC_INRULs.dat"))
//
//    // diagonal streets
//    val diagStRuls = Seq(mkRul(0x09, "StreetBasic", "RUL09_Street_Diagonal_Streets.rul"))
//    DbpfFile.write(diagStRuls, new File(targetDir, "NetworkAddonMod_DiagStreets_INRULs.dat"))
//
//    // roundabouts
//    val raRuls = (Seq.newBuilder[DbpfEntry]
//      += mkRul(0x0A, "StreetAdvanced", "Head.txt", "Roundabouts.txt")
//    ).result
//    DbpfFile.write(raRuls, new File(targetDir, "NetworkAddonMod_Roundabouts_INRULs.dat"))
  }

  // implementation details below

  private def filesToArray(files: Seq[File]): Array[Byte] = files.flatMap { f =>
    managed(new ByteInput(new FileInputStream(f))) acquireAndGet (_.slurp[Byte])
  } (collection.breakOut)

  private def buildRul(id: Int, files: Seq[File]): DbpfEntry = {
    val tgi = Tgi(0,0,id).copy(Tgi.Rul)
    val arr = filesToArray(files)
    BufferedEntry(tgi, RawType(arr), compressed = true)
  }

}
