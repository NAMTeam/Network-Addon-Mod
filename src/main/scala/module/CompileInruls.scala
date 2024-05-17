package com.sc4nam.module

import java.io.{File, FileInputStream}
import resource._
import io.github.memo33.scdbpf, scdbpf._, strategy.throwExceptions

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
    // helper function for single RUL files
    def write(target: String, id: Int, dir: String, fileNames: String*): Unit = {
      val rul = Seq(mkRul(id, dir, fileNames: _*))
      DbpfFile.write(rul, new File(targetDir, target))
    }

    // main INRUL file
    val mainINRULs = (Seq.newBuilder[DbpfEntry]
      += mkRul(0x01, "HighwayBasic", "RUL01_EMHWY_Basic.rul")
      += mkRul(0x02, "HighwayAdvanced", "RUL02_EMHWY_Advanced.rul")
      //+= mkRul(0x03, "SubwayBasic", "RUL03_Subway_Basic.rul")
      //+= mkRul(0x04, "SubwayAdvanced", "RUL04_Subway_Advanced.rul")
      += mkRul(0x05, "RailBasic", "RUL05_Rail_Basic.rul")
      += mkRul(0x06, "RailAdvanced", "RUL06_Rail_Advanced.rul")
      += mkRul(0x07, "RoadBasic", "RUL07_Road_Basic.rul")
      //+= mkRul(0x0B, "PipeBasic", "RUL0B_Pipe_Basic.rul")
      //+= mkRul(0x0C, "PipeAdvanced", "RUL0C_Pipe_Advanced.rul")
      += mkRul(0x0D, "AvenueBasic", "RUL0D_Avenue_Basic.rul")
      += mkRul(0x0E, "AvenueAdvanced", "RUL0E_Avenue_Advanced.rul")
      += mkRul(0x11, "OnewayroadBasic", "Head.txt", "General.txt")
      += mkRul(0x13, "DirtroadBasic", "RUL13_RHW_Basic.rul")
      += mkRul(0x14, "DirtroadAdvanced",
        "Head.txt", "MRC.txt", "DRIs_and_FlexRamps.txt", "FlexFly.txt",
        "DiagonalIntersections.txt", "Special.txt", "Archaic.txt")
      += mkRul(0x17, "GroundhighwayBasic", "RUL17_GHWY_Basic.rul")
      += mkRul(0x18, "GroundhighwayAdvanced", "RUL18_GHWY_Advanced.rul")
    ).result()
    DbpfFile.write(mainINRULs, new File(targetDir, "NetworkAddonMod_IndividualNetworkRULs.dat"))

    // diagonal streets
    write(target = "NetworkAddonMod_Diagonal_Streets_Plugin_INRULs.dat", 0x09, dir = "StreetBasic",
      "RUL09_Street_Diagonal_Streets.rul")

    // GLR
    write(target = "NetworkAddonMod_GroundLightRail_Plugin_INRULs.dat", 0x10, dir = "LightrailAdvanced",
      "RUL10_LightRail_Advanced.rul")

    // OWR arrow reduction
    write(target = "NetworkAddonMod_OneWayRoad_Arrows_Reduction_Plugin_INRULs.dat", 0x11, dir = "OnewayroadBasic",
      "Head.txt", "ArrowReduction.txt", "General.txt")

    // OWR roundabouts
    write(target = "NetworkAddonMod_Roundabouts_OneWayRoads_Plugin_INRULs.dat", 0x12, dir = "OnewayroadAdvanced",
      "RUL12_OWR_Advanced.rul")

    // road roundabouts
    write(target = "NetworkAddonMod_Roundabouts_Roads_Plugin_INRULs.dat", 0x08, dir = "RoadAdvanced",
      "Head.txt", "DiagonalIntersections.txt", "Roundabouts.txt", "General.txt")

    // road roundabouts x FAR
    write(target = "NetworkAddonMod_Roundabouts_Roads_x_draggable_FANs.dat", 0x08, dir = "RoadAdvanced",
      "Head.txt", "DiagonalIntersections.txt", "Roundabouts.txt", "General.txt", "FAR3.txt", "FAR2.txt")

    // street roundabouts
    write(target = "NetworkAddonMod_Roundabouts_Streets_Plugin_INRULs.dat", 0x0A, dir = "StreetAdvanced",
      "Head.txt", "Roundabouts.txt")

    // street roundabouts x FAS
    write(target = "NetworkAddonMod_Roundabouts_Streets_x_draggable_FANs.dat", 0x0A, dir = "StreetAdvanced",
      "Head.txt", "Roundabouts.txt", "WideRadiusCurves.txt")

    // avenue turning lanes
    write(target = "NetworkAddonMod_TurningLanes_Avenues_Plugin_INRULs.dat", 0x0E, dir = "AvenueAdvanced",
      "RUL0E_Avenue_TurningLanes.rul")

    // draggable FAN and WRC
    val fanRuls = (Seq.newBuilder[DbpfEntry]
      += mkRul(0x08, "RoadAdvanced", "Head.txt", "DiagonalIntersections.txt", "General.txt",
        "FAR3.txt", "FAR2.txt")
      += mkRul(0x0A, "StreetAdvanced", "Head.txt", "WideRadiusCurves.txt")
    ).result()
    DbpfFile.write(fanRuls, new File(targetDir, "NetworkAddonMod_FAN_WRC_INRULs.dat"))

    // RRW
    val rrwRuls = (Seq.newBuilder[DbpfEntry]
      += mkRul(0x05, "RailBasic", "RUL05_Rail_Basic-RRW.rul")
      += mkRul(0x06, "RailAdvanced", "RUL06_Rail_Advanced-RRW.rul")
    ).result()
    DbpfFile.write(rrwRuls, new File(targetDir, "RealRailway_Core_INRULs.dat"))
  }

  // implementation details below

  private def filesToArray(files: Seq[File]): Array[Byte] = files.toArray.flatMap { f =>
    managed(new scdbpf.compat.ByteInput(new FileInputStream(f))) acquireAndGet (scdbpf.compat.Input.slurpBytes(_))
  }

  private def buildRul(id: Int, files: Seq[File]): DbpfEntry = {
    val tgi = Tgi(0,0,id).copy(Tgi.Rul)
    val arr = filesToArray(files)
    BufferedEntry(tgi, RawType(arr), compressed = true)
  }

}
