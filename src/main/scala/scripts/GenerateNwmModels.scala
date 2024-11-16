package com.sc4nam.scripts

import io.github.memo33.metarules, metarules._
import io.github.memo33.scdbpf
import com.sc4nam.module.syntax, syntax._, Implicits._, Network._

import scdbpf._, S3d._
import scdbpf.compat.strategy.throwExceptions
import meta._, meta.Flags._

import com.sc4nam.module.NwmResolver

/** Run with `sbt "runMain com.sc4nam.scripts.GenerateNwmModels"`.
  *
  * Creates S3D models for DxD NWM intersections that have large overhangs and
  * are raised at the center of the intersection.
  */
object GenerateNwmModels {

  val bites = Map(
    Tla3 -> 0x51010209,
    Ave2 -> 0x51010209,
    Ard3 -> 0x51010209,
    Owr3 -> 0x51040209,
    Nrd4 -> 0x5105020f) // or 0x51050209 ---> requires different u/v

  def main(args: Array[String]): Unit = {

    val resolver = new NwmResolver()

    def mkModel(majorSeg: Segment, minorSeg: Segment): BufferedEntry[S3d] = {
      // major~ES & minor~SW  where minor <= major

      val major = majorSeg.network
      val minor = minorSeg.network
      assert(minor <= major)
      val modelId = {
        val tile = resolver(majorSeg & minorSeg)
        assert(tile.rf == RotFlip.R0F0)
        tile.id
      }
      val darkTextureId = modelId & 0xFFFFFFF0 | ((modelId & 0xF) match {
        case 0 => 0x0e
        case 9 => 0x1e  // instead of 0x0f in order to allow 5 mipmaps at 0x1a-0x1e
      })

      // parameters
      val a = 5.00f  // raised overhang width
      val b = 1.75f  // slope length
      val lo = 0.1f
      val hi = 0.35f
      val stretch: Network => Float = n => if (n == Nrd4) 0.5f else 1.0f  // Nrd4 bites are 256 pixels wide

      val model = S3d(
        vert = IndexedSeq(
          VertGroup(IndexedSeq(  // left overhang
            Vert(-8-a-b, lo, 8-b,     0.25f-(a+b)/32, 1-b/16),
            Vert(-8-a-b, lo, 8,       0.25f-(a+b)/32, 1),
            Vert(-8-a,   hi, 8,       0.25f-a/32,     1),
            Vert(-8,     lo, 8-2*b-a, 0.25f,          1-(2*b+a)/16),
            Vert(-8,     hi, 8-a,     0.25f,          1-a/16),
            Vert(-8,     hi, 8,       0.25f,          1))),
          VertGroup(IndexedSeq(  // intersection center
            Vert(-8,     lo, 8-2*b-a, 0.25f,          1-(2*b+a)/16),
            Vert(-8,     hi, 8-a,     0.25f,          1-a/16),
            Vert(-8,     hi, 8,       0.25f,          1),
            Vert(-8,     lo, -8,      0.25f,          0),
            Vert(-b,     lo, -8,      0.5f-b/32,      0),
            Vert(-b,     lo, -a-b,    0.5f-b/32,      0.5f-(a+b)/16),
            Vert(0,      hi, -a,      0.5f,           0.5f-a/16),
            Vert(b,      lo, -8,      0.5f+b/32,      0),
            Vert(b,      lo, -a-b,    0.5f+b/32,      0.5f-(a+b)/16),
            Vert(8,      lo, -8,      0.75f,          0),
            Vert(8,      lo, 8-2*b-a, 0.75f,          1-(2*b+a)/16),
            Vert(8,      hi, 8-a,     0.75f,          1-a/16),
            Vert(8,      hi, 8,       0.75f,          1))),
          VertGroup(IndexedSeq(  // right overhang
            Vert(8,      lo, 8-2*b-a, 0.75f,          1-(2*b+a)/16),
            Vert(8,      hi, 8-a,     0.75f,          1-a/16),
            Vert(8,      hi, 8,       0.75f,          1),
            Vert(8+a,    hi, 8,       0.75f+a/32,     1),
            Vert(8+a+b,  lo, 8-b,     0.75f+(a+b)/32, 1-b/16),
            Vert(8+a+b,  lo, 8,       0.75f+(a+b)/32, 1))),
          VertGroup(IndexedSeq(  // nwm bites top left (minor)
            Vert(-8, lo, -8,  1,                           1),
            Vert(-4, lo, -12, 1f - 0.25f * stretch(minor), 0.75f),
            Vert(0, lo, -8,   1f - 0.5f * stretch(minor),  1))),
          VertGroup(IndexedSeq(  // nwm bites top right (major)
            Vert(0, lo, -8,  1f - 0.5f * stretch(major),  1),
            Vert(4, lo, -12, 1f - 0.25f * stretch(major), 0.75f),
            Vert(8, lo, -8,  1,                           1)))),
        indx = IndexedSeq(
          IndxGroup(IndexedSeq(
            0,1,2,
            0,2,3,
            2,4,3,
            2,5,4)),
          IndxGroup(IndexedSeq(
            1,11,6,   // hi
            1,2,11,   // hi
            2,12,11,  // hi
            0,1,5,    // slope
            1,6,5,    // slope
            5,6,7,    // slope
            7,6,10,   // slope
            6,11,10,  // slope
            0,5,3,    // lo
            3,5,4,    // lo
            4,5,7,    // lo
            5,8,7,    // lo
            7,8,9,    // lo
            8,10,9)), // lo
          IndxGroup(IndexedSeq(
            0,1,4,
            1,3,4,
            1,2,3,
            3,5,4)),
          IndxGroup(IndexedSeq(
            0,2,1)),
          IndxGroup(IndexedSeq(
            0,2,1))),
        prim = IndexedSeq(
          PrimGroup(IndexedSeq(Prim(PrimType.Triangle, 0, 12))),
          PrimGroup(IndexedSeq(Prim(PrimType.Triangle, 0, 42))),
          PrimGroup(IndexedSeq(Prim(PrimType.Triangle, 0, 12))),
          PrimGroup(IndexedSeq(Prim(PrimType.Triangle, 0, 3))),
          PrimGroup(IndexedSeq(Prim(PrimType.Triangle, 0, 3)))),
        mats = IndexedSeq(
          S3d.defaultMats(Transparency.Semitransparent, id = darkTextureId, name = Some("overhang-l")),
          S3d.defaultMats(Transparency.Semitransparent, id = darkTextureId, name = Some(s"nwm-dxd ${major} ${minor}")),
          S3d.defaultMats(Transparency.Semitransparent, id = darkTextureId, name = Some("overhang-r")),
          S3d.defaultMats(Transparency.Semitransparent, id = bites(minor), name = Some(s"nwm bites ${minor}")),
          S3d.defaultMats(Transparency.Semitransparent, id = bites(major), name = Some(s"nwm bites ${major}"))),
        anim = IndexedSeq(
          AnimGroup.vipm(0, 0, 0, 0, name = Some("overhang-l")),
          AnimGroup.vipm(1, 1, 1, 1, name = Some(s"nwm-dxd ${major} ${minor}")),
          AnimGroup.vipm(2, 2, 2, 2, name = Some("overhang-r")),
          AnimGroup.vipm(3, 3, 3, 3, name = Some(s"nwm bites ${minor}")),
          AnimGroup.vipm(4, 4, 4, 4, name = Some(s"nwm bites ${major}")))
      )
      BufferedEntry[S3d](content = model, tgi = Tgi(0, 0, modelId).copy(Tgi.S3dMaxis), compressed = true)
    }

    val entries = Seq(
      mkModel(Tla3~ES, Tla3~SW),

      mkModel(Ave2~ES, Tla3~SW),
      mkModel(Ave2~ES, Ave2~SW),

      mkModel(Ard3~ES, Tla3~SW),
      mkModel(Ard3~SE, Tla3~SW),
      mkModel(Ard3~ES, Ave2~SW),
      mkModel(Ard3~SE, Ave2~SW),
      mkModel(Ard3~ES, Ard3~SW),
      mkModel(Ard3~ES, Ard3~WS),
      mkModel(Ard3~SE, Ard3~WS),

      // Owr1 skipped as not model-based

      mkModel(Owr3~ES, Tla3~SW),
      mkModel(Owr3~ES, Ave2~SW),
      mkModel(Owr3~ES, Ard3~SW),
      mkModel(Owr3~ES, Ard3~WS),
      mkModel(Owr3~ES, Owr3~SW),

      mkModel(Nrd4~ES, Tla3~SW),
      mkModel(Nrd4~ES, Ave2~SW),
      mkModel(Nrd4~ES, Ard3~SW),
      mkModel(Nrd4~ES, Ard3~WS),
      mkModel(Nrd4~ES, Owr3~SW),
      mkModel(Nrd4~ES, Nrd4~SW)
    )
    DbpfFile.write(entries, new java.io.File("target/nwm-dxd-models.dat"))
  }
}
