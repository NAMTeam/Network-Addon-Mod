package com.sc4nam.scripts

import io.github.memo33.metarules, metarules._
import io.github.memo33.scdbpf
import com.sc4nam.module.syntax, syntax._, Implicits._, Network._

import io.github.memo33.passera.unsigned.{UInt, UByte, UIntOrdering}
import scdbpf._, DbpfProperty._, DbpfUtil.RotFlip._
import scdbpf.compat.strategy.throwExceptions
import meta._, meta.Flags._

import com.sc4nam.module.{MiscResolver, RhwResolver}
import com.sc4nam.module.flexfly.FlexFlyResolver
import com.sc4nam.module.RhwRuleGenerator.HeightLevel
import com.sc4nam.module.NetworkProperties.{ground, hasRightShoulder, hasLeftShoulder, isDoubleTile, isTripleTile}
import com.sc4nam.module.flexfly.FlexFlyTiles.{T0, T1, T2, T3, T4, T6}

/** Run with `sbt "runMain com.sc4nam.scripts.RhwT21"`. Note that the input file paths will need to be adjusted.
  *
  * This file documents how some of the T21 files for RHW were created, for future reference.
  */
object RhwT21 {

  lazy val namDir = new java.io.File(??? : String)  // Network Addon Mod install folder
  lazy val rhwDir = new java.io.File(namDir, "2 Network Features/RealHighway")

  lazy val inputDbpf = DbpfFile.read(new java.io.File(rhwDir, "RealHighway_Props.dat"))
  lazy val outputDir = new java.io.File("target")
  // lazy val inputMedianBarrierFile = DbpfFile.read(new java.io.File("projects/rhw-t21/RHW-central-support-barrier.dat"))
  lazy val rhwResourcesDbpf = DbpfFile.read(new java.io.File("src/main/resources/rhwResources.dat"))
  lazy val flexFlyTemplates = DbpfFile.read(new java.io.File("src/main/resources/flexfly-T21-templates.dat"))
  lazy val flexFlyFile = DbpfFile.read(new java.io.File(rhwDir, "RealHighway_FLEXFly.dat"))

  def main(args: Array[String]): Unit = {
    val resolver = new MiscResolver orElse new FlexFlyResolver
    val entries = Seq.newBuilder[DbpfEntry]
    val flexFlyEntries = Seq.newBuilder[DbpfEntry]
    val patches = collection.mutable.Map.empty[String, collection.mutable.Builder[DbpfEntry, Seq[DbpfEntry]]]

    // pillars for elevated onslope transitions
    for (rhw <- RhwNetworks - Rhw6cm if rhw.height == 0) {
      val maxHeight = if ((Mis + Rhw4 + Rhw6s).contains(rhw)) 4 else 2
      val minHeight = 1
      for {
        levelDiff <- Seq(1, 2)  // L1 vs L2 onslopes
        height <- minHeight to (maxHeight-levelDiff)
      } /*do*/ {
        val lower: Network = height~rhw
        val upper: Network = (height+levelDiff)~rhw
        val templateTile = resolver(upper~NS)  // using a plain orthogonal as template
        val templateTgi = Tgi.ExemplarT21.copy(iid = Some(templateTile.id)).toTgi
        val templateT21 = inputDbpf.tgiMap(templateTgi).toBufferedEntry.convertContentTo(Exemplar)
        val ex0 = templateT21.content
        val ex1 = translateT21Lot(ex0, 0, -0x34000*levelDiff - 0x04000, 0)  // half a height level 3.25 m and some tolerance of 0.25 m for slope tolerance
        val tile = resolver(upper~NC & lower~CS)
        val tgi = Tgi.ExemplarT21.copy(iid = Some(tile.id)).toTgi
        val ex = adjustT21PatternAndId(ex1, tgi.iid)
        val t21 = templateT21.copy(tgi = tgi, content = ex, compressed = true)
        entries += t21
      }
    }

    // pillars for FLEX-HT continuation tiles and start/end tiles
    for {
      rhw <- RhwNetworks - Rhw6cm if rhw.height == 0
      maxHeight = (if ((Mis + Rhw4 + Rhw6s).contains(rhw)) 4 else 2)
      h <- 1 to maxHeight
      (templateTileMeta, pieceId, updateModel) <- (
        (if (h < maxHeight) Seq((h~rhw~NS, (h+1)*0x100 + 0x40, false)) else Seq.empty)  // continuation tile, e.g. 0x5722440 for L2-L4 continuation tile for MIS (400 is upper end of L2-L3, 430 is lower end of L2-L3)
        ++
        Seq((h~rhw~NS, (h+1)*0x100 + 0x00, true))  // upper start of L1 HT
        ++
        (if (h > 1) Seq(((h-1)~rhw~NS, (h+1)*0x100 + 0x30, true)) else Seq.empty)  // lower end of L1 HT
      )
    } /*do*/ {
      val templateTile = resolver(templateTileMeta)  // using a plain orthogonal as template
      val templateTgi = Tgi.ExemplarT21.copy(iid = Some(templateTile.id)).toTgi
      val templateT21 = inputDbpf.tgiMap(templateTgi).toBufferedEntry.convertContentTo(Exemplar)
      val tileId = RhwResolver.rhwHtRangeId(rhw) + pieceId
      val tgi = Tgi.ExemplarT21.copy(iid = Some(tileId)).toTgi
      val ex = adjustT21PatternAndId(templateT21.content, tgi.iid)
      val t21 = templateT21.copy(tgi = tgi, content = ex, compressed = true)
      entries += t21

      if (updateModel && rhw != Rhw10c) {
        val modelTgi = Tgi.S3dMaxis.copy(iid = Some(tgi.iid)).toTgi
        val fName = rhw match {
          case Dirtroad => "RealHighway_RHW-2.dat"
          case Rhw3 => "RealHighway_RHW-3.dat"
          case Mis => "RealHighway_MIS.dat"
          case Rhw4 => "RealHighway_RHW-4.dat"
          case Rhw6s => "RealHighway_RHW-6S.dat"
          case n if isDoubleTile(n) => "RealHighway_Wide_S-Type.dat"
          case n if isTripleTile(n) => "RealHighway_Wide_C-Type.dat"
        }
        val dbpf = DbpfFile.read(new java.io.File(rhwDir, fName))
        val model = dbpf.tgiMap(modelTgi).toBufferedEntry.content.convertTo(S3d)
        val pillarVertsIndex = model.vert.indexWhere(vg => vg.verts.size == 24 && vg.verts.forall(v => (v.z == 1f || v.z == -1f)))
        require(pillarVertsIndex != -1, f"model does not contain pillar: ${rhw} ${modelTgi}")
        if (pillarVertsIndex != -1) {
          val model2 = model.copy(anim = model.anim.filter(ag => ag.vertBlock.forall(i => i != pillarVertsIndex))).trim  // remove pillar stub from models
          patches.getOrElseUpdate(fName, Seq.newBuilder[DbpfEntry]) += BufferedEntry(tgi = modelTgi, content = model2, compressed = true)
        }
      }
    }

    // val medianBarrierS3d =
    //   inputMedianBarrierFile
    //     .entries.filter(_.tgi.iid == 0x5d772304).head
    //     .toBufferedEntry.content.convertTo(S3d) * R3F0

    // pillars for RHW-6C medians
    for {
      rhw <- RhwNetworks - Rhw6cm - L1Rhw6cm - L2Rhw6cm - Rhw10c - L1Rhw10c - L2Rhw10c
      median <- Rhw6cm + L1Rhw6cm + L2Rhw6cm
      if rhw.height > median.height
    } /*do*/ {
      val templateTile = resolver(rhw~NS)  // using a plain orthogonal as template
      val templateTgi = Tgi.ExemplarT21.copy(iid = Some(templateTile.id)).toTgi
      val templateT21 = inputDbpf.tgiMap(templateTgi).toBufferedEntry.convertContentTo(Exemplar)
      val tile = resolver(rhw~NS & median~EW)
      val tgi = Tgi.ExemplarT21.copy(iid = Some(tile.id)).toTgi
      val ex = adjustT21PatternAndId(templateT21.content, tgi.iid)
      val t21 = templateT21.copy(tgi = tgi, content = ex, compressed = true)
      entries += t21

      // // generate new model with pillar barrier -> does not lead to ideal results, as the pillar placement varies quite a bit for different crossing networks
      // assert(rhw.height >= 1)
      // val templateS3dTgi = Tgi.S3dMaxis.copy(iid = Some(resolver((1~(ground(rhw)))~NS).id)).toTgi
      // val templateS3dL1 = rhwResourcesDbpf.tgiMap(templateS3dTgi).toBufferedEntry.content.convertTo(S3d)
      // val medianS3d =
      //   (templateS3dL1.translate(S3d.Translation(0, 7.5f*(rhw.height-1), 0))
      //    ++ (medianBarrierS3d * R1F0).translate(S3d.Translation(0, 7.5f*median.height, 0)))
      // entries += BufferedEntry(tgi = Tgi.S3dMaxis.copy(iid = Some(tgi.iid)).toTgi, content = medianS3d, compressed = true)
    }

    // FLEX-Fly tile 2 (pillar for crossing network)
    for {
      rhw <- RhwNetworks - Rhw6cm - L1Rhw6cm - L2Rhw6cm
      if rhw.height > 0
      ffNetwork <- RhwNetworks.rangeFrom(Mis).rangeTo(L4Rhw4)
      if ffNetwork.height != rhw.height
      ffReversed <- Seq(false, true)
      rhwReversed <- if (rhw.isSymm) Seq(false) else Seq(false, true)
    } /*do*/ {
      val templateTile = resolver(rhw~NS)  // using a plain orthogonal as template
      val templateTgi = Tgi.ExemplarT21.copy(iid = Some(templateTile.id)).toTgi
      val templateT21 = inputDbpf.tgiMap(templateTgi).toBufferedEntry.convertContentTo(Exemplar)
      val ex0 = templateT21.content
      val ex1 = rotateT21Lot(ex0, if (rhwReversed) R2F0 else R0F0)
      val yOffset = (0  // offset in meters depending on widths of the networks
        + (if (ground(ffNetwork) == Rhw4) 1 else 0)
        + (if ((Rhw6s + Rhw3 + Rhw8c + Rhw10c + Rhw12s).contains(ground(rhw))) 1 else 0)
        + (if (ground(rhw) == Rhw6s && !rhwReversed || ground(rhw) == Rhw10c) 1 else 0)
        )
      val ex2 = translateT21Lot(ex1, 0, 0, if (rhw.height > ffNetwork.height) 0x10000 * yOffset else 0)  // offset to avoid clipping into the FLEX-Fly deck

      val ffSeg = if (ffReversed) (ffNetwork~T2).reverse else ffNetwork~T2
      val rhwSeg = if (rhwReversed) rhw~SN else rhw~NS
      val tile = resolver(ffSeg & rhwSeg)
      assert(tile.rf == meta.RotFlip.R0F0)
      val tgi = Tgi.ExemplarT21.copy(iid = Some(tile.id)).toTgi
      val ex = adjustT21PatternAndId(ex2, tgi.iid)
      val t21 = templateT21.copy(tgi = tgi, content = ex, compressed = true)
      flexFlyEntries += t21
    }

    // FLEX-Fly tile 1, 3 and 4 base (pillar for FLEX-Fly network) as well as
    // tile 4 and 5 crossings (with pillar on FLEX-Fly network)
    for {
      ffNetwork <- RhwNetworks.rangeFrom(Mis).rangeTo(L4Rhw4)
      if ffNetwork.height > 0
      ffReversed <- Seq(false, true)
      flags <- Seq(T1, T3, T4)
      ffSeg = if (ffReversed) (ffNetwork~flags).reverse else ffNetwork~flags
      (ffTileMeta, templateId) <- flags match {
        case T1 => Seq((ffSeg: Tile, 0x5ca5c144))  // L4Mis~T1
        case T3 => (
          Seq((ffSeg: Tile, 0x5ca5c344))  // L4Mis~T3
          ++  // T3: in addition to base tile, we add an orthogonal "central" pillar for Rhw4~NS, as in this combination it seems likely that there is Rhw~SN on T4
          (for (h <- 0 to 4 if h < ffNetwork.height)
           yield (ffSeg & (h~Rhw4)~NS, 0x5CA5C302))  // L4Mis~T3 & Rhw4~NS
          ++  // T5: in addition to base tile, we add pillar to crossing tiles for networks that have enough space on shoulder
          (for (rhw <- Seq(Rhw6c, Rhw8s, Rhw10s); h <- 0 to 2 if h < ffNetwork.height)
           yield (ffSeg*R3F1 & (h~rhw)~SN, 0x5CA5C562))  // L4Mis~T5 & L1Rhw6c~SN
          ++  // T5: in following template, pillar has more offset to reduce deck clipping
          (for (h <- 0 to 4; dir <- Seq(NS, SN) if h < ffNetwork.height)
           yield (ffSeg*R3F1 & (h~Mis)~dir, 0x5CA5C511))  // L4Mis~T5 & L1Mis~SN
          ++  // T5: additionally we can add FLEX-Fly pillar whenever crossing network is higher
          (for {
            rhw <- (RhwNetworks - Dirtroad).iterator if rhw.height > ffNetwork.height
            dir <- (if (hasRightShoulder(rhw)) Seq(SN) else Seq.empty) ++ (if (hasLeftShoulder(rhw) && !rhw.isSymm) Seq(NS) else Seq.empty)
          } yield (ffSeg*R3F1 & rhw~dir, 0x5CA5C562))
        )
        case T4 => (
          Seq((ffSeg: Tile, 0x5CA5C444))  // L4Mis~T4
          ++  // additionally add FLEX-Fly pillar if crossing network is higher
          (for {
            rhw <- (RhwNetworks - Dirtroad).iterator if rhw.height > ffNetwork.height
            dir <- (if (hasRightShoulder(rhw)) Seq(SN) else Seq.empty) ++ (if (hasLeftShoulder(rhw) && !rhw.isSymm) Seq(NS) else Seq.empty)
          } yield (ffSeg & rhw~dir, 0x5CA5C444))  // L4Mis~T4
        )
        case _ => ???
      }
    } /*do*/ {
      val templateTgi = Tgi.ExemplarT21.copy(iid = Some(templateId)).toTgi
      val templateT21 = flexFlyTemplates.tgiMap(templateTgi).toBufferedEntry.convertContentTo(Exemplar)
      val ex0 = templateT21.content
      val ex1 = translateT21Lot(ex0, 0, -0x78000 * (4 - ffNetwork.height), 0)  // template was L4, so we need to lower props

      val tile = resolver(ffTileMeta)
      assert(tile.rf == meta.RotFlip.R0F0)
      val tgi = Tgi.ExemplarT21.copy(iid = Some(tile.id)).toTgi
      val ex = adjustT21PatternAndId(ex1, tgi.iid)
      val t21 = templateT21.copy(tgi = tgi, content = ex, compressed = true)
      flexFlyEntries += t21
    }

    // FLEX-Fly central pillar for (elevated) Rhw6cm
    val T5 = (0,+241,0,-231); val T7 = (+3,0,0,-241)
    for {
      rhw <- Seq(Rhw6cm, L1Rhw6cm, L2Rhw6cm)
      ffNetwork <- RhwNetworks.rangeFrom(Mis).rangeTo(L4Rhw4)
      if ffNetwork.height > rhw.height
      ffReversed <- Seq(false, true)
      flags <- Seq(T0, T1, T2, T3, T6)  // T4, T5, T7 are physically not possible
      if rhw.height > 0 || flags == T0  // T0 does not exist at L0 yet
    } /*do*/ {
      val levelDiff = ffNetwork.height - rhw.height
      val templateTile = resolver(levelDiff~Mis~flags & Rhw6cm~NS)
      val tile = resolver((if (ffReversed) (ffNetwork~flags).reverse else ffNetwork~flags) & rhw~NS)
      assert(tile.rf == meta.RotFlip.R0F0 && templateTile.rf == meta.RotFlip.R0F0)

      val templateTgi = Tgi.ExemplarT21.copy(iid = Some(templateTile.id)).toTgi
      val templateT21 = (if (flags==T0) flexFlyTemplates else flexFlyFile).tgiMap(templateTgi).toBufferedEntry.convertContentTo(Exemplar)
      val ex0 = templateT21.content
      val ex1 = translateT21Lot(ex0, 0, 0x78000 * rhw.height, 0)  // template had Rhw6cm at L0

      val tgi = Tgi.ExemplarT21.copy(iid = Some(tile.id)).toTgi
      val ex = adjustT21PatternAndId(ex1, tgi.iid)
      val t21 = templateT21.copy(tgi = tgi, content = ex, compressed = true)
      flexFlyEntries += t21
    }

    // pillars for R1 curves of single-tile networks
    for {
      rhw <- Seq(Mis, Rhw4, Rhw6s, Dirtroad, Rhw3)
      h <- 1 to (if ((Mis + Rhw4 + Rhw6s).contains(rhw)) 4 else 2)
      flags <- Seq((2,0,-123,0), (123,0,0,-111)) ++ (if (rhw == Rhw6s) Seq((0,111,-3,0)) else Seq.empty)
      reversed <- if (rhw.isSymm) Seq(false) else Seq(false, true)
    } /*do*/ {
      val templateSeg = (if (rhw == Rhw6s) L4Rhw6s else L4Mis)~flags  // pillar placement differs for Rhw6s due to width
      val templateTile = resolver(if (reversed) templateSeg.reverse else templateSeg)
      val tile = resolver(if (reversed) (h~rhw~flags).reverse else h~rhw~flags)
      assert(tile.rf == meta.RotFlip.R0F0 && templateTile.rf == meta.RotFlip.R0F0)

      val templateTgi = Tgi.ExemplarT21.copy(iid = Some(templateTile.id)).toTgi
      val templateT21 = flexFlyTemplates.tgiMap(templateTgi).toBufferedEntry.convertContentTo(Exemplar)
      val ex0 = templateT21.content
      val ex1 = translateT21Lot(ex0, 0, -0x78000 * (4 - h), 0)  // template was L4, so we need to lower props

      val tgi = Tgi.ExemplarT21.copy(iid = Some(tile.id)).toTgi
      val ex = adjustT21PatternAndId(ex1, tgi.iid)
      val t21 = templateT21.copy(tgi = tgi, content = ex, compressed = true)
      entries += t21
    }

    DbpfFile.write(entries.result(), new java.io.File(outputDir, "z1-T21-patch.RealHighway_Props.dat"))
    DbpfFile.write(flexFlyEntries.result(), new java.io.File(outputDir, "z1-T21-patch.RealHighway_FLEXFly.dat"))
    patches.foreach { case (fName, patchEntries) =>
      DbpfFile.write(patchEntries.result(), new java.io.File(outputDir, f"z1-model-patch.${fName}"))
    }
  }

  val LotObjectPropMin = UInt(0x88EDC900)
  val LotObjectPropMax = LotObjectPropMin + UInt(1279)

  private class Lot(width: Int, height: Int) {
    require(width > 0 && height > 0)
    private val xMax = UInt(width * UInt(0x100000) - 1)
    private val yMax = UInt(height * UInt(0x100000) - 1)

    def rotateLotObject(p: Multi[UInt], rf: RotFlip): Multi[UInt] = {
      if (p.values.length < 12) {
        throw new IllegalArgumentException(s"Property has too few reps for being a lot object: $p")
      } else {
        p(0) match {
          case UInt(2) => p  // textures (not supported on T21, TODO handle these properly)
          case UInt(0) | UInt(1) | UInt(4) => // building, prop or flora
            rf match {
              case RotFlip.R0F0 => p
              case RotFlip.R2F0 =>
                Multi[UInt](
                  IndexedSeq(p(0), p(1), UInt((p(2)+rf.rot) % 4),
                    xMax - p(3),
                    p(4),
                    yMax - p(5),
                    xMax - p(8),
                    yMax - p(9),
                    xMax - p(6),
                    yMax - p(7)
                  ) ++ p.values.drop(10)
                )
              case RotFlip.R1F0 => ???  // R1F0 not handled
              case RotFlip.R3F0 => ???  // R3F0 not handled
              case _ => ???  // mirroring not handled
            }
          case objType => throw new NotImplementedError(s"Rotating lot object type $objType not implemented.")  // lot object type not implemented
        }
      }
    }

    // z is height
    def translateLotObject(p: Multi[UInt], x: Int, z: Int, y: Int): Multi[UInt] = {
      if (p.values.length < 12) {
        throw new IllegalArgumentException(s"Property has too few reps for being a lot object: $p")
      } else {
        p(0) match {
          case UInt(2) => p  // textures (not supported on T21, TODO handle these properly)
          case UInt(0) | UInt(1) | UInt(4) => // building, prop or flora
            val x2 = p(3) + UInt(x)
            val y2 = p(5) + UInt(y)
            require(x2 <= xMax && y2 <= yMax, "prop center must be within lot bounds")
            Multi[UInt](
              IndexedSeq(p(0), p(1), p(2),
                x2,
                p(4) + UInt(z),
                y2,
                p(6) + UInt(x),  // bounding box min  (TODO clip to lot bounds?)
                p(7) + UInt(y),  // bounding box min
                p(8) + UInt(x),  // bounding box max
                p(9) + UInt(y),  // bounding box max
              ) ++ p.values.drop(10)
            )
          case objType => throw new NotImplementedError(s"Offsetting lot object type $objType not implemented.")
        }
      }
    }
  }

  /** rotates the lot objects of a T21 exemplar (only 180 degree is supported). */
  def rotateT21Lot(exemplar: Exemplar, rf: RotFlip): Exemplar = {
    val lot = new Lot(1, 1)
    val propsModified =
      exemplar.properties.range(from = LotObjectPropMin, until = LotObjectPropMax + UInt(1)).view.mapValues {
        case DbpfProperty.ValueType.Uint32(p: Multi[UInt]) => lot.rotateLotObject(p, rf)
        case _ => ???
      }
    exemplar.copy(properties = exemplar.properties ++ propsModified)
  }

  // z is height
  def translateT21Lot(exemplar: Exemplar, x: Int, z: Int, y: Int): Exemplar = {
    val lot = new Lot(1, 1)
    val propsModified =
      exemplar.properties.range(from = LotObjectPropMin, until = LotObjectPropMax + UInt(1)).view.mapValues {
        case DbpfProperty.ValueType.Uint32(p: Multi[UInt]) => lot.translateLotObject(p, x, z, y)
        case _ => ???
      }
    exemplar.copy(properties = exemplar.properties ++ propsModified)
  }


  def adjustT21PatternAndId(exemplar: Exemplar, id: Int): Exemplar = {
    exemplar.copy(properties = exemplar.properties
      + (UInt(0x00000021) -> DbpfProperty(UInt(id)))  // exemplar id
      + (UInt(0xC9A5A1BE) -> DbpfProperty(UInt(id)))  // network tile id
      + (UInt(0x49D55951) -> DbpfProperty(Seq.fill(4)(UByte(0xf))))  // placement pattern = every tile
      + (UInt(0xEC3BD470) -> DbpfProperty(UByte(0xf)))  // rotations = all
      )
  }
}
