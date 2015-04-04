package metarules
package module.flexfly

import java.io.File
import scdbpf._, DbpfUtil.RotFlip._, S3d._, Sc4Path._
import rapture.core.strategy.throwExceptions
import meta._, Flags._, Implicits._, Network._
import FlexFlyTiles._, FlexFlyRuleGenerator._
import module.NetworkProperties._

/** Compiles all crossing models of FlexFly from the base curve models and the
  * orthogonal RHW models (L0 and L1), same for the paths. It also generates
  * appropriate exemplar files and preview effect directory files from scratch.
  *
  * This script expects to find the L0 and L1 models and paths in the locations
  * 'src/main/resources/rhwResources.dat' (for orthogonal RHW) and
  * 'src/main/resources/flexFlyResources.dat' (for FlexFly base curves).
  * The generated output (including FlexFly base curves) will be written to
  * 'target/FlexFly.dat'.
  */
object CompileFlexFlyResources {

  private trait Rotatable[A] {
    def rotate(x: A, rf: RotFlip): A
    def append(x: A, y: A): A
    def mkTgi(id: Int): Tgi
    def shift(x: A, f: Float): A
    def prepare(x: A): A
  }
  implicit private object S3dIsRotatable extends Rotatable[S3d] {
    def rotate(x: S3d, rf: RotFlip) = x * rf
    def append(x: S3d, y: S3d) = (x ++ y).trim
    def mkTgi(id: Int) = Tgi.S3dMaxis.copy(iid = Some(id)).toTgi
    def shift(x: S3d, f: Float) = x.translate(Translation(0, f, 0))
    def prepare(x: S3d) = x.copy(mats = x.mats map { mg =>
      mg.copy(materials = mg.materials map { m =>
        // optimized mats settings for embedded mipmaps
        m.copy(magFilter = MagnifFilter.Bilinear, minFilter = MinifFilter.LinearMipmapLinear)
      })
    })
  }
  implicit private object Sc4PathIsRotatable extends Rotatable[Sc4Path] {
    def rotate(x: Sc4Path, rf: RotFlip) = x * rf
    def append(x: Sc4Path, y: Sc4Path) = x ++ y
    def mkTgi(id: Int) = Tgi.Sc4Path3d.copy(iid = Some(id)).toTgi
    def shift(x: Sc4Path, f: Float) = x.shiftHeight(f)
    def prepare(x: Sc4Path) = x
  }

  private def flexFlyTiles = for {
    orient <- orientations.iterator
    n <- (RhwNetworks from Mis to L4Rhw4).iterator
    t <- Seq(T0, T1, T2, T3, T4, T6)
  } yield (n, orient, t)

  /** the directions in wich a network can possibly cross one of the FlexFly tiles */
  private def crossingDirs(t: IntFlags, minor: Network) = {
    if (t == T0 || t == T1 || t == T2)
      Seq(NS, SN)
    else if (t == T4)
      directionsWithShoulderNorth(minor)
    else {
      assert(t == T3 || t == T6)
      NS +: SN +: directionsWithShoulderNorth(minor)
    }
  }

  def main(args: Array[String]): Unit = {
    val Seq(rhwFile, flexFlyFile) = Seq("/rhwResources.dat", "/flexFlyResources.dat") map {
      s => DbpfFile.read(new File(getClass.getResource(s).toURI))
    }
    val target = new File("target/FlexFly.dat")
    val resolve = new FlexFlyResolver

    def buildEntries[A <: DbpfType](implicit ev: Rotatable[A], conv: DbpfUtil.Converter[DbpfType, A]): Iterator[BufferedEntry[A]] = {
      import ev._

      val getBaseModel = {
        val m = collection.mutable.Map.empty[Int, A]
        def add(mkTile: Network => Segment, d: DbpfFile, n: Network) = {
          val id = resolve(mkTile(n)).id
          if (n.height <= 1) {
            val tgi = mkTgi(id)
            m(id) = prepare(d.tgiMap(tgi).toBufferedEntry.content.convert[A])
          } else {
            val id1 = resolve(mkTile(atHeight(ground(n), 1))).id
            m(id) = shift(m(id1), (n.height - 1) * 7.5f)
          }
        }
        for (n <- RhwNetworks filterNot deactivated) add(_~NS, rhwFile, n)
        for ((n, orient, t) <- flexFlyTiles) add(_~orient(t), flexFlyFile, n)
        m
      }

      val seen = collection.mutable.Set.empty[Tgi]
      val crossingModels = for {
        (main, orient, t) <- flexFlyTiles
        minor <- (RhwNetworks from L1Rhw2 filterNot deactivated).iterator
        if main.height != minor.height && !deactivated(minor)
        minDir <- crossingDirs(t, minor)
        id3 = resolve(main~orient(t) & minor~minDir)
        tgi = mkTgi(id3.id)
        if !seen(tgi)
      } yield {
        seen += tgi
        val id1 = resolve(main~orient(t))
        val model1 = rotate(getBaseModel(id1.id), id1.rf)
        val id2 = resolve(minor~minDir)
        val model2 = rotate(getBaseModel(id2.id), id2.rf)
        val model = rotate(append(model1, model2), R0F0 / id3.rf)
        BufferedEntry(tgi, model, compressed = true)
      }

      val baseCurveModels = for ((n, orient, t) <- flexFlyTiles) yield {
        val id = resolve(n~orient(t)).id
        BufferedEntry(mkTgi(id), getBaseModel(id), compressed = true)
      }
      baseCurveModels ++ crossingModels
    }

    val effdirs = for ((id, label) <- CompileFlexFlyRul0And1.previews(resolve)) yield {
      val effdir = Experimental.PreviewEffect(id, label)
      BufferedEntry(Tgi(0,0xEA5118B1,id).copy(Tgi.EffDir), effdir, compressed = true)
    }
    val modelsAndExemplars = buildEntries[S3d] flatMap { model =>
      import passera.unsigned._
      import DbpfProperty._
      val tgi = model.tgi
      val props = Seq(UInt(0x10) -> DbpfProperty(UInt(0x0B)), // type
        UInt(0x20) -> DbpfProperty("FlexFly"), // name
        UInt(0x27812820) -> DbpfProperty(Seq(tgi.tid.toUInt, tgi.gid.toUInt, tgi.iid.toUInt))) // RKT0
      val exemplar = Exemplar(props = props, isCohort = false)
      Seq(model, BufferedEntry(tgi.copy(Tgi.ExemplarDirtroad), exemplar, compressed = true))
    }
    val paths = buildEntries[Sc4Path]
    DbpfFile.write(modelsAndExemplars ++ paths ++ effdirs, target)
  }

}
