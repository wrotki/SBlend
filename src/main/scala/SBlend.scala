import java.io.{FileOutputStream, File, FileInputStream}
import java.nio.charset.Charset

import blender.BlenderCodecs.{FileBlock, Blend}
import blender.SDNA.BlenderCodecs.{Field, Structure, StructureDNA}
import blender._
import playground.{UnderscoreEx, ReaderTOptionExample}
import scodec.bits.{ByteVector, BitVector}
import scodec._
import shapeless._
import codecs._
import scalaz.Tree.Node
import scalaz._
import Scalaz._

import scalaz.Show

/**
  * Created by mariusz on 4/19/15.
  * http://opengameart.org/forumtopic/blender-file-format
  * https://github.com/scodec/scodec
  * http://scodec.org/guide/scodec-core.html
  * https://github.com/scodec/scodec/blob/master/src/test/scala/scodec/examples/PcapExample.scala
  * http://www.atmind.nl/blender/mystery_ot_blend.html
  * https://github.com/ldo/blendhack/blob/master/blendfile.py
  * http://mpilquist.github.io/blog/2013/06/09/scodec-part-3/
  */

/*
val path = "/Users/mariusz/projects/SBlend/resources/zombie.blend"
val args = Array(path)
import blender.BlenderCodecs.Blend
import blender.SDNA.BlenderCodecs.StructureDNA
val (blend: Blend, sdnaDecoded: StructureDNA) = SBlend.getSDNA(args)
import blender.TypeResolver
val tr = new TypeResolver
val typeMap = tr.createStructMap(sdnaDecoded)
typeMap foreach { case (k,v) => println(s"Key: $k") }

 */

object SBlend extends App {

  sealed trait ByteOrdering

  case object BigEndian extends ByteOrdering

  case object LittleEndian extends ByteOrdering

  case class BlenderHeader(ordering: ByteOrdering)


  override def main(args: Array[String]): Unit = {

    //UnderscoreEx.runex

    /*
    Uncomment below to get back toBlender conversion
     */
    loadBlendFile(args)
  }

  def loadBlendFile(args: Array[String]): Unit = {
    //    val h: ::[Int, ::[String, ::[(String, Int), ::[Int, HNil]]]] = 1 :: "foo" :: ("bar",2) :: 3 :: HNil
    //    val h0 = h(0)
    //    val h1 = h(1)
    //    val h2 = h(2)
    //    println(h(2))

    val (blend: Blend, sdnaDecoded: StructureDNA) = getSDNA(args)

    import blender.TypeResolver

    val typeMap = TypeResolver.createStructMap(sdnaDecoded)
    typeMap filterKeys {
      _ == "ID"
      //_ == "Scene"
    } foreach { case (k, v) => println(s"Type: $k Fields: $v") }
    // ----------------

   // println(TypeResolver.fieldLength("", "id", "ID", typeMap))
    val lengths = TypeResolver.fieldLength("", "sc", "Scene", typeMap)
    println(lengths)

//    def offsets(input: Seq[Int]): Seq(Int) = {
//
//    }
    val offsets = (lengths map { _.length }).scanLeft(0)(_ + _)
    println(offsets)

    //    println(sdnaDecoded)

    //val st: Structure = sdnaDecoded.structureTypes(0)

    //    sdnaDecoded.structureTypes foreach { st =>
    //      val std = (sdnaDecoded.names(st.name), st.fields map { f => (sdnaDecoded.names(f.fieldName), sdnaDecoded.types(f.fieldType)) })
    //      println(std)
    //    }
    //    printScene(blend, typeMap)
    //    println("----------------------")
    //
    //    import blender.Show._
    //    println(SceneTree.typeTree.drawTree)
  }

  private def printScene(blend: Blend, typeMap: Map[String,Type]): Unit = {
    //    val sceneType = sdnaDecoded.structureTypes(scene.header.sdnaIndex)
    //    val sceneTypeName = sdnaDecoded.types(sceneType.name)
    //    val len = sdnaDecoded.lenghts(sceneType.name)
    //
    //    println(s"SceneType: $sceneTypeName" + s" length: $len")
    //
    //    sceneType.fields foreach {
    //      f => println("Field(" + sdnaDecoded.names(f.fieldName) + ":" + sdnaDecoded.types(f.fieldType) + ") length: " + sdnaDecoded.lenghts(f.fieldType))
    //    }
    //
    //    println("----------------------")
    //    val typeTable = sceneType.fields map {
    //      f =>
    //        TypeProperties(
    //          sdnaDecoded.names(f.fieldName),
    //          sdnaDecoded.types(f.fieldType),
    //          sdnaDecoded.lenghts(f.fieldType),
    //          0
    //        )
    //    }
    //    println(typeTable)
    //    println("----------------------")
    //
    //    //printStructure(sdnaDecoded,"ID")
    //
    //    println(blender.Field(sdnaDecoded, "scene", "Scene"))
    //    println("----------------------")



    val scene: FileBlock = blend.records filter {
      _.header.code.startsWith("SC")
    } head
    val sceneBytes: ByteVector = scene.data.data
    val fo = new FileOutputStream(new File("./scene"))
    val sdnaReadable = sceneBytes.copyToStream(fo)

    //val b1 = blend flatMap { println }
    //println(scene)

    println("----------------------")
    println("----------------------")
  }

  def getSDNA(args: Array[String]): (Blend, StructureDNA) = {
    val parser = new Parser
    val blend = parser.Parse(args(0)).require.value

    println(blend.fileHeader)

    //blend.records filter {fb: FileBlock => fb.header.code != "DATA"} foreach { fb: FileBlock => println(fb.header.code)}

    val sdna: FileBlock = blend.records filter {
      _.header.code == "DNA1"
    } head


    val sdnaBytes: ByteVector = sdna.data.data
    //    val fo = new FileOutputStream(new File("./sdna"))
    //    val sdnaReadable = sdnaBytes.copyToStream(fo)
    //
    val bits = sdnaBytes.bits
    //    println(bits)

    val sdnaDecoded = Codec.decode[StructureDNA](bits).require.value
    (blend, sdnaDecoded)
  }

  def printStructure(sdna: StructureDNA, typeName: String): Unit = {
    val typeID = sdna.types.indexOf(typeName)
    val struct = sdna.structureTypes filter {
      _.name == typeID
    } head

    val len = sdna.lenghts(typeID)
    println(s"StructType: $typeName" + s" length: $len")

    struct.fields foreach {
      f => println("Field(" + sdna.names(f.fieldName) +
        ":" + sdna.types(f.fieldType) +
        ") length: " + sdna.lenghts(f.fieldType))
    }

    println("----------------------")
  }



}
