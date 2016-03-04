import java.io.{FileOutputStream, File, FileInputStream}
import java.nio.charset.Charset

import blender.BlenderCodecs.{FileBlock, Blend}
import blender.SDNA.BlenderCodecs.{Field, Structure, StructureDNA}
import blender._
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


object SBlend extends App {

  sealed trait ByteOrdering

  case object BigEndian extends ByteOrdering

  case object LittleEndian extends ByteOrdering

  case class BlenderHeader(ordering: ByteOrdering)


  override def main(args: Array[String]): Unit = {

    //    val h: ::[Int, ::[String, ::[(String, Int), ::[Int, HNil]]]] = 1 :: "foo" :: ("bar",2) :: 3 :: HNil
    //    val h0 = h(0)
    //    val h1 = h(1)
    //    val h2 = h(2)
    //    println(h(2))

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
    println(sdnaDecoded)

    //val st: Structure = sdnaDecoded.structureTypes(0)

    sdnaDecoded.structureTypes foreach { st =>
      val std = (sdnaDecoded.names(st.name), st.fields map { f => (sdnaDecoded.names(f.fieldName), sdnaDecoded.types(f.fieldType)) })
      println(std)
    }

    val scene: FileBlock = blend.records filter {
      _.header.code.startsWith("SC")
    } head
    val sceneBytes: ByteVector = scene.data.data
    val fo = new FileOutputStream(new File("./scene"))
    val sdnaReadable = sceneBytes.copyToStream(fo)

    //val b1 = blend flatMap { println }
    println(scene)

    println("----------------------")
    println("----------------------")
    println("----------------------")

    val sceneType = sdnaDecoded.structureTypes(scene.header.sdnaIndex)
    val sceneTypeName = sdnaDecoded.types(sceneType.name)
    val len = sdnaDecoded.lenghts(sceneType.name)

    println(s"SceneType: $sceneTypeName" + s" length: $len")

    sceneType.fields foreach {
      f => println("Field(" + sdnaDecoded.names(f.fieldName) + ":" + sdnaDecoded.types(f.fieldType) + ") length: " + sdnaDecoded.lenghts(f.fieldType))
    }

    println("----------------------")
    val typeTable = sceneType.fields map {
      f =>
        TypeProperties(
          sdnaDecoded.names(f.fieldName),
          sdnaDecoded.types(f.fieldType),
          sdnaDecoded.lenghts(f.fieldType),
          0
        )
    }
    println(typeTable)
    println("----------------------")

    //printStructure(sdnaDecoded,"ID")

    println(transformStructure(sdnaDecoded, "scene", "Scene"))
    println("----------------------")

    import blender.Show._
    println(SceneTree.typeTree.drawTree)

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

  def transformStructure(sdna: StructureDNA, fieldName: String, typeName: String): Tree[TypeProperties] = {
    if (typeName == "void") {
      return TypeProperties(fieldName,"void",0,0).leaf
    }
    val typeID = sdna.types.indexOf(typeName)

    val struct: Option[Structure] = sdna.structureTypes filter {
      _.name == typeID
    } headOption

    if (struct.isEmpty) {
      return TypeProperties(fieldName,typeName,0,0).leaf
    }
    val subForest = struct.get.fields map {
      f => transformStructure(sdna,sdna.names(f.fieldName), sdna.types(f.fieldType))
    } toStream
    val root =
    Tree.Node(
      TypeProperties(fieldName,typeName,0,0),
      subForest
    )
    root
    //    val subFields = struct map { s =>
    //      s.fields map {
    //        f => FileBlockField(sdna.names(f.fieldName),sdna.types(f.fieldType),sdna.lenghts(f.fieldType),0,
    //          if ( sdna.names(f.fieldName).startsWith("*")) { Seq() } else { transformStructure(sdna,sdna.types(f.fieldType))} )
    //      }
    //    }
    //sdna.lenghts(typeID)


    //    val seqFields = struct.fields map {
    //      f => FileBlockField(sdna.names(f.fieldName),sdna.types(f.fieldType),sdna.lenghts(f.fieldType),0,
    //        if ( sdna.names(f.fieldName).startsWith("*")) { Seq() } else { transformStructure(sdna,sdna.types(f.fieldType))} )
    //    }
    //    seqFields
    //Seq()
  }


}
