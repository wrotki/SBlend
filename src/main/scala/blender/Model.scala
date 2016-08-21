package blender

import java.io.{File, FileOutputStream}
import java.nio.charset.Charset

import blender.BlenderCodecs.{FileBlock, Blend}
import blender.SDNA.BlenderCodecs.StructureDNA
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._
import shapeless.{HNil, HList}

case class Model(blend: Blend, typeMap: Map[String, Type],heap: Map[Int, FileBlock]) {

  implicit val charSet: Charset = Charset.forName("UTF-8")

  private val intCodec = ("int" | fixedSizeBytes(4, int32L)).hlist
  private val shortCodec = ("short" | fixedSizeBytes(2, int16L)).hlist

  val sceneCodec = intCodec :: intCodec :: intCodec :: intCodec ::
    fixedSizeBytes(24, string) :: shortCodec :: shortCodec :: intCodec ::
    intCodec /* *camera */ :: intCodec

  def printScene: Unit = {

    val scene: FileBlock = blend.records filter {
      //_.header.code.startsWith("SC")
      _.header.sdnaIndex == 140
    } head
    val sceneBytes: ByteVector = scene.data.data
    val fo = new FileOutputStream(new File("./scene"))
    val sdnaReadable = sceneBytes.copyToStream(fo)
    val fbh = scene.header
    println(s"Scene FileBlockHeader: $fbh")

    // println(TypeResolver.fieldLength("", "id", "ID", typeMap))
    val lengths: Seq[FieldLength] = TypeResolver.fieldLength("", "sc", "Scene", typeMap)

    lengths map { l => println(l.id,l.typeRef,l.length) }



    //println(lengths map {_.length})

    //    def offsets(input: Seq[Int]): Seq(Int) = {
    //
    //    }
    val offsets = (lengths map { _.length }).scanLeft(0)(_ + _)
    //println(offsets)


    //val b1 = blend flatMap { println }
    //println(scene)

    println("----------------------")
    println("----------------------")

    val pointers = sceneCodec.decode(scene.data.data.toBitVector)
    println ("Pointers: "+ pointers)
  }

  /**
    *
    *
    * Unimportant stuff
    *
    *
    */

  private def printCameraBytes: Unit = {

    // Camera: *Object: 0336c480  -    (minus ) 138933992 (0x847F6E8)
    //    val cam0: FileBlock = heap(138933992)
    val cam0: FileBlock = heap(0x0336c480)

    val camera: FileBlock = blend.records filter {
      //_.header.code.startsWith("SC")
      _.header.sdnaIndex == 30
    } head
    val cameraBytes: ByteVector = camera.data.data
    val cfo = new FileOutputStream(new File("./camera"))
    val csdnaReadable = cameraBytes.copyToStream(cfo)
    val cfbh = camera.header
    println(s"Camera FileBlockHeader: $cfbh")

    val clengths = TypeResolver.fieldLength("", "cam", "Camera", typeMap)
    clengths map { l => println(l.id,l.typeRef,l.length) }
  }
}
/**
  * Created by mariusz on 8/21/16.
  */
object Model {

  def loadBlendFile(args: Array[String]): Model = {
    //    val h: ::[Int, ::[String, ::[(String, Int), ::[Int, HNil]]]] = 1 :: "foo" :: ("bar",2) :: 3 :: HNil
    //    val h0 = h(0)
    //    val h1 = h(1)
    //    val h2 = h(2)
    //    println(h(2))

    val (blend: Blend, sdnaDecoded: StructureDNA) = getSDNA(args)

    import blender.TypeResolver

    println("140:"+sdnaDecoded.types(sdnaDecoded.structureTypes(140).name)) // Scene
    println("30:"+sdnaDecoded.types(sdnaDecoded.structureTypes(30).name)) // Camera
    val typeMap: Map[String, Type] = TypeResolver.createStructMap(sdnaDecoded)
    typeMap filterKeys {
      //_ == "ID"
      _ == "Scene"
    } foreach { case (k, v) => println(s"Type: $k Fields: $v") }
    // ----------------


    //    println(sdnaDecoded)

    //val st: Structure = sdnaDecoded.structureTypes(0)

    //    sdnaDecoded.structureTypes foreach { st =>
    //      val std = (sdnaDecoded.names(st.name), st.fields map { f => (sdnaDecoded.names(f.fieldName), sdnaDecoded.types(f.fieldType)) })
    //      println(std)
    //    }
    println("Structure(140) names:" + sdnaDecoded.types(140))

    val heap: Map[Int, FileBlock] = blend.records map { fb => (fb.header.oldMemoryAddress, fb) } toMap

    //    println("----------------------")
    //
    //    import blender.Show._
    //    println(SceneTree.typeTree.drawTree)

    new Model(blend,typeMap,heap)
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
