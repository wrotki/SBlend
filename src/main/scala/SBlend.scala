import java.nio.charset.Charset

import scodec.bits.BitVector
import scodec._
import codecs._

/**
 * Created by mariusz on 4/19/15.
 * http://opengameart.org/forumtopic/blender-file-format
 * https://github.com/scodec/scodec
 * http://scodec.org/guide/scodec-core.html
 * https://github.com/scodec/scodec/blob/master/src/test/scala/scodec/examples/PcapExample.scala
 * http://www.atmind.nl/blender/mystery_ot_blend.html
 * https://github.com/ldo/blendhack/blob/master/blendfile.py
 */


object SBlend extends App{

  sealed trait ByteOrdering
  case object BigEndian extends ByteOrdering
  case object LittleEndian extends ByteOrdering

  case class BlenderHeader(ordering: ByteOrdering)

//  val pair = utf8 ~ uint8

  override def main(args: Array[String]) {
    def bits: BitVector = BitVector.fromMmap(new java.io.FileInputStream(new java.io.File(args(0))).getChannel)

//    val enc = pair.encode(("Foo",42))
//    val r = pair.decode(enc.toOption.get)
    val charset = Charset.forName("UTF-8")
    val blender = fixedSizeBytes[String](7,string(charset))
    val b = blender.decode(bits)
//    println(enc.)
//    val u = uint32
    val res =  bits(0)
  }

}
