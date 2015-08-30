import java.nio.charset.Charset

import blender.Parser
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
 * http://mpilquist.github.io/blog/2013/06/09/scodec-part-3/
 */


object SBlend extends App{

  sealed trait ByteOrdering
  case object BigEndian extends ByteOrdering
  case object LittleEndian extends ByteOrdering

  case class BlenderHeader(ordering: ByteOrdering)

  override def main(args: Array[String]): Unit = {
    val parser = new Parser
    val blend = parser.Parse(args(0))
    println(blend)
  }

}
