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
  * http://homac.cakelab.org/projects/JavaBlend/spec.html
  * https://github.com/mewspring/blend
  */

object SBlend extends App {

  override def main(args: Array[String]): Unit = {

    val model = Model.loadBlendFile(args)
    model.printScene
  }
}
