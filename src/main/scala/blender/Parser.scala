package blender

import blender.BlenderCodecs.Blend
import scodec.{Codec}
import scodec.bits._

class Parser {

  def Parse(filePath: String) : Blend = {
    def bits: BitVector = BitVector.fromMmap(new java.io.FileInputStream(new java.io.File(filePath)).getChannel)
    val result: Blend = Codec.decode[Blend](bits).require.value
    result
  }

}
