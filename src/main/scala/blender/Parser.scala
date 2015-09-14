package blender

import java.io.{File, FileInputStream}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel

import blender.BlenderCodecs.Blend
import scodec.{DecodeResult, Attempt, Codec}
import scodec.bits._

class Parser {

  def Parse(filePath: String) : Attempt[DecodeResult[Blend]] = {
    //println(s"$filePath")
    val fileChannel: FileChannel = new FileInputStream(new File(filePath)).getChannel
//    val buf = ByteBuffer.allocateDirect(10)
//    val count = fileChannel.read(buf)
//    val cBuf = buf.asCharBuffer
//
//    buf.flip
//    while (buf.hasRemaining) {
//      println(buf.get.asInstanceOf[Char])
//    }
    def bits: BitVector = {
      BitVector.fromMmap(fileChannel)
    }
    //val decoded = Codec.decode[MpegPacket](encoded).require.value
    val attempt: Attempt[DecodeResult[Blend]] = Codec.decode[Blend](bits)
    return attempt
  }

}
