package blender

import java.nio.charset.Charset
import scodec.Codec
import scodec.codecs._
import scodec.bits._
import shapeless.HNil

object BlenderCodecs {
  case class FileHeader (
                          //blender: String,
                          pointerSize: String,
                          endianness: String,
                          versionNumber: String
                          )
  case class Blend(
                    fileHeader: FileHeader,
                    dummy: String,
                    remainder: String
                    )

  implicit val charSet: Charset = Charset.forName("UTF-8")

  implicit val fileHeader: Codec[FileHeader] = fixedSizeBytes(12, {
      ("blender" | string.unit("BLENDER")) :~>:
      ("pointerSize" | fixedSizeBytes(1,string)) ::
      ("endianness" | fixedSizeBytes(1,string)) ::
      ("versionNumber" | fixedSizeBytes(3,string))
  }).as[FileHeader]

  implicit val blend: Codec[Blend] = {
    ("fileHeader" | fileHeader) >>:~ { (hdr: FileHeader) =>
      ("dummy" | fixedSizeBytes(1,string)) ::
      ("remainder" | string )
    }
  }.as[Blend]
}