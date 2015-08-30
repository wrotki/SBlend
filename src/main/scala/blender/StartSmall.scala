package blender

import java.nio.charset.Charset

import scodec.Codec


object StartSmall {
  case class FileHeader (
                          meat: String
                          )
  implicit val charSet: Charset = Charset.forName("UTF-8")
  //implicit val evidence : Integral[String]
  import scodec.codecs._

  val cod = {
    //("blender" | string.unit("BLENDER")) :~>:
     // ("meat" | fixedSizeBytes(5,string))
  }

 // implicit val fileHeader: Codec[FileHeader] = fixedSizeBytes(12, cod).as[FileHeader]

//  implicit val blend: Codec[Blend] = {
//    ("fileHeader" | fileHeader) >>:~ { hdr =>
//      ("remainder" | variableSizeBytes(uintL(18), utf8))
//    }
//  }.as[Blend]
}