package blender

import java.nio.charset.Charset
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._
import shapeless.HList

object BlenderCodecs {

  implicit val charSet: Charset = Charset.forName("UTF-8")

  case class FileHeader(
                         //blender: String,
                         pointerSize: String,
                         endianness: String,
                         versionNumber: String
                         )

  implicit val fileHeader: Codec[FileHeader] = fixedSizeBytes(12, {
    ("blender" | fixedSizeBytes(7, ascii.unit("BLENDER"))) :~>:
      //      ("blender" | fixedSizeBytes(7,string)) ::
      ("pointerSize" | fixedSizeBytes(1, string)) ::
      ("endianness" | fixedSizeBytes(1, string)) ::
      ("versionNumber" | fixedSizeBytes(3, string))
  }).as[FileHeader]

  case class FileBlockHeader(
                        code: String,
                        size: Int,
                        oldMemoryAddress: Int,
                        sdnaIndex: Int,
                        count: Int
                        )

  implicit val fileBlockHeader: Codec[FileBlockHeader] = {
    ("code" | fixedSizeBytes(4,string)) >>:~ { implicit code =>
      ("size" | fixedSizeBytes(4, int32)) ::
        ("oldMemoryAddress" | fixedSizeBytes(4, int32))
    }}.as[FileBlockHeader]

  case class FileBlockData(data: ByteVector)

  case class FileBlock(
                        header: FileBlockHeader,
                          data: FileBlockData
                            )
//  implicit val fileBlock: Codec[FileBlock] = {
//    ("header" | fileBlockHeader) >>:~ { (hdr: FileBlockHeader) =>
//      HList("data" | bytes(hdr.size)) }
//  }.as[FileBlock]

  //  implicit def pcapRecord(implicit ordering: ByteOrdering) = {
  //    ("record_header"    | pcapRecordHeader                   ) >>:~ { hdr =>
  //      ("record_data"      | bits(hdr.includedLength * 8) ).hlist
  //    }}.as[PcapRecord]

  case class Blend(
                    fileHeader: FileHeader,
                    records: Vector[FileBlock]
                    //                    dummy:String,
                    //                    remainder: ByteVector
                    )


  implicit val blend: Codec[Blend] = {

    ("fileHeader" | fileHeader) >>:~ { (hdr: FileHeader) =>
      ("dummy" | fixedSizeBytes(1, string)) ::
        ("remainder" | bytes)
    }
  }.as[Blend]
}