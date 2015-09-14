package blender

import java.nio.charset.Charset
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._
import shapeless.HList

object BlenderCodecs {

  implicit val charSet: Charset = Charset.forName("UTF-8")

  case class FileHeader(
                         pointerSize: Int,
                         endianness: String,
                         versionNumber: String
                         )

  implicit val fileHeader: Codec[FileHeader] = fixedSizeBytes(12, {
    ("blender" | fixedSizeBytes(7, ascii.unit("BLENDER"))) :~>:
      ("pointerSize" | fixedSizeBytes(1, int8)) ::
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

  def fileBlockHeader(pointerSize: Int): Codec[FileBlockHeader] = {
    ("code" | fixedSizeBytes(4, string)) ::
      ("size" | fixedSizeBytes(4, int32)) ::
      ("oldMemoryAddress" | fixedSizeBytes(pointerSize, int32)) ::
      ("sdnaIndex" | fixedSizeBytes(4, int32)) ::
      ("count" | fixedSizeBytes(pointerSize, int32))
  }.as[FileBlockHeader]

  case class FileBlockData(data: ByteVector)

  def fileBlockData(size: Int) : Codec[FileBlockData]  = {
    ("data" | bytes(size))
  }.as[FileBlockData]

  case class FileBlock(
                        header: FileBlockHeader,
                        data: FileBlockData
                        )

  def fileBlock(pointerSize: Int) : Codec[FileBlock]  = {
    ("header" | fileBlockHeader(pointerSize)) >>:~ { (hdr: FileBlockHeader) =>
      ("data" | fileBlockData(hdr.size))
    }
  }.as[FileBlock]

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