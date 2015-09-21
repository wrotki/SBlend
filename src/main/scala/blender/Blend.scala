package blender

import java.nio.charset.Charset
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._
import shapeless.{HNil, HList}

object BlenderCodecs {

  implicit val charSet: Charset = Charset.forName("UTF-8")

  case class FileHeader(
                         pointerSize: String,
                         endianness: String,
                         versionNumber: String
                         )

  implicit val fileHeader: Codec[FileHeader] = fixedSizeBytes(12, {
    ("blender" | fixedSizeBytes(7, ascii.unit("BLENDER"))) :~>:
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
  def pSize(size: String) = if( size == "_") 4 else 8

  implicit def fileBlockHeader(pointerSize: String): Codec[FileBlockHeader] = {
    ("code" | fixedSizeBytes(4, string)) ::
      ("size" | fixedSizeBytes(4, int32L)) ::
      ("oldMemoryAddress" | fixedSizeBytes(pSize(pointerSize), if (pSize(pointerSize) ==  4) int32L else int32L /*int64L*/)) :: // TODO 64 bit pointers
      ("sdnaIndex" | fixedSizeBytes(4, int32L)) ::
      ("count" | fixedSizeBytes(4, int32L))
  }.as[FileBlockHeader]

  case class FileBlockData(data: ByteVector)

  implicit def fileBlockData(size: Int) : Codec[FileBlockData]  = {
    ("data" | bytes(size))
  }.as[FileBlockData]

  case class FileBlock(
                        header: FileBlockHeader,
                        data: FileBlockData
                        )

  def fileBlock(pointerSize: String) : Codec[FileBlock]  = {
    ("header" | fileBlockHeader(pointerSize)) >>:~ { (hdr: FileBlockHeader) =>
      ("data" | fileBlockData(hdr.size)).hlist

    }
  }.as[FileBlock]

  case class Blend(
                    fileHeader: FileHeader,
                    records: List[FileBlock]
                    )

  implicit val blend: Codec[Blend] = {
    ("fileHeader" | fileHeader) >>:~ { (hdr: FileHeader) =>
      list(fileBlock(hdr.pointerSize)).hlist
    }
  }.as[Blend]
}