package blender

import java.nio.charset.Charset

import blender.SDNA.BlenderCodecs.StructureDNA
import scodec.codecs.FlattenLeftPairs.Aux
import scodec.{DecodeResult, SizeBound, Codec}
import scodec.bits.BitVector
import scodec.codecs._
import shapeless.HNil

/**
 * Created by mariusz on 10/10/15.
 */
object SDNA {

  object BlenderCodecs {

    implicit val charSet: Charset = Charset.forName("UTF-8")

    class WordAlignedCodec[A](codec: Codec[A]) extends Codec[A] {

      private def padAmount(size: Long) = {
        val mod = size % 32
        if (mod == 0) 0 else 32 - mod
      }

      def sizeBound = {
        val sz = codec.sizeBound
        val lb = sz.lowerBound + padAmount(sz.lowerBound)
        val ub = sz.upperBound.map { ub => ub + padAmount(ub) }
        SizeBound(lb, ub)
      }

      def encode(a: A) = {
        codec.encode(a) map { enc =>
          val pad = padAmount(enc.size)
          if (pad == 0) enc
          else enc.padTo(enc.size + pad)
        }
      }

      def decode(b: BitVector) = {
        codec.decode(b) map { res =>
//          println("\nAligning")
//          println(b.size)
//          println(res.remainder.size)
          val taken = b.size - res.remainder.size
          val pad = padAmount(taken)
//          println("\nPad")
//          println(pad)
          if (pad == 0) res
          else DecodeResult(res.value, res.remainder.drop(pad))
        }
      }

      override def toString = s"byteAligned($codec)"
    }

    def wordAligned[A](codec: Codec[A]): Codec[A] = new WordAlignedCodec(codec)

/*
============================================================================
*/

    /**StructureDNA
      *
      * @param names
      * @param numberOfTypes
      * @param types
      * @param lenghts
      * @param structureTypes
      */
    case class StructureDNA(
                             // SDNA // identifier: String,
                             // NAME // nameIdentifier: String,
                             //numberOfNames: Int,
                             names: List[String],
                             // TYPE // typeIdentifier: String,
                             numberOfTypes: Int,
                             types: List[String],
                             // TLEN //
                             lenghts: List[Int],
                             // STRC //
                             //numberOfStructures: Int,
                             structureTypes: List[Structure]
                             //oneStructureType: Structure
                           )

    val sdnaUnit = "SDNA" | fixedSizeBytes(4, ascii.unit("SDNA"))
    val nameUnit = "NAME" | fixedSizeBytes(4, ascii.unit("NAME"))
    val withCtx = "names" | listOfN(fixedSizeBytes(4, int32L), cstring)
    val typeName = "TYPE" | fixedSizeBytes(4, ascii.unit("TYPE"))
    val typesWithCtx = "types" | listOfN(fixedSizeBytes(4, int32L), cstring)
    val strName = "STRC" | fixedSizeBytes(4, ascii.unit("STRC"))

    val allTogetherWithoutTemps5 = sdnaUnit :~>: nameUnit :~>: wordAligned(withCtx) :: typeName :~>:   (
      ("numberOfTypes" | fixedSizeBytes(4, int32L)) flatPrepend { numberOfTypes =>
        ("types" | wordAligned(listOfN(provide(numberOfTypes), cstring))) ::
          ("TLEN" | fixedSizeBytes(4, ascii.unit("TLEN"))) :~>:
          ("lengths" | wordAligned(listOfN(provide(numberOfTypes), int16L))) ::
          strName :~>: ("structures" | listOfN(fixedSizeBytes(4, int32L), structure))
      })
    implicit val structureDNA = allTogetherWithoutTemps5.as[StructureDNA]

    /** Structure
      *
      * @param name
      */
    case class Structure(
                          name: Int, // // each: Index in types containing the name of the structure
                          fields: List[Field]
                          )

    implicit val field: Codec[Field] =
      (
        ("fieldType" | fixedSizeBytes(2, int16L)) ::
          ("fieldName" | fixedSizeBytes(2, int16L))
        ).as[Field]

    implicit val structure: Codec[Structure] =
      (("name" | fixedSizeBytes(2, int16L)) ::
        ("fields" | listOfN(fixedSizeBytes(2, int16L), field))
        ).as[Structure]

    case class Field (
                       fieldType: Int,
                       fieldName: Int
                     )
 }
}
