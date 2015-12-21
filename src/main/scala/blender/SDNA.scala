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

    /* private[codecs] final*/ class WordAlignedCodec[A](codec: Codec[A]) extends Codec[A] {

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
          val taken = b.size - res.remainder.size
          val pad = padAmount(taken)
          if (pad == 0) res
          else DecodeResult(res.value, res.remainder.drop(pad))
        }
      }

      override def toString = s"byteAligned($codec)"
    }


    case class Structure(
                          name: Int, // // each: Index in types containing the name of the structure
                          numberOfFields: Int,
                          fieldType: Int,
                          fieldName: Int
                          )

    implicit val structure: Codec[Structure] = fixedSizeBytes(8, {
      ("name" | fixedSizeBytes(2, int16L)) ::
        ("numberOfFields" | fixedSizeBytes(2, int16L)) ::
        ("fieldType" | fixedSizeBytes(2, int16L)) ::
        ("fieldName" | fixedSizeBytes(2, int16L))
    }).as[Structure]

    case class StructureDNA(
                             // SDNA // identifier: String,
                             // NAME // nameIdentifier: String,
                             //numberOfNames: Int,
                             names: List[String],
                             // TYPE // typeIdentifier: String,
                             numberOfTypes: Int,
                             types: List[String],
                             // TLEN //
                             lenghts: List[Int]//,
                             // STRC //
                             //numberOfStructures: Int,
                             //structureTypes: List[Structure]
                             //oneStructureType: Structure
                             )

    // http://mpilquist.github.io/blog/2013/06/01/scodec-part-2/
    case class TypesStruct(
                          numberOfTypes: Int,
                          types: List[String],
                          lengths: List[Int]
                          )

    implicit val typesCodec: Codec[Aux[shapeless.::[Int, shapeless.::[List[String], shapeless.::[List[Int], HNil]]], shapeless.::[shapeless.::[Int, shapeless.::[List[String], shapeless.::[List[Int], HNil]]], HNil]]#Out] = (
      ("numberOfTypes" | fixedSizeBytes(4, int32L)) flatPrepend { numberOfTypes =>
        ("types" | listOfN(provide(numberOfTypes), cstring)) ::
          ("TLEN" | new WordAlignedCodec(fixedSizeBytes(4, ascii.unit("TLEN")))) :~>:
          ("lengths" | listOfN(provide(numberOfTypes), int16L))
      }  ) flattenLeftPairs


    val typesStructAs: Codec[TypesStruct] = typesCodec.as[TypesStruct]

    implicit val structureDNA: Codec[StructureDNA] = {
      ("SDNA" | fixedSizeBytes(4, ascii.unit("SDNA"))) :~>:
        ("NAME" | fixedSizeBytes(4, ascii.unit("NAME"))) :~>:
        ("names" | listOfN(fixedSizeBytes(4, int32L), cstring)) ::
        ("TYPE" | new WordAlignedCodec(fixedSizeBytes(4, ascii.unit("TYPE")))) :~>: (
         typesCodec )::
        ("STRC" | new WordAlignedCodec(fixedSizeBytes(4, ascii.unit("STRC")))) //:~>:
//        ("structureType" | fixedSizeBytes(8, structure))
      //("structureTypes" | listOfN(fixedSizeBytes(4, int32L), structure))
    }.as[StructureDNA]
  }

}
