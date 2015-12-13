package blender

import java.nio.charset.Charset

import blender.SDNA.BlenderCodecs.StructureDNA
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._
import shapeless.HNil

/**
 * Created by mariusz on 10/10/15.
 */
object SDNA {

  object BlenderCodecs {

    implicit val charSet: Charset = Charset.forName("UTF-8")

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
                             )

    implicit val structureDNA: Codec[StructureDNA] = {
      ("SDNA" | fixedSizeBytes(4, ascii.unit("SDNA"))) :~>:
        ("NAME" | fixedSizeBytes(4, ascii.unit("NAME"))) :~>:
        ("names" | listOfN(fixedSizeBytes(4, int32L), cstring)) ::
        ("TYPE" | fixedSizeBytes(4, ascii.unit("TYPE"))) :~>:
        (
        ("numberOfTypes" | fixedSizeBytes(4, int32L)) flatPrepend { numberOfTypes =>
          ("types" | listOfN(provide(numberOfTypes), cstring)) ::
          ("TLEN" | fixedSizeBytes(4, ascii.unit("TLEN"))) :~>:
          ("lengths" | listOfN(provide(numberOfTypes), int16L))
          })}.as[StructureDNA]
  }

}
