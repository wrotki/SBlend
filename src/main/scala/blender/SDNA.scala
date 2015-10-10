package blender

import java.nio.charset.Charset

import scodec.bits.ByteVector

/**
 * Created by mariusz on 10/10/15.
 */
object SDNA {
  object BlenderCodecs {

    implicit val charSet: Charset = Charset.forName("UTF-8")

    case class Structure(
                        name: Short ,// // each: Index in types containing the name of the structure
                        numberOfFields: Short,
                        fieldType: Short,
                        fieldName: Short
                          )
    case class StructureDNA(
                           // SDNA // identifier: String,
                           // NAME // nameIdentifier: String,
                           numberOfNames: Int,
                           names: List[String],
                           // TYPE // typeIdentifier: String,
                           numberOfTypes: Int,
                           types: List[String],
                           // TLEN //
                           lenghts: List[Short],
                           // STRC //
                           numberOfStructures: Int,
                           structureTypes: List[Structure]
                           )
  }
}
