package blender

import blender.SDNA.BlenderCodecs.{Structure, StructureDNA}

import scalaz._
import Scalaz._

case class Field(tree: Tree[TypeProperties],types: Map[String,Field])

object Field {
  def apply(sdna: StructureDNA, fieldName: String, typeName: String): Field = {
    if (typeName == "void") {
      return Field(Tree.Leaf(TypeProperties(fieldName,typeName,0,0)),Map())
    }
    val typeID = sdna.types.indexOf(typeName)

    val struct: Option[Structure] = sdna.structureTypes filter {
      _.name == typeID
    } headOption

    if (struct.isEmpty) {
      return Field(Tree.Leaf(TypeProperties(fieldName,typeName,0,0)),Map())
    }
    val subForest = struct.get.fields map {
      f => Field(sdna,sdna.names(f.fieldName), sdna.types(f.fieldType)).tree
    } toStream
    val root = TypeProperties(fieldName,typeName,0,0).node(subForest:_*)
    Field(root,Map())
  }
}


