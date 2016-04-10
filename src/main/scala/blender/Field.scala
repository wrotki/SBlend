package blender

import blender.SDNA.BlenderCodecs.{Structure, StructureDNA}

import scalaz._
import Scalaz._

case class Type(name: String,fields: Seq[FieldDef])
case class FieldDef(id: String,typeRef: String)


object TypeResolver {
  
  def createStructMap(sdna: StructureDNA) : Map[String,Type] = {
    sdna.structureTypes map {
      st =>
        val tn = sdna.types(st.name)
        tn -> Type(tn, getFields(sdna,tn))
    } toMap
  }

  def getFields(sdna: StructureDNA, typeName: String): Seq[FieldDef] = {
    val typeID = sdna.types.indexOf(typeName)

    val struct: Option[Structure] = sdna.structureTypes find {
      _.name == typeID
    }
    return struct match {
      case Some(str) =>
        str.fields map {
          f => FieldDef(sdna.names(f.fieldName), sdna.types(f.fieldType))
        } toSeq
      case None => Seq()
    }
  }

  def fieldLength(fieldName: String, fieldType: String, typeMap: Map[String,Type]) : Seq[Int] = {

    val lengths = fieldName match {
      case l if l startsWith("*") =>
        Seq(4)
      case _ =>
        val structType = typeMap get fieldType
        val fields = structType flatMap { t => Some(t.fields) }
        val lenghts = fields flatMap { fs =>
          val ret = fs flatMap { f => fieldLength(f.id,f.typeRef,typeMap) }
          Some(ret)
        }
        lenghts getOrElse(Seq())
    }
    lengths
  }

  def fieldLengthFor(fieldName: String, fieldType: String, typeMap: Map[String,Type]) : Seq[Int] = {

    val lengths = fieldName match {
      case l if l endsWith("*") =>
        Seq(4)
      case _ =>
        val structType = typeMap get fieldType
        val fields = structType flatMap { t => Some(t.fields) }
        val lenghts = fields flatMap { fs =>
          val ret = fs flatMap { f => fieldLengthFor(f.id,f.typeRef,typeMap) }
          Some(ret)
        }
        lenghts getOrElse(Seq())
    }
    lengths
  }
}

class TypeResolver {
//
//  def updateTypeMap(sdna: StructureDNA, knownTypes: Map[String,Option[Type]], typeName: String):
//    Map[String,Option[Type]] = {
//    if (typeName == "void") {
//      return knownTypes
//    }
//    val typeID = sdna.types.indexOf(typeName)
//    val tID: Option[String] = sdna.types find { _ == typeName}
//
//    val struct: Option[Structure] = sdna.structureTypes find {
//      _.name == typeID
//    }
//
//    if (struct.isEmpty) {
//      return knownTypes
//    }
//
//    val updated = knownTypes + ("typename" -> Type(typeName,Seq()))
//
//    val types = struct.get.fields map {
//      f => (sdna.types(f.fieldType),
//            updated(sdna.types(f.fieldType)))
//    } toMap
//
//    val typeOption = Some(Type(typeName,Seq[FieldDef]())) // Add stuff here
////    val types2: Map[String, Option[Type]] = types + (typeName -> typeOption)
//
//    knownTypes
//  }
//
}

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

    // Add to input map(all types not already in it)

    val forward = struct.get.fields map {
      f => Field(sdna,sdna.names(f.fieldName), sdna.types(f.fieldType)).tree
    } toStream

    val subForest = struct.get.fields map {
      f => Field(sdna,sdna.names(f.fieldName), sdna.types(f.fieldType)).tree
    } toStream
    val root = TypeProperties(fieldName,typeName,0,0).node(subForest:_*)
    Field(root,Map())
  }
}


