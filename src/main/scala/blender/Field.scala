package blender

import blender.SDNA.BlenderCodecs.{Structure, StructureDNA}

import scala.Option
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
    val arrayP = ".*\\[([0-9]+)\\]".r
    val lengths = fieldName match {
      case p if p startsWith("*") =>
        Seq(4) // TODO: take into account pointer length
      case arrayP(len) =>
        Seq(len toInt) // TODO: take into account the type of array element
      case _ =>
        val lghts = for {
          sType <- typeMap get fieldType
        } yield sType
        if (lghts.isEmpty) {
          return Seq(fieldType match {
            case "void" => 0
            case "char" => 1
            case "short" => 2
            case "int" => 4
            case "long" => 8
          })
        }
//  http://debasishg.blogspot.com/2011/07/monad-transformers-in-scala.html
        // http://blog.originate.com/blog/2013/10/21/reader-monad-for-dependency-injection/
        // http://eed3si9n.com/herding-cats/Combined+Pages.html
        // https://inoio.de/blog/2016/02/12/type-class-101-monadtransformer/
        //
        //  Type():
        //    Seq(Option[Type])

        val lengths = for {
          f <- ListT(typeMap get fieldType map { _.fields toList })
        } yield fieldLength(f.id,f.typeRef,typeMap)

        val resOpt = lengths.run
        val res = resOpt match {
          case Some(l) => l flatten
          case _ => List()
        }
        res toSeq
//        val structType: Option[Type] = typeMap get fieldType
//        val fields: Option[Seq[FieldDef]] = structType flatMap { t => Some(t.fields) }
//        val lenghts = fields flatMap { fs =>
//          val ret = fs flatMap { f => fieldLength(f.id,f.typeRef,typeMap) }
//          Some(ret)
//        }
//        lenghts getOrElse(Seq())


        // http://blog.sigfpe.com/2006/05/grok-haskell-monad-transformers.html
//        val lghtscr = (typeMap get fieldType).map { case structType => structType }
//        val lghtscr5 = for {
//          structType <- typeMap get fieldType
//        } yield structType
//
//          val lghtsc3 = for {
//          structType <- typeMap get fieldType
//          fs <- structType.fields
//          r <- fieldLength(fs.id,fs.typeRef,typeMap)
//        } yield r


        //        val lghtsc = for {
//          structType <- typeMap get fieldType
//          fs <- structType.fields
//          r <- fieldLength(fs.id,fs.typeRef,typeMap)
//        } yield r

        //lghtsc toSeq
        Seq(0)


//        val structType = typeMap get fieldType
//        val fields = structType flatMap { t => Some(t.fields) }
//        val lenghts = fields flatMap { fs =>
//          val ret = fs flatMap { f => fieldLength(f.id,f.typeRef,typeMap) }
//          Some(ret)
//        }
//        lenghts getOrElse(Seq())
    }
    lengths
  }

  def fieldLengthFor(fieldName: String, fieldType: String, typeMap: Map[String, Type]): Seq[Int] = {

    (0 to 10).map { case i => i }

    Seq(0)
//    val lengths = fieldName match {
//      case l if l startsWith ("*") =>
//        Seq(4)
//      case _ =>
//        val res2 = for {
//          structType <- typeMap get fieldType
//          fields <- Some(structType.fields)
//          field <- fields
//          length <- fieldLengthFor(field.id, field.typeRef, typeMap)
//        } yield length
//        res2
//    }
//    lengths toSeq
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


