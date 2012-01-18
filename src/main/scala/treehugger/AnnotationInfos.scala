/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package treehugger

/** AnnotationInfo and its helpers */
trait AnnotationInfos extends api.AnnotationInfos { self: Forest =>
  case class AnnotationInfo(atp: Type, args: List[Tree], assocs: List[(Name, ClassfileAnnotArg)])
  
  object AnnotationInfo extends AnnotationInfoExtractor
  
  /** Arguments to classfile annotations (which are written to
   *  bytecode as java annotations) are either:
   *
   *  - constants
   *  - arrays of constants
   *  - or nested classfile annotations
   */
  abstract class ClassfileAnnotArg extends Product

  /** Represents a compile-time Constant (`Boolean`, `Byte`, `Short`,
   *  `Char`, `Int`, `Long`, `Float`, `Double`, `String`, `java.lang.Class` or
   *  an instance of a Java enumeration value).
   */
  case class LiteralAnnotArg(const: Constant)
  extends ClassfileAnnotArg {
    override def toString = const.escapedStringValue
  }
  
  lazy val classfileAnnotArgManifest: ClassManifest[ClassfileAnnotArg] =
    reflect.ClassManifest.classType(classOf[ClassfileAnnotArg])  
}
