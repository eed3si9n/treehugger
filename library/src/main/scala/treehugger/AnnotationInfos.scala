/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package treehugger

import scala.reflect.ClassManifest

/** AnnotationInfo and its helpers */
trait AnnotationInfos extends api.AnnotationInfos { self: Forest =>
  object AnnotationInfo extends AnnotationInfoExtractor {
    def marker(atp: Type): AnnotationInfo =
      apply(atp, Nil, Nil)

    // def lazily(lazyInfo: => AnnotationInfo) =
    //  new LazyAnnotationInfo(lazyInfo)

    def apply(
        atp: Type,
        args: List[Tree],
        assocs: List[(Name, ClassfileAnnotArg)]
    ): AnnotationInfo =
      new CompleteAnnotationInfo(atp, args, assocs)

    def unapply(
        info: AnnotationInfo
    ): Option[(Type, List[Tree], List[(Name, ClassfileAnnotArg)])] =
      Some((info.atp, info.args, info.assocs))
  }

  class CompleteAnnotationInfo(
      val atp: Type,
      val args: List[Tree],
      val assocs: List[(Name, ClassfileAnnotArg)]
  ) extends AnnotationInfo {
    // Classfile annot: args empty. Scala annot: assocs empty.
    assert(args.isEmpty || assocs.isEmpty, atp)

    override def toString = (
      atp +
        (if (!args.isEmpty) args.mkString("(", ", ", ")") else "") +
        (if (!assocs.isEmpty) (assocs map { case (x, y) =>
           x + " = " + y
         } mkString ("(", ", ", ")"))
         else "")
    )
  }

  /** Typed information about an annotation. It can be attached to either a
    * symbol or an annotated type.
    *
    * Annotations are written to the classfile as Java annotations if `atp`
    * conforms to `ClassfileAnnotation` (the classfile parser adds this
    * interface to any Java annotation class).
    *
    * Annotations are pickled (written to scala symtab attribute in the
    * classfile) if `atp` inherits form `StaticAnnotation`.
    *
    * `args` stores arguments to Scala annotations, represented as typed trees.
    * Note that these trees are not transformed by any phases following the
    * type-checker.
    *
    * `assocs` stores arguments to classfile annotations as name-value pairs.
    */
  sealed abstract class AnnotationInfo
      extends AbsAnnotationInfo
      with Product3[Type, List[Tree], List[(Name, ClassfileAnnotArg)]] {
    def atp: Type
    def args: List[Tree]
    def assocs: List[(Name, ClassfileAnnotArg)]

    /** Hand rolling Product. */
    def _1 = atp
    def _2 = args
    def _3 = assocs
    def canEqual(other: Any) = other.isInstanceOf[AnnotationInfo]
    override def productPrefix = "AnnotationInfo"

    // see annotationArgRewriter
    // lazy val isTrivial = atp.isTrivial && !hasArgWhich(_.isInstanceOf[This])

    private var rawpos: Position = NoPosition
    def pos = rawpos
    def setPos(pos: Position): this.type = {
      rawpos = pos
      this
    }

    /** Annotations annotating annotations are confusing so I drew an example.
      */
    def symbol = atp.typeSymbol

    /** These are meta-annotations attached at the use site; they only apply to
      * this annotation usage. For instance, in
      * `@(deprecated @setter @field) val ...` metaAnnotations = List(setter,
      * field).
      */
    def metaAnnotations: List[AnnotationInfo] = atp match {
      case AnnotatedType(metas, _, _) => metas
      case _                          => Nil
    }

    /** The default kind of members to which this annotation is attached. For
      * instance, for scala.deprecated defaultTargets = List(getter, setter,
      * beanGetter, beanSetter).
      */
    // def defaultTargets = symbol.annotations map (_.symbol) filter isMetaAnnotation

    // Test whether the typeSymbol of atp conforms to the given class.
    def matches(clazz: Symbol) = symbol isNonBottomSubClass clazz
    // All subtrees of all args are considered.
    def hasArgWhich(p: Tree => Boolean) = args exists (_ exists p)

    /** Check whether the type or any of the arguments are erroneous */
    // def isErroneous = atp.isErroneous || args.exists(_.isErroneous)

    // def isStatic = symbol isNonBottomSubClass StaticAnnotationClass

    /** Check whether any of the arguments mention a symbol */
    def refsSymbol(sym: Symbol) = hasArgWhich(_.symbol == sym)

    /** Change all ident's with Symbol "from" to instead use symbol "to" */
    // def substIdentSyms(from: Symbol, to: Symbol) = {
    //   val subs = new TreeSymSubstituter(List(from), List(to))
    //   AnnotationInfo(atp, args.map(subs(_)), assocs).setPos(pos)
    // }

    def stringArg(index: Int) = constantAtIndex(index) map (_.stringValue)
    def intArg(index: Int) = constantAtIndex(index) map (_.intValue)
    def symbolArg(index: Int) = argAtIndex(index) collect {
      case Apply(fun, Literal(str) :: Nil)
          if fun.symbol == definitions.Symbol_apply =>
        newTermName(str.stringValue)
    }

    // !!! when annotation arguments are not literals, but any sort of
    // expression, there is a fair chance they will turn up here not as
    // Literal(const) but some arbitrary AST.
    def constantAtIndex(index: Int): Option[Constant] =
      argAtIndex(index) collect { case Literal(x) => x }

    def argAtIndex(index: Int): Option[Tree] =
      if (index < args.size) Some(args(index)) else None

    override def hashCode = atp.## + args.## + assocs.##
    override def equals(other: Any) = other match {
      case x: AnnotationInfo =>
        (atp == x.atp) && (args == x.args) && (assocs == x.assocs)
      case _ => false
    }
  }

  /** Arguments to classfile annotations (which are written to bytecode as java
    * annotations) are either:
    *
    *   - constants
    *   - arrays of constants
    *   - or nested classfile annotations
    */
  abstract class ClassfileAnnotArg extends Product

  /** Represents a compile-time Constant (`Boolean`, `Byte`, `Short`, `Char`,
    * `Int`, `Long`, `Float`, `Double`, `String`, `java.lang.Class` or an
    * instance of a Java enumeration value).
    */
  case class LiteralAnnotArg(const: Constant) extends ClassfileAnnotArg {
    override def toString = const.escapedStringValue
  }

  lazy val classfileAnnotArgManifest: ClassManifest[ClassfileAnnotArg] =
    ClassManifest.classType(classOf[ClassfileAnnotArg])
}
