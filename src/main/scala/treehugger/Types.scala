package treehugger

trait Types extends api.Types { self: Universe =>
  import definitions._
  
  abstract class AbsTypeImpl extends AbsType { this: Type =>
  }
  
  /** The base class for all types */
  abstract class Type extends AbsTypeImpl {
    /** Method to be implemented in subclasses.
     *  Converts this type to a string in calling toString for its parts.
     */
    def safeToString: String = super.toString
    
    /** The type symbol associated with the type
      * Note that the symbol of the normalized type is returned (@see normalize)
      */
    def typeSymbol: Symbol = NoSymbol
    
    /** The base type underlying a type proxy, identity on all other types */
    def underlying: Type = this

    /** Widen from singleton type to its underlying non-singleton
     *  base type by applying one or more `underlying` dereferences,
     *  identity for all other types.
     *
     *  class Outer { class C ; val x: C }
     *  val o: Outer
     *  <o.x.type>.widen = o.C
     */
    def widen: Type = this
    

    /** For a class or intersection type, its parents.
     *  For a TypeBounds type, the parents of its hi bound.
     *  inherited by typerefs, singleton types, and refinement types,
     *  The empty list for all other types */
    def parents: List[Type] = List()
    
    /** For a classtype or refined type, its defined or declared members;
     *  inherited by subtypes and typerefs.
     *  The empty scope for all other types.
     */
    def decls: Scope = EmptyScope
    
    /** For a method or poly type, its first value parameter section,
     *  the empty list for all other types */
    def params: List[Symbol] = List()
    
    /** For a (nullary) method or poly type, its direct result type,
     *  the type itself for all other types. */
    def resultType: Type = this
    
    /** For a (potentially wrapped) poly type, its type parameters,
     *  the empty list for all other types */
    def typeParams: List[Symbol] = List() 
  }
  
  trait UniqueType extends Product {
    final override val hashCode = scala.runtime.ScalaRunTime._hashCode(this)
  }

  /** A base class for types that defer some operations
    *  to their immediate supertype.
    */
  abstract class SubType extends Type {
    def supertype: Type
    // override def parents: List[Type] = supertype.parents
    // override def decls: Scope = supertype.decls
    // override def baseType(clazz: Symbol): Type = supertype.baseType(clazz)
    // override def baseTypeSeq: BaseTypeSeq = supertype.baseTypeSeq
    // override def baseTypeSeqDepth: Int = supertype.baseTypeSeqDepth
    // override def baseClasses: List[Symbol] = supertype.baseClasses
    // override def isNotNull = supertype.isNotNull
  }

  /** A proxy for a type (identified by field `underlying`) that forwards most
   *  operations to it (for exceptions, see WrappingProxy, which forwards even more operations).
   *  every operation that is overridden for some kind of types should be forwarded.
   */
  trait SimpleTypeProxy extends Type {
    def underlying: Type
    override def typeSymbol = underlying.typeSymbol
  }
  
  abstract class SingletonType extends SubType with SimpleTypeProxy with AbsSingletonType {
    def supertype = underlying
    override def widen: Type = underlying.widen
  }
  
  /** An object representing a non-existing type */
  case object NoType extends Type {
    // override def isTrivial: Boolean = true
    override def safeToString: String = "<notype>"
    // override def isNullable: Boolean = true
    // override def kind = "NoType"
  }
  
  /** An object representing a non-existing prefix */
  case object NoPrefix extends Type {
    override def safeToString: String = "<noprefix>"
    // override def isNullable: Boolean = true
    // override def kind = "NoPrefixType"
  }
  
  /** A class for this-types of the form <sym>.this.type
   */
  abstract case class ThisType(sym: Symbol) extends SingletonType {
    override def safeToString: String = super.safeToString
  }
  
  final class UniqueThisType(sym: Symbol) extends ThisType(sym) with UniqueType { }
  
  object ThisType extends ThisTypeExtractor {
    def apply(sym: Symbol): Type = new UniqueThisType(sym)
  }
  
  /** A class for singleton types of the form `<prefix>.<sym.name>.type`.
   *  Cannot be created directly; one should always use `singleType` for creation.
   */
  abstract case class SingleType(pre: Type, sym: Symbol) extends SingletonType {
  }
  
  final class UniqueSingleType(pre: Type, sym: Symbol) extends SingleType(pre, sym) with UniqueType { }
  
  object SingleType extends SingleTypeExtractor {
    def apply(pre: Type, sym: Symbol): Type = {
      new UniqueSingleType(pre, sym)
    }
  }
  
  abstract case class SuperType(thistpe: Type, supertpe: Type) extends SingletonType {
    override def typeSymbol = thistpe.typeSymbol
    override def underlying = supertpe
  }

  final class UniqueSuperType(thistp: Type, supertp: Type) extends SuperType(thistp, supertp) with UniqueType { }

  object SuperType extends SuperTypeExtractor {
    def apply(thistp: Type, supertp: Type): Type = new UniqueSuperType(thistp, supertp)
  }
  
  /** A class for the bounds of abstract types and type parameters
   */
  abstract case class TypeBounds(lo: Type, hi: Type) extends SubType {
    def supertype = hi
    // override val isTrivial: Boolean = lo.isTrivial && hi.isTrivial
    // override def bounds: TypeBounds = this
    // def containsType(that: Type) = that match {
    //   case TypeBounds(_, _) => that <:< this
    //   case _                => lo <:< that && that <:< hi
    // }
    // override def isNullable: Boolean = NullClass.tpe <:< lo;
    override def safeToString = ">: " + lo + " <: " + hi
  }
  
  final class UniqueTypeBounds(lo: Type, hi: Type) extends TypeBounds(lo, hi) with UniqueType { }

  object TypeBounds extends TypeBoundsExtractor {
    def empty: TypeBounds           = apply(NothingClass.tpe, AnyClass.tpe)
    def upper(hi: Type): TypeBounds = apply(NothingClass.tpe, hi)
    def lower(lo: Type): TypeBounds = apply(lo, AnyClass.tpe)
    def apply(lo: Type, hi: Type): TypeBounds = {
      new UniqueTypeBounds(lo, hi).asInstanceOf[TypeBounds]
    }
  }
  
  /** A class for named types of the form
   *  `<prefix>.<sym.name>[args]`
   *  Cannot be created directly; one should always use `typeRef`
   *  for creation. (@M: Otherwise hashing breaks)
   *
   * @M: a higher-kinded type is represented as a TypeRef with sym.info.typeParams.nonEmpty, but args.isEmpty
   *  @param pre  ...
   *  @param sym  ...
   *  @param args ...
   */
  abstract case class TypeRef(pre: Type, sym: Symbol, args: List[Type]) extends Type {
  }
  
  final class UniqueTypeRef(pre: Type, sym: Symbol, args: List[Type]) extends TypeRef(pre, sym, args) with UniqueType { }
  
  object TypeRef extends TypeRefExtractor {
    def apply(pre: Type, sym: Symbol, args: List[Type]): Type = {
      new UniqueTypeRef(pre, sym, args)
    }
  }
  
  /** A class representing a method type with parameters.
   *  Note that a parameterless method is represented by a NullaryMethodType:
   *
   *    def m(): Int        MethodType(Nil, Int)
   *    def m: Int          NullaryMethodType(Int)
   */
  case class MethodType(override val params: List[Symbol],
                        override val resultType: Type) extends Type {
  }
  
  object MethodType extends MethodTypeExtractor
  
  /** A class representing a constant type.
   *
   *  @param value ...
   */
  abstract case class ConstantType(value: Constant) extends SingletonType {
    // override def underlying: Type = value.tpe
    // override def safeToString: String =
    //  underlying.toString + "(" + value.escapedStringValue + ")"
  }
  
  final class UniqueConstantType(value: Constant) extends ConstantType(value) with UniqueType {
  }

  object ConstantType extends ConstantTypeExtractor {
    def apply(value: Constant): ConstantType = {
      new UniqueConstantType(value).asInstanceOf[ConstantType]
    }
  }  
  
  /** A common base class for intersection types and class types
   */
  abstract class CompoundType extends Type {
  }

  /** A class representing intersection types with refinements of the form
   *    `<parents_0> with ... with <parents_n> { decls }`
   *  Cannot be created directly;
   *  one should always use `refinedType` for creation.
   */
  case class RefinedType(override val parents: List[Type],
                         override val decls: Scope) extends CompoundType {
  }
  
  final class RefinedType0(parents: List[Type], decls: Scope, clazz: Symbol) extends RefinedType(parents, decls) {
    override def typeSymbol = clazz
  }

  object RefinedType extends RefinedTypeExtractor {
    def apply(parents: List[Type], decls: Scope, clazz: Symbol): RefinedType =
      new RefinedType0(parents, decls, clazz)
  }
  
  /** A class representing a class info
   */
  case class ClassInfoType(
    override val parents: List[Type],
    override val decls: Scope,
    override val typeSymbol: Symbol) extends CompoundType {
  }

  object ClassInfoType extends ClassInfoTypeExtractor

  class PackageClassInfoType(decls: Scope, clazz: Symbol)
  extends ClassInfoType(List(), decls, clazz)
   
  case class NullaryMethodType(override val resultType: Type) extends SimpleTypeProxy {
  }
  
  object NullaryMethodType extends NullaryMethodTypeExtractor
  
  /** A type function or the type of a polymorphic value (and thus of kind *).
   *
   * Before the introduction of NullaryMethodType, a polymorphic nullary method (e.g, def isInstanceOf[T]: Boolean)
   * used to be typed as PolyType(tps, restpe), and a monomorphic one as PolyType(Nil, restpe)
   * This is now: PolyType(tps, NullaryMethodType(restpe)) and NullaryMethodType(restpe)
   * by symmetry to MethodTypes: PolyType(tps, MethodType(params, restpe)) and MethodType(params, restpe)
   *
   * Thus, a PolyType(tps, TypeRef(...)) unambiguously indicates a type function (which results from eta-expanding a type constructor alias).
   * Similarly, PolyType(tps, ClassInfoType(...)) is a type constructor.
   *
   * A polytype is of kind * iff its resultType is a (nullary) method type.
   */
  case class PolyType(override val typeParams: List[Symbol], override val resultType: Type)
       extends Type {
    override def params: List[Symbol] = resultType.params
  }
  
  object PolyType extends PolyTypeExtractor
  
  case class ExistentialType(quantified: List[Symbol],
                             override val underlying: Type) extends Type {
                               
  }
  
  object ExistentialType extends ExistentialTypeExtractor
  
  /** A type carrying some annotations. Created by the typechecker
   *  when eliminating ''Annotated'' trees (see typedAnnotated).
   *
   *  @param annotations the list of annotations on the type
   *  @param underlying the type without the annotation
   *  @param selfsym a "self" symbol with type `underlying`;
   *    only available if -Yself-in-annots is turned on. Can be `NoSymbol`
   *    if it is not used.
   */
  case class AnnotatedType(val annotations: List[AnnotationInfo],
                           override val underlying: Type,
                           val selfsym: Symbol) extends Type {
    
  }
  
  object AnnotatedType extends AnnotatedTypeExtractor
  
  
}
