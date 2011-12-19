package treehugger

import scala.annotation.tailrec

trait Types extends api.Types { self: Universe =>
  import definitions._
  
  abstract class AbsTypeImpl extends AbsType { this: Type =>
  }
  
  /** The base class for all types */
  abstract class Type extends AbsTypeImpl {
    /** Is this type higher-kinded, i.e., is it a type constructor @M */
    def isHigherKinded: Boolean = false    
    
    protected def objectPrefix = "object "
    protected def packagePrefix = "package "
    def trimPrefix(str: String) = str stripPrefix objectPrefix stripPrefix packagePrefix
    
    /** The string representation of this type used as a prefix */
    def prefixString = trimPrefix(toString) + "#"    
    
    /** Convert toString avoiding infinite recursions by cutting off
    *  after `maxTostringRecursions` recursion levels. Uses `safeToString`
    *  to produce a string on each level.
    */
    override def toString: String =
      if (tostringRecursions >= maxTostringRecursions)
       "..."
      else
       try {
         tostringRecursions += 1
         safeToString
       } finally {
         tostringRecursions -= 1
       }  
    
    /** Method to be implemented in subclasses.
     *  Converts this type to a string in calling toString for its parts.
     */
    def safeToString: String = super.toString
    
    /** The term symbol associated with the type
      * Note that the symbol of the normalized type is returned (@see normalize)
      */
    def termSymbol: Symbol = NoSymbol
    
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
        
    /** For a typeref or single-type, the prefix of the normalized type (@see normalize).
     *  NoType for all other types. */
    def prefix: Type = NoType
    
    /** A chain of all typeref or singletype prefixes of this type, longest first.
     *  (Only used from safeToString.)
     */
    def prefixChain: List[Type] = this match {
      case TypeRef(pre, _, _) => pre :: pre.prefixChain
      case SingleType(pre, _) => pre :: pre.prefixChain
      case _ => List()
    }
    
    /** For a classtype or refined type, its defined or declared members;
     *  inherited by subtypes and typerefs.
     *  The empty scope for all other types.
     */
    def decls: Scope = EmptyScope

    /** For a typeref, its arguments. The empty list for all other types */
    def typeArgs: List[Type] = List()
    
    /** For a (nullary) method or poly type, its direct result type,
     *  the type itself for all other types. */
    def resultType: Type = this
    
    /** For a curried/nullary method or poly type its non-method result type,
     *  the type itself for all other types */
    def finalResultType: Type = this
    
    /** For a method or poly type, its first value parameter section,
     *  the empty list for all other types */
    def params: List[Symbol] = List()
    
    /** For a (potentially wrapped) poly type, its type parameters,
     *  the empty list for all other types */
    def typeParams: List[Symbol] = List() 
    
    /** Replace formal type parameter symbols with actual type arguments.
     *
     * Amounts to substitution except for higher-kinded types. (See overridden method in TypeRef) -- @M
     */
    // def instantiateTypeParams(formals: List[Symbol], actuals: List[Type]): Type =
    //  if (sameLength(formals, actuals)) this.subst(formals, actuals) else ErrorType
      
    /** Reduce to beta eta-long normal form.
     *  Expands type aliases and converts higher-kinded TypeRefs to PolyTypes.
     *  Functions on types are also implemented as PolyTypes.
     *
     *  Example: (in the below, <List> is the type constructor of List)
     *    TypeRef(pre, <List>, List()) is replaced by
     *    PolyType(X, TypeRef(pre, <List>, List(X)))
     */
    def normalize = this // @MAT
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

  case class NotNullType(override val underlying: Type) extends SubType {
    def supertype = underlying
    override def safeToString: String = underlying.toString + " with NotNull"
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
    override def isHigherKinded = false // singleton type classifies objects, thus must be kind *
    override def safeToString: String = prefixString + "type"
  }
  
  /** An object representing an erroneous type */
  case object ErrorType extends Type {
    override def safeToString: String = "<error>"
  }
  
  /** An object representing an unknown type, used during type inference.
   *  If you see WildcardType outside of inference it is almost certainly a bug.
   */
  case object WildcardType extends Type {
    override def safeToString: String = "?"
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
    override def prefixString = ""
    // override def isNullable: Boolean = true
    // override def kind = "NoPrefixType"
  }
  
  /** A class for this-types of the form <sym>.this.type
   */
  abstract case class ThisType(sym: Symbol) extends SingletonType {
    override def isHigherKinded = sym.isRefinementClass && underlying.isHigherKinded
    override def prefixString =
      if (sym.isAnonOrRefinementClass) "this."
      else if (sym.isOmittablePrefix) ""
      else if (sym.isModuleClass) sym.fullName + "."
      else sym.nameString + ".this."
      
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
    override def termSymbol = sym
    override def prefix: Type = pre
    override def prefixString = (
      if (sym.skipPackageObject.isOmittablePrefix) ""
      else if (sym.isPackageObjectOrClass) pre.prefixString
      else pre.prefixString + sym.nameString + "."
    )
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
    override def prefix: Type = supertpe.prefix
    override def prefixString = thistpe.prefixString.replaceAll("""\bthis\.$""", "super.")
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
  
  /** A common base class for intersection types and class types
   */
  abstract class CompoundType extends Type {
    override def safeToString: String =
      parents.mkString(" with ")
  }
  
  /** A class representing intersection types with refinements of the form
   *    `<parents_0> with ... with <parents_n> { decls }`
   *  Cannot be created directly;
   *  one should always use `refinedType` for creation.
   */
  case class RefinedType(override val parents: List[Type],
                         override val decls: Scope) extends CompoundType {
    override def isHigherKinded = (
      parents.nonEmpty &&
      (parents forall (_.isHigherKinded))
    )
    
    override def typeParams =
      if (isHigherKinded) parents.head.typeParams
      else super.typeParams
      
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
  
  /** A class representing a constant type.
   *
   *  @param value ...
   */
  abstract case class ConstantType(value: Constant) extends SingletonType {
    // override def underlying: Type = value.tpe
    override def safeToString: String =
      underlying.toString + "(" + value.escapedStringValue + ")"
  }
  
  final class UniqueConstantType(value: Constant) extends ConstantType(value) with UniqueType {
  }

  object ConstantType extends ConstantTypeExtractor {
    def apply(value: Constant): ConstantType = {
      new UniqueConstantType(value).asInstanceOf[ConstantType]
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
    private var normalized: Type = null
    override def prefix: Type = pre
    override def typeArgs: List[Type] = args
    
    // @MAT was typeSymbol.unsafeTypeParams, but typeSymbol normalizes now
    private def typeParamsDirect = Nil // sym.unsafeTypeParams
      
    // (!result.isEmpty) IFF isHigherKinded
    override def typeParams: List[Symbol] = if (isHigherKinded) typeParamsDirect else List()
    
    // A reference (in a Scala program) to a type that has type
    // parameters, but where the reference does not include type
    // arguments. Note that it doesn't matter whether the symbol refers
    // to a java or scala symbol, but it does matter whether it occurs in
    // java or scala code. TypeRefs w/o type params that occur in java
    // signatures/code are considered raw types, and are represented as
    // existential types.
    override def isHigherKinded = args.isEmpty && typeParamsDirect.nonEmpty
    
    private def normalize0: Type = (
      if (pre eq WildcardType) WildcardType // arises when argument-dependent types are approximated (see def depoly in implicits)
      // else if (isHigherKinded) etaExpand   // eta-expand, subtyping relies on eta-expansion of higher-kinded types
      // else if (isBetaReducible) betaReduce.normalize // beta-reduce, but don't do partial application -- cycles have been checked in typeRef
      // else if (sym.isRefinementClass) sym.info.normalize // I think this is okay, but see #1241 (r12414), #2208, and typedTypeConstructor in Typers
      // else if (sym.isAliasType) ErrorType //println("!!error: "+(pre, sym, sym.info, sym.info.typeParams, args))
      else super.normalize
    )
    
    // TODO: test case that is compiled  in a specific order and in different runs
     override def normalize: Type = {
       if (normalized == null) {
         normalized = normalize0
       }
       normalized
     }

     private def preString = (
       // ensure that symbol is not a local copy with a name coincidence
       if (shorthands(sym.fullName) && sym.ownerChain.forall(_.isClass)) ""
       else pre.prefixString
     )
     private def argsString = if (args.isEmpty) "" else args.mkString("[", ",", "]")
     // private def refinementString = (
     //   if (sym.isStructuralRefinement) (
     //     decls filter (sym => sym.isPossibleInRefinement && sym.isPublic)
     //       map (_.defString)
     //       mkString(" {", "; ", "}")
     //   )
     //   else ""
     // )
     
    private def finishPrefix(rest: String) = (
      if (sym.isPackageClass) packagePrefix + rest
      else if (sym.isModuleClass) objectPrefix + rest
      // else if (!sym.isInitialized) rest
      // else if (sym.isAnonymousClass)
      //   thisInfo.parents.mkString("", " with ", refinementString)
      // else if (sym.isRefinementClass) "" + thisInfo
      else rest
    )
    
    private def customToString = this match {
      case TypeRef(_, RepeatedParamClass, arg :: _) => arg + "*"
      case TypeRef(_, ByNameParamClass, arg :: _)   => "=> " + arg
      case _ =>
        if (isFunctionType(this)) {
          val targs = normalize.typeArgs
          // Aesthetics: printing Function1 as T => R rather than (T) => R
          // ...but only if it's not a tuple, so ((T1, T2)) => R is distinguishable
          // from (T1, T2) => R.
          targs match {
            case in :: out :: Nil if !isTupleTypeOrSubtype(in)  =>
              "" + in + " => " + out
            case xs =>
              xs.init.mkString("(", ", ", ")") + " => " + xs.last
          }
        }
        else if (isTupleTypeOrSubtype(this))
          normalize.typeArgs.mkString("(", ", ", if (hasLength(normalize.typeArgs, 1)) ",)" else ")")
        else if (sym.isAliasType && prefixChain.exists(_.termSymbol.isSynthetic) && (normalize ne this))
          "" + normalize
        else
          ""
    }
    
    override def safeToString = {
      val custom = customToString
      if (custom != "") custom
      else finishPrefix(preString + sym.nameString + argsString)
    }
    
    override def prefixString = "" + (
      if (sym.isOmittablePrefix)
        ""
      else if (sym.isPackageClass || sym.isPackageObjectOrClass)
        sym.skipPackageObject.fullName + "."
      else if (nme.isSingletonName(sym.name))
        nme.dropSingletonName(sym.name) + "."
      else
        super.prefixString
    )
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
    override def finalResultType: Type = resultType.finalResultType
    // override def safeToString = paramString(this) + resultType
  }
  
  object MethodType extends MethodTypeExtractor
  
  class JavaMethodType(ps: List[Symbol], rt: Type) extends MethodType(ps, rt) {
    def isJava = true
  }
  
  case class NullaryMethodType(override val resultType: Type) extends SimpleTypeProxy {
    override def params            = Nil
    // override def paramTypes        = Nil
    override def safeToString      = "=> " + resultType
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
    override def finalResultType: Type = resultType.finalResultType
    
    override def isHigherKinded = !typeParams.isEmpty
    
    // typeParamsString is in debug
    // override def safeToString = typeParamsString(this) + resultType
  }
  
  object PolyType extends PolyTypeExtractor
  
  case class ExistentialType(quantified: List[Symbol],
                             override val underlying: Type) extends Type {
    override def isHigherKinded = false
    
    override def safeToString: String = {
      if (!(quantified exists (_.isSingletonExistential)))
        // try to represent with wildcards first
        underlying match {
          case TypeRef(pre, sym, args) if args.nonEmpty =>
            val wargs = Nil // wildcardArgsString(quantified.toSet, args)
            if (sameLength(wargs, args))
              return TypeRef(pre, sym, List()) + wargs.mkString("[", ", ", "]")
          case _ =>
        }
      var ustr = underlying.toString
      underlying match {
        case MethodType(_, _) | NullaryMethodType(_) | PolyType(_, _) => ustr = "("+ustr+")"
        case _ =>
      }
      val str =
        ustr+(quantified map (_.existentialToString) mkString(" forSome { ", "; ", " }"))
      str
    }
  }
  
  object ExistentialType extends ExistentialTypeExtractor

  /** A class containing the alternatives and type prefix of an overloaded symbol.
   *  Not used after phase `typer`.
   */
  case class OverloadedType(pre: Type, alternatives: List[Symbol]) extends Type {
    override def prefix: Type = pre
    // override def safeToString =
    //  (alternatives map pre.memberType).mkString("", " <and> ", "")
  }
  
  

  //@M
  // a TypeVar used to be a case class with only an origin and a constr
  // then, constr became mutable (to support UndoLog, I guess),
  // but pattern-matching returned the original constr0 (a bug)
  // now, pattern-matching returns the most recent constr
  object TypeVar {
    def unapply(tv: TypeVar): Some[(Type, TypeConstraint)] = Some((tv.origin, tv.constr))
    def apply(origin: Type, constr: TypeConstraint) = new TypeVar(origin, constr, List(), List())
    // TODO why not initialise TypeConstraint with bounds of tparam?
    // @PP: I tried that, didn't work out so well for me.
    def apply(tparam: Symbol) = new TypeVar(tparam.tpeHK, new TypeConstraint, List(), List())
    def apply(origin: Type, constr: TypeConstraint, args: List[Type], params: List[Symbol]) =
      new TypeVar(origin, constr, args, params)
  }
  
  /** A class representing a type variable: not used after phase `typer`.
   *
   *  A higher-kinded TypeVar has params (Symbols) and typeArgs (Types).
   *  A TypeVar with nonEmpty typeArgs can only be instantiated by a higher-kinded
   *  type that can be applied to those args.  A TypeVar is much like a TypeRef,
   *  except it has special logic for equality and subtyping.
   */
  class TypeVar(
    val origin: Type,
    val constr0: TypeConstraint,
    override val typeArgs: List[Type],
    override val params: List[Symbol]
  ) extends Type {
    private def levelString = "" // if (settings.explaintypes.value) level else ""
    override def safeToString = constr.inst match {
      case null   => "<null " + origin + ">"
      case NoType => "?" + levelString + origin + this.toString
      case x      => "" + x
    }
    
    /** The constraint associated with the variable */
    var constr = constr0
    
    /** Two occurrences of a higher-kinded typevar, e.g. `?CC[Int]` and `?CC[String]`, correspond to
     *  ''two instances'' of `TypeVar` that share the ''same'' `TypeConstraint`.
     *
     *  `constr` for `?CC` only tracks type constructors anyway,
     *   so when `?CC[Int] <:< List[Int]` and `?CC[String] <:< Iterable[String]`
     *  `?CC's` hibounds contains List and Iterable.
     */
    def applyArgs(newArgs: List[Type]): TypeVar =
      if (newArgs.isEmpty) this // SubstMap relies on this (though this check is redundant when called from appliedType...)
      else TypeVar(origin, constr, newArgs, params) // @M TODO: interaction with undoLog??
  }
  
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
    override def safeToString = annotations.mkString(underlying + " @", " @", "")
  }

  /** Creator for AnnotatedTypes.  It returns the underlying type if annotations.isEmpty
   *  rather than walking into the assertion.
   */
  def annotatedType(annots: List[AnnotationInfo], underlying: Type, selfsym: Symbol = NoSymbol): Type =
    if (annots.isEmpty) underlying
    else AnnotatedType(annots, underlying, selfsym)
  
  object AnnotatedType extends AnnotatedTypeExtractor
  
  /** A class representing types with a name. When an application uses
   *  named arguments, the named argument types for calling isApplicable
   *  are represented as NamedType.
   */
  case class NamedType(name: Name, tp: Type) extends Type {
    override def safeToString: String = name.toString +": "+ tp
  }
  
  
// Creators ---------------------------------------------------------------
  
  /** The canonical creator for single-types */
  def singleType(pre: Type, sym: Symbol): Type = {
    if (sym.isRootPackage) ThisType(RootClass)
    else SingleType(pre, sym)
  }
  
  def typeRef(sym: Symbol): Type = TypeRef(NoPrefix, sym, Nil)
  
  def typeRef(pre: Type, sym: Symbol, args: List[Type]): Type = TypeRef(pre, sym, args)

  // Optimization to avoid creating unnecessary new typerefs.
  def copyTypeRef(tp: Type, pre: Type, sym: Symbol, args: List[Type]): Type = tp match {
    case TypeRef(pre0, sym0, _) if pre == pre0 && sym0.name == sym.name =>
      TypeRef(pre, sym, args)
    case _ =>
      typeRef(pre, sym, args)
  }

  /** A creator for type applications */
  def appliedType(tycon: Type, args: List[Type]): Type =
    if (args.isEmpty) tycon //@M! `if (args.isEmpty) tycon' is crucial (otherwise we create new types in phases after typer and then they don't get adapted (??))
    else tycon match {
      case TypeRef(pre, sym @ (NothingClass|AnyClass), _) => copyTypeRef(tycon, pre, sym, Nil)   //@M drop type args to Any/Nothing
      case TypeRef(pre, sym, _)                           => copyTypeRef(tycon, pre, sym, args)
      case PolyType(tparams, restpe)                      => PolyType(tparams, restpe) // args
      case ExistentialType(tparams, restpe)               => ExistentialType(tparams, appliedType(restpe, args))
      case st: SingletonType                              => appliedType(st.widen, args) // @M TODO: what to do? see bug1
      case RefinedType(parents, decls)                    => RefinedType(parents map (appliedType(_, args)), decls) // MO to AM: please check
      case TypeBounds(lo, hi)                             => TypeBounds(appliedType(lo, args), appliedType(hi, args))
      case tv@TypeVar(_, _)                               => tv.applyArgs(args)
      case AnnotatedType(annots, underlying, self)        => AnnotatedType(annots, appliedType(underlying, args), self)
      case ErrorType                                      => tycon
      case WildcardType                                   => tycon // needed for neg/t0226
      case _                                              => error(tycon.toString)
    }

// Helper Classes ---------------------------------------------------------

    /** A class expressing upper and lower bounds constraints of type variables,
     * as well as their instantiations.
     */
    class TypeConstraint(lo0: List[Type], hi0: List[Type], numlo0: Type, numhi0: Type, avoidWidening0: Boolean = false) {
      def this(lo0: List[Type], hi0: List[Type]) = this(lo0, hi0, NoType, NoType)
      def this() = this(List(), List())
      
      private var lobounds = lo0
      private var hibounds = hi0
      private var numlo = numlo0
      private var numhi = numhi0
      private var avoidWidening = avoidWidening0

      def loBounds: List[Type] = if (numlo == NoType) lobounds else numlo :: lobounds
      def hiBounds: List[Type] = if (numhi == NoType) hibounds else numhi :: hibounds
      
      var inst: Type = NoType // @M reduce visibility?
      
      override def toString =
        (loBounds map (_.safeToString)).mkString("[ _>:(", ",", ") ") +
        (hiBounds map (_.safeToString)).mkString("| _<:(", ",", ") ] _= ") +
        inst.safeToString
    }
    
// Helper Methods  -------------------------------------------------------------

  /** True if two lists have the same length.  Since calling length on linear sequences
   *  is O(n), it is an inadvisable way to test length equality.
   */
  final def sameLength(xs1: List[_], xs2: List[_]) = compareLengths(xs1, xs2) == 0
  @tailrec final def compareLengths(xs1: List[_], xs2: List[_]): Int =
    if (xs1.isEmpty) { if (xs2.isEmpty) 0 else -1 }
    else if (xs2.isEmpty) 1
    else compareLengths(xs1.tail, xs2.tail)
  
  /** Again avoiding calling length, but the lengthCompare interface is clunky.
   */
  final def hasLength(xs: List[_], len: Int) = xs.lengthCompare(len) == 0

  val shorthands = Set(
    "scala.collection.immutable.List",
    "scala.collection.immutable.Nil",
    "scala.collection.Seq",
    "scala.collection.Traversable",
    "scala.collection.Iterable",
    "scala.collection.mutable.StringBuilder",
    "scala.collection.IndexedSeq",
    "scala.collection.Iterator")
  
  /** The maximum number of recursions allowed in toString
   */
  final val maxTostringRecursions = 50
  
  private var tostringRecursions = 0
}
