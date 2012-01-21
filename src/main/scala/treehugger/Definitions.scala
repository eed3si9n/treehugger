package treehugger

import Flags._
import PartialFunction._

trait Definitions extends api.StandardDefinitions { self: Forest =>
  // the scala value classes
  trait ValueClassDefinitions { self: definitions.type =>
    import scala.collection.mutable
    
    lazy val symbolCache: mutable.Map[Name, Symbol] =
      mutable.Map(nme.scala_ -> RootClass.newPackage(NoPosition, nme.scala_).setFlag(FINAL))    
    
    private[Definitions] def cacheSymbol(owner: Symbol, name: Name) =
      symbolCache.getOrElseUpdate(name,
        if (name.isTypeName) owner.newClass(name.toTypeName)
        else owner.newModule(name.toTermName))
    
    private[Definitions] def valueCache(name: Name) = cacheSymbol(ScalaPackageClass, name)
        
    private val nameToWeight = Map[Name, Int](
      tpnme.Byte   -> 2,
      tpnme.Char   -> 3,
      tpnme.Short  -> 4,
      tpnme.Int    -> 12,
      tpnme.Long   -> 24,
      tpnme.Float  -> 48,
      tpnme.Double -> 96
    )
    
    private def classesMap[T](f: Name => T) = symbolsMap(ScalaValueClassesNoUnit, f)
    private def symbolsMap[T](syms: List[Symbol], f: Name => T): Map[Symbol, T] = syms zip (syms map (x => f(x.name))) toMap
    private def symbolsMapFilt[T](syms: List[Symbol], p: Name => Boolean, f: Name => T) = symbolsMap(syms filter (x => p(x.name)), f)    
    
    lazy val numericWeight    = symbolsMapFilt(ScalaValueClasses, nameToWeight.keySet, nameToWeight)
    
    private lazy val scalaValueClassesSet = ScalaValueClasses.toSet
    
    /** Is symbol a value class? */
    def isValueClass(sym: Symbol) = scalaValueClassesSet(sym)
    
    /** Is symbol a numeric value class? */
    def isNumericValueClass(sym: Symbol): Boolean =
      numericWeight contains sym
    
    lazy val AnyValClass  = valueCache(tpnme.AnyVal)
    lazy val UnitClass    = valueCache(tpnme.Unit)
    lazy val ByteClass    = valueCache(tpnme.Byte)
    lazy val ShortClass   = valueCache(tpnme.Short)
    lazy val CharClass    = valueCache(tpnme.Char)
    lazy val IntClass     = valueCache(tpnme.Int)
    lazy val LongClass    = valueCache(tpnme.Long)
    lazy val FloatClass   = valueCache(tpnme.Float)
    lazy val DoubleClass  = valueCache(tpnme.Double)
    lazy val BooleanClass = valueCache(tpnme.Boolean)      
      def Boolean_and = newMethod(BooleanClass, nme.ZAND)
      def Boolean_or  = newMethod(BooleanClass, nme.ZOR)
      def Boolean_not = newMethod(BooleanClass, nme.UNARY_!)
    
    def ScalaValueClassesNoUnit = ScalaValueClasses filterNot (_ eq UnitClass)
    def ScalaValueClasses: List[Symbol] = List(
      UnitClass,
      BooleanClass,
      ByteClass,
      ShortClass,
      CharClass,
      IntClass,
      LongClass,
      FloatClass,
      DoubleClass
    )
    
    private def newMethod(owner: Symbol, name: TermName): Symbol = {
      val msym = owner.newMethod(NoPosition, name)
      // owner.info.decls.enter(msym)
      msym
    }    
  }
  
  object definitions extends AbsDefinitions with ValueClassDefinitions {
    private var isInitialized = false
    def isDefinitionsInitialized = isInitialized
    
    // This is the actual root of everything, including the package _root_.
    lazy val RootClass: ModuleClassSymbol = (
      NoSymbol.newModuleClass(NoPosition, tpnme.ROOT)
        setFlag (FINAL | MODULE | PACKAGE | JAVA)
    )
    // The empty package, which holds all top level types without given packages.
    lazy val EmptyPackage       = RootClass.newPackage(NoPosition, nme.EMPTY_PACKAGE_NAME).setFlag(FINAL)
    lazy val EmptyPackageClass  = EmptyPackage.moduleClass

    lazy val JavaLangPackage      = getModule(sn.JavaLang)
    lazy val JavaLangPackageClass = JavaLangPackage.moduleClass
    lazy val ScalaPackage         = getModule(nme.scala_)
    lazy val ScalaPackageClass    = ScalaPackage.moduleClass
    
    // convenient one-argument parameter lists
    lazy val anyparam     = List(AnyClass.typeConstructor)
    lazy val anyvalparam  = List(AnyValClass.typeConstructor)
    lazy val anyrefparam  = List(AnyRefClass.typeConstructor)
    
    // private parameter conveniences
    private def booltype    = BooleanClass.typeConstructor
    private def inttype     = IntClass.typeConstructor
    private def stringtype  = StringClass.typeConstructor
    
    // top types
    lazy val AnyClass             = newClass(ScalaPackageClass, tpnme.Any, Nil) setFlag (ABSTRACT)
    lazy val AnyRefClass          = newClass(ScalaPackageClass, tpnme.AnyRef, Nil) // newAlias(ScalaPackageClass, tpnme.AnyRef, ObjectClass.typeConstructor)
    lazy val ObjectClass          = getClass(sn.Object)
    lazy val AnyCompanionClass    = getClass("scala.AnyCompanion") setFlag (SEALED | ABSTRACT | TRAIT)
    lazy val AnyValCompanionClass = getClass("scala.AnyValCompanion") setFlag (SEALED | ABSTRACT | TRAIT)
    
    // bottom types
    final val SCALA_NOTHING = "scala.runtime.Nothing$"
    final val SCALA_NULL = "scala.runtime.Null$"
    lazy val RuntimeNothingClass  = getClass(SCALA_NOTHING)
    lazy val RuntimeNullClass     = getClass(SCALA_NULL)
    
    sealed abstract class BottomClassSymbol(name: TypeName, parent: Symbol) extends ClassSymbol(ScalaPackageClass, NoPosition, name) {
      this setFlag ABSTRACT | TRAIT | FINAL
      // final override def isBottomClass = true
    }
    final object NothingClass extends BottomClassSymbol(tpnme.Nothing, AnyClass) {
      override def isSubClass(that: Symbol) = true
    }
    final object NullClass extends BottomClassSymbol(tpnme.Null, AnyRefClass) {
      override def isSubClass(that: Symbol) = (
           (that eq AnyClass)
        || (that ne NothingClass) && (that isSubClass ObjectClass)
      )      
    }
    
    // exceptions and other throwables
    lazy val ClassCastExceptionClass        = getClass("java.lang.ClassCastException")
    lazy val IllegalArgumentExceptionClass  = getClass("java.lang.IllegalArgumentException")
    lazy val IndexOutOfBoundsExceptionClass = getClass(sn.IOOBException)
    lazy val InvocationTargetExceptionClass = getClass(sn.InvTargetException)
    lazy val MatchErrorClass                = getClass("scala.MatchError")
    lazy val NonLocalReturnControlClass     = getClass("scala.runtime.NonLocalReturnControl")
    lazy val NullPointerExceptionClass      = getClass(sn.NPException)
    lazy val ThrowableClass                 = getClass(sn.Throwable)
    lazy val UninitializedErrorClass        = getClass("scala.UninitializedFieldError")    
    
    // fundamental reference classes
    // lazy val ScalaObjectClass           = getMember(ScalaPackageClass, tpnme.ScalaObject)
    lazy val PartialFunctionClass       = getClass("scala.PartialFunction")
    lazy val AbstractPartialFunctionClass = getClass("scala.runtime.AbstractPartialFunction")
    lazy val SymbolClass                = getClass("scala.Symbol")
    lazy val StringClass                = getClass(sn.String)
    // lazy val StringModule               = StringClass.linkedClassOfClass
    lazy val ClassClass                 = getClass(sn.Class)
    //  def Class_getMethod               = getMember(ClassClass, nme.getMethod_)
    lazy val DynamicClass               = getClass("scala.Dynamic")
    
    // fundamental modules
    // lazy val SysPackage = getPackageObject("scala.sys")
    
    // Modules whose members are in the default namespace
    lazy val UnqualifiedModules = List(PredefModule, ScalaPackage, JavaLangPackage)
    // Those modules and their module classes
    lazy val UnqualifiedOwners  = UnqualifiedModules.toSet ++ UnqualifiedModules.map(_.moduleClass)
    
    lazy val PredefModule: Symbol = getModule("scala.Predef")
    lazy val PredefModuleClass = PredefModule.moduleClass

    // classes with special meanings
    lazy val StringAddClass   = getClass("scala.runtime.StringAdd")
    lazy val ArrowAssocClass  = getClass("scala.Predef.ArrowAssoc")
    // lazy val StringAdd_+      = getMember(StringAddClass, nme.PLUS)
    lazy val NotNullClass     = getClass("scala.NotNull")
    lazy val ScalaNumberClass           = getClass("scala.math.ScalaNumber")
    lazy val TraitSetterAnnotationClass = getClass("scala.runtime.TraitSetter")
    lazy val DelayedInitClass = getClass("scala.DelayedInit")
      // def delayedInitMethod = getMember(DelayedInitClass, nme.delayedInit)
      
      // a dummy value that communicates that a delayedInit call is compiler-generated
      // from phase UnCurry to phase Constructors
      // !!! This is not used anywhere (it was checked in that way.)
      // def delayedInitArgVal = EmptyPackageClass.newValue(NoPosition, nme.delayedInitArg)
      //   .setInfo(UnitClass.tpe)

    lazy val TypeConstraintClass   = getClass("scala.annotation.TypeConstraint")
    lazy val SingletonClass        = newClass(ScalaPackageClass, tpnme.Singleton, anyparam) setFlag (ABSTRACT | TRAIT | FINAL)
    lazy val SerializableClass     = getClass("scala.Serializable")
    lazy val JavaSerializableClass = getClass(sn.JavaSerializable)
    lazy val ComparableClass       = getClass("java.lang.Comparable")
    lazy val JavaCloneableClass    = getClass("java.lang.Cloneable")
    lazy val RemoteInterfaceClass  = getClass("java.rmi.Remote")
    lazy val RemoteExceptionClass  = getClass("java.rmi.RemoteException")

    lazy val RepeatedParamClass = newCovariantPolyClass(
      ScalaPackageClass,
      tpnme.REPEATED_PARAM_CLASS_NAME,
      tparam => seqType(tparam.typeConstructor)
    )

    lazy val JavaRepeatedParamClass = newCovariantPolyClass(
      ScalaPackageClass,
      tpnme.JAVA_REPEATED_PARAM_CLASS_NAME,
      tparam => arrayType(tparam.typeConstructor)
    )

    def isByNameParamType(tp: Type)        = tp.typeSymbol == ByNameParamClass
    def isScalaRepeatedParamType(tp: Type) = tp.typeSymbol == RepeatedParamClass
    def isJavaRepeatedParamType(tp: Type)  = tp.typeSymbol == JavaRepeatedParamClass
    def isRepeatedParamType(tp: Type)      = isScalaRepeatedParamType(tp) || isJavaRepeatedParamType(tp)
    def isCastSymbol(sym: Symbol)          = sym == Any_asInstanceOf || sym == Object_asInstanceOf

    def isJavaVarArgs(params: List[Symbol])  = params.nonEmpty && isJavaRepeatedParamType(params.last.tpe)
    def isScalaVarArgs(params: List[Symbol]) = params.nonEmpty && isScalaRepeatedParamType(params.last.tpe)
    def isVarArgsList(params: List[Symbol])  = params.nonEmpty && isRepeatedParamType(params.last.tpe)
    def isVarArgTypes(formals: List[Type])   = formals.nonEmpty && isRepeatedParamType(formals.last)

    def hasRepeatedParam(tp: Type): Boolean = tp match {
      case MethodType(formals, restpe) => isScalaVarArgs(formals) || hasRepeatedParam(restpe)
      case PolyType(_, restpe)         => hasRepeatedParam(restpe)
      case _                           => false
    }

    def isPrimitiveArray(tp: Type) = tp match {
      case TypeRef(_, ArrayClass, arg :: Nil) => isValueClass(arg.typeSymbol)
      case _                                  => false
    }
    def isArrayOfSymbol(tp: Type, elem: Symbol) = tp match {
      case TypeRef(_, ArrayClass, arg :: Nil) => arg.typeSymbol == elem
      case _                                  => false
    }

    lazy val ByNameParamClass = newCovariantPolyClass(
      ScalaPackageClass,
      tpnme.BYNAME_PARAM_CLASS_NAME,
      tparam => AnyClass.typeConstructor
    )
    lazy val EqualsPatternClass = {
      val clazz = newClass(ScalaPackageClass, tpnme.EQUALS_PATTERN_NAME, Nil)
      // clazz setInfo polyType(List(newTypeParam(clazz, 0)), ClassInfoType(anyparam, new Scope, clazz))
      clazz
    }
    
    // collections classes
    lazy val ConsClass          = getClass("scala.collection.immutable.$colon$colon")
    lazy val IterableClass      = getClass("scala.collection.Iterable")
    lazy val IteratorClass      = getClass("scala.collection.Iterator")
    lazy val ListClass          = getClass("scala.collection.immutable.List")
    lazy val SeqClass           = getClass("scala.collection.Seq")
    lazy val MapClass           = getClass("scala.collection.Map")
    lazy val StringBuilderClass = getClass("scala.collection.mutable.StringBuilder")
    lazy val TraversableClass   = getClass("scala.collection.Traversable")
    lazy val ArrayBufferClass   = getClass("scala.collection.mutable.ArrayBuffer")
    lazy val MutableMapClass    = getClass("scala.collection.mutable.Map")
     
    lazy val ListModule       = getModule("scala.collection.immutable.List")
    //  lazy val List_apply = getMember(ListModule, nme.apply)
    lazy val NilModule        = getModule("scala.collection.immutable.Nil")
    lazy val SeqModule        = getModule("scala.collection.Seq")
    
    // arrays and their members
    lazy val ArrayModule  = getModule("scala.Array")
    lazy val ArrayClass   = getClass("scala.Array")
    
    // Option classes
    lazy val OptionClass: Symbol = getClass("scala.Option")
    lazy val SomeClass: Symbol   = getClass("scala.Some")
    lazy val NoneModule: Symbol  = getModule("scala.None")
    lazy val SomeModule: Symbol  = getModule("scala.Some")
    
    def isOptionType(tp: Type)  = cond(tp.normalize) { case TypeRef(_, OptionClass, List(_)) => true }
    def isSomeType(tp: Type)    = cond(tp.normalize) { case TypeRef(_,   SomeClass, List(_)) => true }
    def isNoneType(tp: Type)    = cond(tp.normalize) { case TypeRef(_,   NoneModule, List(_)) => true }

    def optionType(tp: Type)    = typeRef(NoPrefix, OptionClass, List(tp))
    def someType(tp: Type)      = typeRef(NoPrefix, SomeClass, List(tp))
    def symbolType              = typeRef(SymbolClass.typeConstructor.prefix, SymbolClass, List())
    def longType                = typeRef(LongClass.typeConstructor.prefix, LongClass, List())
    
    // Product, Tuple, Function
    private def mkArityArray(name: String, arity: Int, countFrom: Int = 1): Array[Symbol] = {
      val list = countFrom to arity map (i => getClass("scala." + name + i))
      if (countFrom == 0) list.toArray
      else (NoSymbol +: list).toArray
    }

    val MaxTupleArity, MaxProductArity, MaxFunctionArity = 22
    /** The maximal dimensions of a generic array creation.
     *  I.e. new Array[Array[Array[Array[Array[T]]]]] creates a 5 times
     *  nested array. More is not allowed.
     */
    val MaxArrayDims = 5
    lazy val TupleClass     = mkArityArray("Tuple", MaxTupleArity)
    lazy val ProductClass   = mkArityArray("Product", MaxProductArity)
    lazy val FunctionClass  = mkArityArray("Function", MaxFunctionArity, 0)
    lazy val AbstractFunctionClass = mkArityArray("runtime.AbstractFunction", MaxFunctionArity, 0)

    def tupleField(n: Int, j: Int) = getMember(TupleClass(n), "_" + j)
    def isTupleType(tp: Type): Boolean = isTupleType(tp, false)
    def isTupleTypeOrSubtype(tp: Type): Boolean = isTupleType(tp, true)
    private def isTupleType(tp: Type, subtypeOK: Boolean) = tp.normalize match {
      case TypeRef(_, sym, args) if args.nonEmpty =>
        val len = args.length
        len <= MaxTupleArity && {
          val tsym = TupleClass(len)
          (sym == tsym) || (subtypeOK && !tp.isHigherKinded && sym.isSubClass(tsym))
        }
      case _ => false
    }

    def tupleType(elems: List[Type]) = {
      val len = elems.length
      if (len <= MaxTupleArity) {
        val sym = TupleClass(len)
        typeRef(sym.typeConstructor.prefix, sym, elems)
      } else NoType
    }

    def unapplyUnwrap(tpe:Type) = tpe.finalResultType.normalize match {
      case RefinedType(p :: _, _) => p.normalize
      case tp                     => tp
    }

    def functionApply(n: Int) = getMember(FunctionClass(n), nme.apply)
    def functionType(formals: List[Type], restpe: Type) = {
      val len = formals.length
      if (len <= MaxFunctionArity) {
        val sym = FunctionClass(len)
        typeRef(sym.typeConstructor.prefix, sym, formals :+ restpe)
      } else NoType
    }

    def abstractFunctionForFunctionType(tp: Type) = tp.normalize match {
      case tr @ TypeRef(_, _, args) if isFunctionType(tr) =>
        val sym = AbstractFunctionClass(args.length - 1)
        typeRef(sym.typeConstructor.prefix, sym, args)
      case _ =>
        NoType
    }

    def isFunctionType(tp: Type): Boolean = tp.normalize match {
      case TypeRef(_, sym, args) if args.nonEmpty =>
        val len = args.length
        len < MaxFunctionArity && sym == FunctionClass(len - 1)
      case _ =>
        false
    }

    def isSeqType(tp: Type) = elementType(SeqClass, tp.normalize) != NoType

    def elementType(container: Symbol, tp: Type): Type = tp match {
      case TypeRef(_, `container`, arg :: Nil)  => arg
      case _                                    => NoType
    }
    
    def seqType(arg: Type)    = appliedType(SeqClass.typeConstructor, List(arg))
    def arrayType(arg: Type)  = appliedType(ArrayClass.typeConstructor, List(arg))
    def listType(arg: Type)   = appliedType(ListClass.typeConstructor, List(arg))
    def byNameType(arg: Type) = appliedType(ByNameParamClass.typeConstructor, List(arg))
    def mapType(arg1: Type, arg2: Type) = appliedType(MapClass.typeConstructor, List(arg1, arg2))
    def mutableMapType(arg1: Type, arg2: Type) = appliedType(MutableMapClass.typeConstructor, List(arg1, arg2))
    
    // def ClassType(arg: Type) = appliedType(ClassClass.typeConstructor, List(arg))
    
    // generalized type constraints
    lazy val TpEqualsType         = getClass("=:=")
    lazy val ConformsType         = getClass("<:<")
    lazy val ConformsOrViewAsType = getClass("<%<")
    
    def tpEqualsType(arg1: Type, arg2: Type)   = appliedType(TpEqualsType.typeConstructor, List(arg1, arg2))
    def conformsType(arg1: Type, arg2: Type)   = appliedType(ConformsType.typeConstructor, List(arg1, arg2))
    def conformsOrViewAsType(arg1: Type, arg2: Type)   = appliedType(ConformsOrViewAsType.typeConstructor, List(arg1, arg2))
    
    // typeclass classes
    lazy val EquivClass           = getClass("scala.Equiv")
    lazy val FractionalClass      = getClass("scala.Fractional")
    lazy val NumericClass         = getClass("scala.Numeric")
    lazy val OrderedClass         = getClass("scala.Ordered")
    lazy val OrderingClass        = getClass("scala.Ordering")
    lazy val PartialOrderingClas  = getClass("scala.PartialOrdering")
    
    def equivType(arg: Type)      = appliedType(EquivClass.typeConstructor, List(arg))
    def fractionalType(arg: Type) = appliedType(FractionalClass.typeConstructor, List(arg))
    def numericType(arg: Type)    = appliedType(NumericClass.typeConstructor, List(arg))
    def orderedType(arg: Type)    = appliedType(OrderedClass.typeConstructor, List(arg))
    def orderingType(arg: Type)   = appliedType(OrderingClass.typeConstructor, List(arg))
    def partialOrderingType(arg: Type) = appliedType(PartialOrderingClas.typeConstructor, List(arg))
    
    // members of class scala.Any
    var Any_==          : Symbol = _
    var Any_!=          : Symbol = _
    var Any_equals      : Symbol = _
    var Any_hashCode    : Symbol = _
    var Any_toString    : Symbol = _
    var Any_getClass    : Symbol = _
    var Any_isInstanceOf: Symbol = _
    var Any_asInstanceOf: Symbol = _
    var Any_##          : Symbol = _

    // members of class java.lang.{Object, String}
    var Object_eq          : Symbol = _
    var Object_ne          : Symbol = _
    var Object_==          : Symbol = _
    var Object_!=          : Symbol = _
    var Object_##          : Symbol = _
    var Object_synchronized: Symbol = _    
    lazy val Object_isInstanceOf = newPolyMethod(
      ObjectClass, "$isInstanceOf",
      tparam => MethodType(List(), booltype)) setFlag (FINAL | SYNTHETIC)
    lazy val Object_asInstanceOf = newPolyMethod(
      ObjectClass, "$asInstanceOf",
      tparam => MethodType(List(), tparam.typeConstructor)) setFlag (FINAL | SYNTHETIC)
    
    def Object_getClass  = getMember(ObjectClass, nme.getClass_)
    def Object_clone     = getMember(ObjectClass, nme.clone_)
    def Object_finalize  = getMember(ObjectClass, nme.finalize_)
    def Object_notify    = getMember(ObjectClass, nme.notify_)
    def Object_notifyAll = getMember(ObjectClass, nme.notifyAll_)
    def Object_equals    = getMember(ObjectClass, nme.equals_)
    def Object_hashCode  = getMember(ObjectClass, nme.hashCode_)
    def Object_toString  = getMember(ObjectClass, nme.toString_)
    
    def init() {
      if (isInitialized) return
      
      // members of class scala.Any
      Any_== = newMethod(AnyClass, nme.EQ, anyparam, booltype) setFlag FINAL
      Any_!= = newMethod(AnyClass, nme.NE, anyparam, booltype) setFlag FINAL
      Any_equals   = newMethod(AnyClass, nme.equals_, anyparam, booltype)
      Any_hashCode = newMethod(AnyClass, nme.hashCode_, Nil, inttype)
      Any_toString = newMethod(AnyClass, nme.toString_, Nil, stringtype)
      Any_##       = newMethod(AnyClass, nme.HASHHASH, Nil, inttype) setFlag FINAL
      
      Any_getClass = (
        newMethod(AnyClass, nme.getClass_, Nil, NoType)
          setFlag DEFERRED
      )
      Any_isInstanceOf = newPolyMethod(
        AnyClass, nme.isInstanceOf_, tparam => NullaryMethodType(booltype)) setFlag FINAL
      Any_asInstanceOf = newPolyMethod(
        AnyClass, nme.asInstanceOf_, tparam => NullaryMethodType(tparam.typeConstructor)) setFlag FINAL
      
      // members of class java.lang.{ Object, String }
      Object_## = newMethod(ObjectClass, nme.HASHHASH, Nil, inttype) setFlag FINAL
      Object_== = newMethod(ObjectClass, nme.EQ, anyrefparam, booltype) setFlag FINAL
      Object_!= = newMethod(ObjectClass, nme.NE, anyrefparam, booltype) setFlag FINAL
      Object_eq = newMethod(ObjectClass, nme.eq, anyrefparam, booltype) setFlag FINAL
      Object_ne = newMethod(ObjectClass, nme.ne, anyrefparam, booltype) setFlag FINAL
      Object_synchronized = newPolyMethodCon(
        ObjectClass, nme.synchronized_,
        tparam => msym => MethodType(msym.newSyntheticValueParams(List(tparam.typeConstructor)), tparam.typeConstructor)) setFlag FINAL
    }
    
    def getClass(fullname: Name): Symbol = getModuleOrClass(fullname.toTypeName)
    
    def getModule(fullname: Name): Symbol = getModuleOrClass(fullname.toTermName)
    
    private def getModuleOrClass(path: Name, len: Int): Symbol =
      symbolCache.getOrElseUpdate(path, {
        val point = path lastPos('.', len - 1)
        val owner =
          if (point > 0) getModuleOrClass(path.toTermName, point).moduleClass
          else RootClass
        val name = path subName (point + 1, len)
        if (path.isTypeName) owner.newClass(name.toTypeName)
        else owner.newModule(name.toTermName)
      })
    
    def getMember(owner: Symbol, name: Name): Symbol = {
      if (owner == NoSymbol) NoSymbol
      else symbolCache.getOrElseUpdate(owner.fullName + "." + name.toString, {
        newMethod(owner, name)
      })
    }
    
    /** If you're looking for a class, pass a type name.
     *  If a module, a term name.
     */
    private def getModuleOrClass(path: Name): Symbol = getModuleOrClass(path, path.length)
    
    private def newClass(owner: Symbol, name: TypeName, parents: List[Type]): Symbol = {
      val clazz = owner.newClass(NoPosition, name)
      // clazz.setInfo(ClassInfoType(parents, new Scope, clazz))
      // owner.info.decls.enter(clazz)
      clazz
    }
    
    private def newCovariantPolyClass(owner: Symbol, name: TypeName, parent: Symbol => Type): Symbol = {
      val clazz  = newClass(owner, name, List())
      // val tparam = newTypeParam(clazz, 0) setFlag COVARIANT
      // val p      = parent(tparam)
      // clazz.setInfo(
      //   polyType(
      //     List(tparam),
      //     ClassInfoType(List(AnyRefClass.tpe, p), new Scope, clazz)))
      clazz
    }
    
    private def newAlias(owner: Symbol, name: TypeName, alias: Type): Symbol = {
      val tpsym = owner.newAliasType(NoPosition, name)
      // tpsym.setInfo(alias)
      // owner.info.decls.enter(tpsym)
      tpsym
    }
    
    private def newMethod(owner: Symbol, name: TermName): Symbol = {
      val msym = owner.newMethod(NoPosition, name)
      // owner.info.decls.enter(msym)
      msym
    }
    
    private[Definitions] def newMethod(owner: Symbol, name: TermName, formals: List[Type], restpe: Type): Symbol = {
      val msym = newMethod(owner, name)
      val params = msym.newSyntheticValueParams(formals)
      // msym.setInfo(MethodType(params, restpe))
      msym
    }
    
    /** tcon receives the type parameter symbol as argument */
    private def newPolyMethod(owner: Symbol, name: TermName, tcon: Symbol => Type): Symbol =
      newPolyMethodCon(owner, name, tparam => msym => tcon(tparam))

    /** tcon receives the type parameter symbol and the method symbol as arguments */
    private def newPolyMethodCon(owner: Symbol, name: TermName, tcon: Symbol => Symbol => Type): Symbol = {
      val msym = newMethod(owner, name)
      // val tparam = newTypeParam(msym, 0)
      // msym.setInfo(polyType(List(tparam), tcon(tparam)(msym)))
      msym
    }
    
    // todo: reconcile with javaSignature!!!
    def signature(tp: Type): String = {
      val NAME_JOIN_STRING = "$"
      def erasure(tp: Type): Type = tp match {
        // case st: SubType => erasure(st.supertype)
        // case RefinedType(parents, _) => erasure(parents.head)
        case _ => tp
      }
      def flatNameString(sym: Symbol, separator: Char): String =
        if (sym == NoSymbol) ""   // be more resistant to error conditions, e.g. neg/t3222.scala
        // else if (sym.owner.isPackageClass) sym.javaClassName
        else flatNameString(sym.owner, separator) + NAME_JOIN_STRING + sym.simpleName
      // def signature1(etp: Type): String = {
      //   if (etp.typeSymbol == ArrayClass) "[" + signature1(erasure(etp.normalize.typeArgs.head))
      //   else if (isValueClass(etp.typeSymbol)) abbrvTag(etp.typeSymbol).toString()
      //   else "L" + flatNameString(etp.typeSymbol, '/') + ";"
      // }
      val etp = erasure(tp)
      // if (etp.typeSymbol == ArrayClass) signature1(etp)
      // else flatNameString(etp.typeSymbol, '.')
      flatNameString(etp.typeSymbol, '.')
    }
  }
}
