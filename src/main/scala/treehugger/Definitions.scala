package treehugger

import Flags._

trait Definitions extends api.StandardDefinitions { self: Universe =>
  // the scala value classes
  trait ValueClassDefinitions { self: definitions.type =>
    import scala.collection.mutable
    
    lazy val symbolCache: mutable.Map[Name, Symbol] =
      mutable.Map(nme.scala_ -> ScalaPackageClass)    
    
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
    
    lazy val ScalaPackage         = getModule(nme.scala_)
    lazy val ScalaPackageClass    = ScalaPackage.moduleClass
    
    lazy val PredefModule: Symbol = getModule("scala.Predef")
    
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
    
    sealed abstract class BottomClassSymbol(name: TypeName, parent: Symbol) extends ClassSymbol(ScalaPackageClass, NoPosition, name) {
      this setFlag ABSTRACT | TRAIT | FINAL
      // final override def isBottomClass = true
    }
    final object NothingClass extends BottomClassSymbol(tpnme.Nothing, AnyClass) {
    }
    final object NullClass extends BottomClassSymbol(tpnme.Null, AnyRefClass) {
    }
    
    lazy val SymbolClass                = getClass("scala.Symbol")
    lazy val StringClass                = getClass(sn.String)
    lazy val ClassClass                 = getClass(sn.Class)
    
    
    // collections classes
    lazy val ConsClass          = getClass("scala.collection.immutable.$colon$colon")
    lazy val IterableClass      = getClass("scala.collection.Iterable")
    lazy val IteratorClass      = getClass("scala.collection.Iterator")
    lazy val ListClass          = getClass("scala.collection.immutable.List")
    lazy val SeqClass           = getClass("scala.collection.Seq")
    lazy val StringBuilderClass = getClass("scala.collection.mutable.StringBuilder")
    lazy val TraversableClass   = getClass("scala.collection.Traversable")
    
    lazy val ListModule       = getModule("scala.collection.immutable.List")
    //  lazy val List_apply = getMember(ListModule, nme.apply)
    lazy val NilModule        = getModule("scala.collection.immutable.Nil")
    lazy val SeqModule        = getModule("scala.collection.Seq")
    
    def getClass(fullname: Name): Symbol = getModuleOrClass(fullname.toTermName)
    
    def getModule(fullname: Name): Symbol = getModuleOrClass(fullname.toTermName)
      
    private def getModuleOrClass(path: Name, len: Int): Symbol =
      symbolCache.getOrElseUpdate(path, {
        val point = path lastPos('.', len - 1)
        val owner =
          if (point > 0) getModuleOrClass(path.toTermName, point)
          else RootClass
        val name = path subName (point + 1, len)
        if (name.isTypeName) owner.newClass(name.toTypeName)
        else owner.newModule(name.toTermName)
      })
      
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
    
    // def ClassType(arg: Type) = appliedType(ClassClass.typeConstructor, List(arg))
    
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
