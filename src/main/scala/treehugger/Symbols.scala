package treehugger

import Flags._
import api.Modifier

trait Symbols extends api.Symbols { self: Universe =>
  import definitions._
  
  private var ids = 0
  
  abstract class Symbol(initOwner: Symbol, initPos: Position, initName: Name)
      extends AbsSymbol with HasFlags {
    
    type FlagsType          = Long
    type AccessBoundaryType = Symbol
    type AnnotationType     = AnnotationInfo
    
    var rawowner: Symbol = initOwner
    var rawname: Name = initName
    var rawpos = initPos
    var rawflags = 0L
    val id = { ids += 1; ids } // identity displayed when -uniqid
    
    override def hasModifier(mod: Modifier.Value) =
      hasFlag(flagOfModifier(mod)) &&
      (!(mod == Modifier.bynameParameter) || isTerm) &&
      (!(mod == Modifier.covariant) || isType)

    override def allModifiers: Set[Modifier.Value] =
      Modifier.values filter hasModifier
    
// ------ creators -------------------------------------------------------------------

    final def newValue(pos: Position, name: TermName) =
      new TermSymbol(this, pos, name)
    final def newValue(name: TermName, pos: Position = NoPosition) =
      new TermSymbol(this, pos, name)
    final def newVariable(pos: Position, name: TermName) =
      newValue(pos, name).setFlag(MUTABLE)
    final def newValueParameter(pos: Position, name: TermName) =
      newValue(pos, name).setFlag(PARAM)
    /** Create local dummy for template (owner of local blocks) */
    final def newLocalDummy(pos: Position) =
      newValue(pos, nme.localDummyName(this))
    final def newMethod(pos: Position, name: TermName) =
      new MethodSymbol(this, pos, name).setFlag(METHOD)
    final def newMethod(name: TermName, pos: Position = NoPosition) =
      new MethodSymbol(this, pos, name).setFlag(METHOD)
    final def newLabel(pos: Position, name: TermName) =
      newMethod(pos, name).setFlag(LABEL)

    /** Propagates ConstrFlags (JAVA, specifically) from owner to constructor. */
    final def newConstructor(pos: Position) =
      newMethod(pos, nme.CONSTRUCTOR) setFlag getFlag(ConstrFlags)
    /** Static constructor with info set. */
    def newStaticConstructor(pos: Position) =
      newConstructor(pos) setFlag STATIC
    /** Instance constructor with info set. */
    def newClassConstructor(pos: Position) =
      newConstructor(pos)
    
    /** The owner of this symbol.
     */
    def owner: Symbol = rawowner

    /** The name of the symbol as a member of the `Name` type.
     */
    def name: Name = rawname
    
    /** The simple name of this Symbol */
    final def simpleName: Name = name
    
    /** The decoded name of the symbol, e.g. `==` instead of `\$eq\$eq`.
     */
    def decodedName: String = name.name
    
    /** String representation of symbol's simple name.
     *  If !settings.debug translates expansions of operators back to operator symbol.
     *  E.g. $eq => =.
     *  If settings.uniqid, adds id.
     */
    def nameString = decodedName
    
    /** The module class corresponding to this module.
     */
    def moduleClass: Symbol = NoSymbol
    
    final def newModule(pos: Position, name: TermName): ModuleSymbol =
      new ModuleSymbol(this, pos, name)
    final def newModule(name: TermName, pos: Position = NoPosition): ModuleSymbol =
      new ModuleSymbol(this, pos, name)      
      
    final def newPackage(pos: Position, name: TermName): ModuleSymbol = {
      val m = newModule(pos, name)
      m
    }
    final def newModuleClass(pos: Position, name: TypeName) =
      new ModuleClassSymbol(this, pos, name)
    final def newModuleClass(name: TypeName, pos: Position = NoPosition) =
      new ModuleClassSymbol(this, pos, name)
      
    final def newClass(pos: Position, name: TypeName) =
      new ClassSymbol(this, pos, name)
    final def newClass(name: TypeName, pos: Position = NoPosition) =
      new ClassSymbol(this, pos, name)
    
    /** Symbol of a type definition  type T = ...
     */
    final def newAliasType(pos: Position, name: TypeName) =
      new TypeSymbol(this, pos, name)
    final def newAliasType(name: TypeName, pos: Position = NoPosition) =
      new TypeSymbol(this, pos, name)
    
    /** Synthetic value parameters when parameter symbols are not available.
     *  Calling this method multiple times will re-use the same parameter names.
     */
    final def newSyntheticValueParams(argtypes: List[Type]): List[Symbol] =
      newSyntheticValueParamss(List(argtypes)).head

    /** Synthetic value parameter when parameter symbol is not available.
     *  Calling this method multiple times will re-use the same parameter name.
     */
    final def newSyntheticValueParam(argtype: Type): Symbol =
      newSyntheticValueParams(List(argtype)).head    
    
    /** Symbol of an abstract type  type T >: ... <: ...
     */
    final def newAbstractType(pos: Position, name: TypeName) =
      new TypeSymbol(this, pos, name).setFlag(DEFERRED)
    final def newAbstractType(name: TypeName, pos: Position = NoPosition) =
      new TypeSymbol(this, pos, name).setFlag(DEFERRED)

    /** Symbol of a type parameter
     */
    final def newTypeParameter(pos: Position, name: TypeName) =
      newAbstractType(pos, name).setFlag(PARAM)

    /** Synthetic value parameters when parameter symbols are not available
     */
    final def newSyntheticValueParamss(argtypess: List[List[Type]]): List[List[Symbol]] = {
      var cnt = 0
      def freshName() = { cnt += 1; newTermName("x$" + cnt) }
      def param(tp: Type) =
        newValueParameter(NoPosition, freshName()).setFlag(SYNTHETIC)
      argtypess map (_.map(param))
    }

    final def newExistential(pos: Position, name: TypeName): Symbol =
      newAbstractType(pos, name).setFlag(EXISTENTIAL)
    
    def tpe: Type = NoType
    
    /** The type constructor of a symbol is:
     *  For a type symbol, the type corresponding to the symbol itself,
     *  excluding parameters.
     *  Not applicable for term symbols.
     */
    def typeConstructor: Type =
      error("typeConstructor inapplicable for " + this)

    def isTerm         = false  // to be overridden
    def isType         = false  // to be overridden
    def isClass        = false  // to be overridden
    def isBottomClass  = false  // to be overridden
    def isAliasType    = false  // to be overridden
    def isAbstractType = false  // to be overridden
    
    /** Package tests */
    final def isEmptyPackage      = isPackage && name == nme.EMPTY_PACKAGE_NAME
    final def isEmptyPackageClass = isPackageClass && name == tpnme.EMPTY_PACKAGE_NAME
    final def isPackage           = isModule && hasFlag(PACKAGE)
    final def isPackageClass      = isClass && hasFlag(PACKAGE)
    final def isRoot              = isPackageClass && owner == NoSymbol
    final def isRootPackage       = isPackage && owner == NoSymbol    
    
    /** Is this symbol an effective root for fullname string?
     */
    def isEffectiveRoot = isRoot || isEmptyPackageClass
    
    /** Is this symbol a type but not a class? */
    def isNonClassType = false // to be overridden
    
    final def isMethod             = isTerm && hasFlag(METHOD)
    final def isModule             = isTerm && hasFlag(MODULE)
    final def isModuleClass        = isClass && hasFlag(MODULE)
    final def isRefinementClass    = isClass && name == tpnme.REFINE_CLASS_NAME
    
    
    final def isValueParameter = isTerm && hasFlag(PARAM)
    // final def isLocalDummy = isTerm && nme.isLocalDummyName(name)
    final def isInitializedToDefault = !isType && hasAllFlags(DEFAULTINIT | ACCESSOR)
    final def isClassConstructor = isTerm && (name == nme.CONSTRUCTOR)
    final def isMixinConstructor = isTerm && (name == nme.MIXIN_CONSTRUCTOR)
    final def isError = hasFlag(IS_ERROR)
    
    final def isAnonymousClass             = isClass && (name containsName tpnme.ANON_CLASS_NAME)
    
    // A package object or its module class
    final def isPackageObjectOrClass = name == nme.PACKAGE || name == tpnme.PACKAGE
    final def isPackageObject        = name == nme.PACKAGE && owner.isPackageClass
    final def isPackageObjectClass   = name == tpnme.PACKAGE && owner.isPackageClass
    
    /** The owner, skipping package objects.
     */
    def effectiveOwner = owner.skipPackageObject

    /** If this is a package object or its implementing class, its owner: otherwise this.
     */
    final def skipPackageObject: Symbol = if (isPackageObjectOrClass) owner else this
    
    /** Is this symbol locally defined? I.e. not accessed from outside `this` instance */
    final def isLocal: Boolean = owner.isTerm
    
// ------ flags attribute --------------------------------------------------------------

    final def flags: Long = {
      val fs = rawflags
      (fs | ((fs & LateFlags) >>> LateShift)) & ~(fs >>> AntiShift)
    }
    final def flags_=(fs: Long) = rawflags = fs
    final def setFlag(mask: Long): this.type = { rawflags = rawflags | mask; this }
    final def resetFlag(mask: Long): this.type = { rawflags = rawflags & ~mask; this }
    final def getFlag(mask: Long): Long = flags & mask
    final def resetFlags() { rawflags = rawflags & TopLevelCreationFlags }

    /** Does symbol have ANY flag in `mask` set? */
    final def hasFlag(mask: Long): Boolean = (flags & mask) != 0L

    /** Does symbol have ALL the flags in `mask` set? */
    final def hasAllFlags(mask: Long): Boolean = (flags & mask) == mask

    /** If the given flag is set on this symbol, also set the corresponding
     *  notFLAG.  For instance if flag is PRIVATE, the notPRIVATE flag will
     *  be set if PRIVATE is currently set.
     */
    final def setNotFlag(flag: Int) = if (hasFlag(flag)) setFlag((flag: @annotation.switch) match {
      case PRIVATE   => notPRIVATE
      case PROTECTED => notPROTECTED
      case OVERRIDE  => notOVERRIDE
      case _         => error("setNotFlag on invalid flag: " + flag)
    })

    /** The class or term up to which this symbol is accessible,
     *  or RootClass if it is public.  As java protected statics are
     *  otherwise completely inaccessible in scala, they are treated
     *  as public.
     */
    def accessBoundary(base: Symbol): Symbol = {
      if (hasFlag(PRIVATE) || isLocal) owner
      else if (hasAllFlags(PROTECTED | STATIC | JAVA)) RootClass
      else if (hasAccessBoundary) privateWithin
      else if (hasFlag(PROTECTED)) base
      else RootClass
    }

    // def isLessAccessibleThan(other: Symbol): Boolean = {
    //   val tb = this.accessBoundary(owner)
    //   val ob1 = other.accessBoundary(owner)
    //   val ob2 = ob1.linkedClassOfClass
    //   var o = tb
    //   while (o != NoSymbol && o != ob1 && o != ob2) {
    //     o = o.owner
    //   }
    //   o != NoSymbol && o != tb
    // }

    /** See comment in HasFlags for how privateWithin combines with flags.
     */
    private[this] var _privateWithin: Symbol = _
    def privateWithin = _privateWithin
    def privateWithin_=(sym: Symbol) { _privateWithin = sym }
    def setPrivateWithin(sym: Symbol): this.type = { privateWithin_=(sym) ; this }

    /** Does symbol have a private or protected qualifier set? */
    final def hasAccessBoundary = (privateWithin != null) && (privateWithin != NoSymbol)

// ----- annotations ------------------------------------------------------------

    // null is a marker that they still need to be obtained.
    private var _annotations: List[AnnotationInfo] = Nil

    def annotationsString = if (annotations.isEmpty) "" else annotations.mkString("(", ", ", ")")

    /** After the typer phase (before, look at the definition's Modifiers), contains
     *  the annotations attached to member a definition (class, method, type, field).
     */
    def annotations: List[AnnotationInfo] = _annotations
    def setAnnotations(annots: List[AnnotationInfo]): this.type = {
      _annotations = annots
      this
    }

    def withAnnotations(annots: List[AnnotationInfo]): this.type =
      setAnnotations(annots ::: annotations)

    def withoutAnnotations: this.type =
      setAnnotations(Nil)

    def filterAnnotations(p: AnnotationInfo => Boolean): this.type =
      setAnnotations(annotations filter p)

    def addAnnotation(annot: AnnotationInfo): this.type =
      setAnnotations(annot :: annotations)

    // Convenience for the overwhelmingly common case
    def addAnnotation(sym: Symbol, args: Tree*): this.type =
      addAnnotation(AnnotationInfo(sym.tpe, args.toList, Nil))
    
    def hasFlagsToString(mask: Long): String = flagsToString(
      flags & mask,
      if (hasAccessBoundary) privateWithin.toString else ""
    )
    
// ------ access to related symbols --------------------------------------------------    
    
    /** The module corresponding to this module class (note that this
     *  is not updated when a module is cloned), or NoSymbol if this is not a ModuleClass.
     */
    def sourceModule: Symbol = NoSymbol
  }
  
  /** A class for term symbols */
  class TermSymbol(initOwner: Symbol, initPos: Position, initName: TermName)
  extends Symbol(initOwner, initPos, initName) {
    final override def isTerm = true
  } 
  
  /** A class for module symbols */
  class ModuleSymbol(initOwner: Symbol, initPos: Position, initName: TermName)
  extends TermSymbol(initOwner, initPos, initName) {
  }
  
  /** A class for method symbols */
  class MethodSymbol(initOwner: Symbol, initPos: Position, initName: TermName)
  extends TermSymbol(initOwner, initPos, initName) {
  }
  
  class TypeSymbol(initOwner: Symbol, initPos: Position, initName: TypeName)
  extends Symbol(initOwner, initPos, initName) {
    override def name: TypeName = super.name.asInstanceOf[TypeName]
    
    override def isNonClassType = true
    
    // private def newTypeRef(targs: List[Type]) = {
    //   // val pre = if (hasFlag(PARAM | EXISTENTIAL)) NoPrefix else owner.thisType
    //   val pre = owner.thisType
    //   typeRef(pre, this, targs)
    // }
    
    // override def typeConstructor: Type = newTypeRef(Nil)
  }
  
  /** A class for class symbols */
  class ClassSymbol(initOwner: Symbol, initPos: Position, initName: TypeName)
  extends TypeSymbol(initOwner, initPos, initName) {
    final override def isClass = true
    final override def isNonClassType = false
    final override def isAbstractType = false
    final override def isAliasType = false
  }
  
  /** A class for module class symbols
   *  Note: Not all module classes are of this type; when unpickled, we get
   *  plain class symbols!
   */
  class ModuleClassSymbol(owner: Symbol, pos: Position, name: TypeName)
  extends ClassSymbol(owner, pos, name) {
  }
  
  object NoSymbol extends Symbol(null, NoPosition, nme.NO_NAME) {}
}
