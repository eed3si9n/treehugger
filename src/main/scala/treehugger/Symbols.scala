package treehugger

import Flags._
import api.Modifier

trait Symbols extends api.Symbols { self: Forest =>
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
    
    def pos = rawpos
    
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
    
    private def finishModule(m: ModuleSymbol, clazz: ClassSymbol): ModuleSymbol = {
      // Top-level objects can be automatically marked final, but others
      // must be explicitly marked final if overridable objects are enabled.
      val flags = if (isPackage) MODULE | FINAL else MODULE
      m setFlag flags
      m setModuleClass clazz
      m
    }
    private def finishModule(m: ModuleSymbol): ModuleSymbol =
      finishModule(m, new ModuleClassSymbol(m))
    
    final def newModule(pos: Position, name: TermName, clazz: ClassSymbol): ModuleSymbol =
      finishModule(new ModuleSymbol(this, pos, name), clazz)
    
    final def newModule(name: TermName, clazz: Symbol, pos: Position = NoPosition): ModuleSymbol =
      newModule(pos, name, clazz.asInstanceOf[ClassSymbol])
    
    final def newModule(pos: Position, name: TermName): ModuleSymbol =
      finishModule(new ModuleSymbol(this, pos, name))   
    
    final def newModule(name: TermName): ModuleSymbol =
      finishModule(new ModuleSymbol(this, NoPosition, name))
    
    final def newPackage(pos: Position, name: TermName): ModuleSymbol = {
      val m = newModule(pos, name).setFlag(JAVA | PACKAGE)
      m.moduleClass setFlag (JAVA | PACKAGE)
      m
    }
    final def newPackage(name: TermName, pos: Position = NoPosition): ModuleSymbol =
      newPackage(pos, name)
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
    final def newTypeParameter(name: TypeName, pos: Position = NoPosition) =
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
    
    /** Get type. The type of a symbol is:
     *  for a type symbol, the type corresponding to the symbol itself,
     *    @M you should use tpeHK for a type symbol with type parameters if
     *       the kind of the type need not be *, as tpe introduces dummy arguments
     *       to generate a type of kind *
     *  for a term symbol, its usual type.
     *  See the tpe/tpeHK overrides in TypeSymbol for more.
     */
    def tpe: Type = NoType
    def tpeHK: Type = tpe
    
    /** The type constructor of a symbol is:
     *  For a type symbol, the type corresponding to the symbol itself,
     *  excluding parameters.
     *  Not applicable for term symbols.
     */
    def typeConstructor: Type =
      error("typeConstructor inapplicable for " + this)
    
    final def toType: Type = typeConstructor
    
    def typeParams: List[Symbol] = Nil
    def paramss: List[List[Symbol]] = Nil

    def isTerm         = false  // to be overridden
    def isType         = false  // to be overridden
    def isClass        = false  // to be overridden
    def isBottomClass  = false  // to be overridden
    def isAliasType    = false  // to be overridden
    def isAbstractType = false  // to be overridden
    private[treehugger] def isSkolem = false // to be overridden
    
    /** Is this symbol a type but not a class? */
    def isNonClassType = false // to be overridden
    
    override final def isTrait     = isClass && hasFlag(TRAIT)
    final def isAbstractClass      = isClass && hasFlag(ABSTRACT)
    final def isBridge             = hasFlag(BRIDGE)
    final def isContravariant      = isType && hasFlag(CONTRAVARIANT)
    final def isConcreteClass      = isClass && !hasFlag(ABSTRACT | TRAIT)
    final def isCovariant          = isType && hasFlag(COVARIANT)
    final def isEarlyInitialized   = isTerm && hasFlag(PRESUPER)
    final def isExistentiallyBound = isType && hasFlag(EXISTENTIAL)
    final def isImplClass          = isClass && hasFlag(IMPLCLASS)
    final def isLazyAccessor       = isLazy && lazyAccessor != NoSymbol
    final def isMethod             = isTerm && hasFlag(METHOD)
    final def isModule             = isTerm && hasFlag(MODULE)
    final def isModuleClass        = isClass && hasFlag(MODULE)
    final def isNumericValueClass  = definitions.isNumericValueClass(this)
    final def isOverloaded         = hasFlag(OVERLOADED)
    final def isOverridableMember  = !(isClass || isEffectivelyFinal) && owner.isClass
    final def isRefinementClass    = isClass && name == tpnme.REFINE_CLASS_NAME
    final def isSourceMethod       = isMethod && !hasFlag(STABLE) // exclude all accessors!!!
    final def isTypeParameter      = isType && isParameter && !isSkolem
    final def isValueClass         = definitions.isValueClass(this)
    final def isVarargsMethod      = isMethod && hasFlag(VARARGS)
    
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
    
    /** Term symbols with the exception of static parts of Java classes and packages.
     */
    final def isValue = isTerm && !(isModule && hasFlag(PACKAGE | JAVA))
    
    final def isVariable  = isTerm && isMutable && !isMethod
    
    final def isValueParameter = isTerm && hasFlag(PARAM)
    // final def isLocalDummy = isTerm && nme.isLocalDummyName(name)
    final def isInitializedToDefault = !isType && hasAllFlags(DEFAULTINIT | ACCESSOR)
    final def isClassConstructor = isTerm && (name == nme.CONSTRUCTOR)
    final def isMixinConstructor = isTerm && (name == nme.MIXIN_CONSTRUCTOR)
    final def isError = hasFlag(IS_ERROR)
    
    final def isAnonymousClass             = isClass && (name containsName tpnme.ANON_CLASS_NAME)
    final def isAnonymousFunction          = isSynthetic && (name containsName tpnme.ANON_FUN_NAME)
    final def isAnonOrRefinementClass      = isAnonymousClass || isRefinementClass
        
    // A package object or its module class
    final def isPackageObjectOrClass = name == nme.PACKAGE || name == tpnme.PACKAGE
    final def isPackageObject        = name == nme.PACKAGE && owner.isPackageClass
    final def isPackageObjectClass   = name == tpnme.PACKAGE && owner.isPackageClass
    
    final def isJavaInterface = isJavaDefined && isTrait
    
    /** The owner, skipping package objects.
     */
    def effectiveOwner = owner.skipPackageObject

    /** If this is a package object or its implementing class, its owner: otherwise this.
     */
    final def skipPackageObject: Symbol = if (isPackageObjectOrClass) owner else this
    
    /** Conditions where we omit the prefix when printing a symbol, to avoid
     *  unpleasantries like Predef.String, $iw.$iw.Foo and <empty>.Bippy.
     */
    final def isOmittablePrefix = (
         UnqualifiedOwners(skipPackageObject)
      || isEmptyPrefix
    )
    
    def isEmptyPrefix = (
         isEffectiveRoot                      // has no prefix for real, <empty> or <root>
      || isAnonOrRefinementClass              // has uninteresting <anon> or <refinement> prefix
      // || nme.isReplWrapperName(name)          // has ugly $iw. prefix (doesn't call isInterpreterWrapper due to nesting)
    )
    
    /** The encoded full path name of this symbol, where outer names and inner names
     *  are separated by `separator` characters.
     *  Never translates expansions of operators back to operator symbol.
     *  Never adds id.
     *  Drops package objects.
     */
    final def fullName(separator: Char): String = stripNameString(fullNameInternal(separator))

    /** Doesn't drop package objects, for those situations (e.g. classloading)
     *  where the true path is needed.
     */
    private def fullNameInternal(separator: Char): String = (
      if (isRoot || isRootPackage || this == NoSymbol) this.toString
      else if (owner.isEffectiveRoot) decodedName
      else effectiveOwner.enclClass.fullName(separator) + separator + decodedName
    )
    
    /** Is this symbol effectively final? I.e, it cannot be overridden */
    final def isEffectivelyFinal: Boolean = (
         isFinal
      || hasModuleFlag
      || isTerm && (
             isPrivate
          || isLocal
          || owner.isClass && owner.isEffectivelyFinal
      )
    )
    
    /** Is this symbol locally defined? I.e. not accessed from outside `this` instance */
    final def isLocal: Boolean = owner.isTerm
    
    /** Strip package objects and any local suffix.
     */
    private def stripNameString(s: String) = s stripSuffix nme.LOCAL_SUFFIX_STRING
      
    /** The encoded full path name of this symbol, where outer names and inner names
     *  are separated by periods.
     */
    final def fullName: String = fullName('.')
    
    /** The variance of this symbol as an integer */
    final def variance: Int =
      if (isCovariant) 1
      else if (isContravariant) -1
      else 0    

// ------ owner attribute --------------------------------------------------------------

    /** The owner of this symbol.
     */
    def owner: Symbol = rawowner
    
    def ownerChain: List[Symbol] =
      if (owner eq null) this :: Nil
      else this :: owner.ownerChain
    
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
    
// ------ comparisons ----------------------------------------------------------------

    /** Is this class symbol a subclass of that symbol? */
    final def isNonBottomSubClass(that: Symbol): Boolean = (
      (this eq that) || this.isError || that.isError // || info.baseTypeIndex(that) >= 0
    )

    /** Overridden in NullClass and NothingClass for custom behavior.
     */
    def isSubClass(that: Symbol) = isNonBottomSubClass(that)
    
// ------ access to related symbols --------------------------------------------------    
    
    /** The next enclosing class. */
    def enclClass: Symbol = if (isClass) this else owner.enclClass
    
    /** If symbol is a class, the type <code>this.type</code> in this class,
     * otherwise <code>NoPrefix</code>.
     * We always have: thisType <:< typeOfThis
     */
    def thisType: Type = NoPrefix

    /** The module corresponding to this module class (note that this
     *  is not updated when a module is cloned), or NoSymbol if this is not a ModuleClass.
     */
    def sourceModule: Symbol = NoSymbol
    
    /** For a lazy value, its lazy accessor. NoSymbol for all others. */
    def lazyAccessor: Symbol = NoSymbol
    
// ------ toString -------------------------------------------------------------------
    
    /** String representation of symbol's definition key word */
    final def keyString: String =
      if (isJavaInterface) "interface"
      else if (isTrait) "trait"
      else if (isClass) "class"
      else if (isType && !isParameter) "type"
      else if (isVariable) "var"
      else if (isPackage) "package"
      else if (isModule) "object"
      else if (isSourceMethod) "def"
      else if (isTerm && (!isParameter || isParamAccessor)) "val"
      else ""
      
    override def toString = nameString
    
    def signatureString = "<_>" // if (hasRawInfo) infoString(rawInfo) else "<_>"
        
    def hasFlagsToString(mask: Long): String = flagsToString(
      flags & mask,
      if (hasAccessBoundary) privateWithin.toString else ""
    )
    
    /** String representation of symbol's variance */
    def varianceString: String =
      if (variance == 1) "+"
      else if (variance == -1) "-"
      else ""
    
    def defaultFlagMask =
      if (owner.isRefinementClass) ExplicitFlags & ~OVERRIDE
      else ExplicitFlags

    def accessString = hasFlagsToString(PRIVATE | PROTECTED | LOCAL)
    def defaultFlagString = hasFlagsToString(defaultFlagMask)
    

    
    private def defStringCompose(infoString: String) = compose(
      defaultFlagString,
      keyString,
      varianceString + nameString + infoString
    )
    /** String representation of symbol's definition.  It uses the
     *  symbol's raw info to avoid forcing types.
     */
    def defString = defStringCompose(signatureString)
    
    /** Concatenate strings separated by spaces */
    private def compose(ss: String*) = ss filter (_ != "") mkString " "
    
    def isSingletonExistential = false
      // nme.isSingletonName(name) && (info.bounds.hi.typeSymbol isSubClass SingletonClass)

    /** String representation of existentially bound variable */
    def existentialToString = defString
  }
  
  /** A class for term symbols */
  class TermSymbol(initOwner: Symbol, initPos: Position, initName: TermName)
  extends Symbol(initOwner, initPos, initName) {
    final override def isTerm = true
    
    override def name: TermName = super.name
    
    private var referenced: Symbol = NoSymbol
    override def moduleClass: Symbol =
      if (hasFlag(MODULE)) referenced
      else NoSymbol
    def setModuleClass(clazz: Symbol): TermSymbol = {
      assert(hasFlag(MODULE))
      referenced = clazz
      this
    }
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
    private var tyconCache: Type = null
    
    override def name: TypeName = super.name.asInstanceOf[TypeName]
    final override def isType = true
    override def isNonClassType = true
    
    private def newTypeRef(targs: List[Type]) = {
      val pre = if (hasFlag(PARAM | EXISTENTIAL)) NoPrefix else owner.thisType
      typeRef(pre, this, targs)
    }
    
    override def typeConstructor: Type = {
      if (tyconCache eq null) {
        tyconCache = newTypeRef(Nil)
      }
      tyconCache
    }
        
    override def tpeHK = typeConstructor // @M! used in memberType
  }
  
  /** A class for class symbols */
  class ClassSymbol(initOwner: Symbol, initPos: Position, initName: TypeName)
  extends TypeSymbol(initOwner, initPos, initName) {
    final override def isClass = true
    final override def isNonClassType = false
    final override def isAbstractType = false
    final override def isAliasType = false
    
    private var thisTypeCache: Type = null
    /** the type this.type in this class */
    override def thisType: Type = {
      if (thisTypeCache eq null) {
        thisTypeCache = ThisType(this)
      }
      
      thisTypeCache
    }
  }
  
  /** A class for module class symbols
   *  Note: Not all module classes are of this type; when unpickled, we get
   *  plain class symbols!
   */
  class ModuleClassSymbol(owner: Symbol, pos: Position, name: TypeName)
  extends ClassSymbol(owner, pos, name) {
    def this(module: TermSymbol) = {
      this(module.owner, module.pos, module.name.toTypeName)
      setFlag(module.getFlag(ModuleToClassFlags) | MODULE)
      // sourceModule = module
    }
  }
  
  object NoSymbol extends Symbol(null, NoPosition, nme.NO_NAME) {}
}
