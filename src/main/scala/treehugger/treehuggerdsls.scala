/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 *
 * @author  Paul Phillips
 */

package treehugger

import PartialFunction._
import Flags._

/** A DSL for generating scala code.  The goal is that the
 *  code generating code should look a lot like the code it
 *  generates.
 */

trait TreehuggerDSLs { self: Forest =>
  
  import definitions._
  import self.{ scalaDot }
  
  object treehuggerDSL {
    // Add a null check to a Tree => Tree function
    def nullSafe[T](f: Tree => Tree, ifNull: Tree): Tree => Tree =
      tree => IF (tree MEMBER_== NULL) THEN ifNull ELSE f(tree)

    // def returning[T](x: T)(f: T => Unit): T = util.returning(x)(f)

    object LIT extends (Any => Literal) {
      def apply(x: Any)   = Literal(Constant(x))
      def unapply(x: Any) = condOpt(x) { case Literal(Constant(value)) => value }
    }

    // You might think these could all be vals, but empirically I have found that
    // at least in the case of UNIT the compiler breaks if you re-use trees.
    // However we need stable identifiers to have attractive pattern matching.
    // So it's inconsistent until I devise a better way.
    val TRUE          = LIT(true)
    val FALSE         = LIT(false)
    val ZERO          = LIT(0)
    def NULL          = LIT(null)
    def UNIT          = LIT(())
    val SUPER         = Super(EmptyTree)

    object WILD {
      def empty               = Ident(nme.WILDCARD)
      def apply(tpe: Type)    = Ident(nme.WILDCARD) setType tpe
      def unapply(other: Any) = cond(other) { case Ident(nme.WILDCARD) => true }
    }

    def fn(lhs: Tree, op:   Name, args: Tree*)  = Apply(Select(lhs, op), args.toList)
    def fn(lhs: Tree, op: Symbol, args: Tree*)  = Apply(Select(lhs, op), args.toList)

    class TreeMethods(target: Tree) {
      /** logical/comparison ops **/
      def OR(other: Tree) =
        if (target == EmptyTree) other
        else if (other == EmptyTree) target
        else mkOr(target, other)

      def AND(other: Tree) =
        if (target == EmptyTree) other
        else if (other == EmptyTree) target
        else mkAnd(target, other)

      /** Note - calling ANY_== in the matcher caused primitives to get boxed
       *  for the comparison, whereas looking up nme.EQ does not.  See #3570 for
       *  an example of how target.tpe can be non-null, yet it claims not to have
       *  a member called nme.EQ.  Not sure if that should happen, but we can be
       *  robust by dragging in Any regardless.
       */
      def MEMBER_== (other: Tree)   = {
        val opSym = NoSymbol // if (target.tpe == null) NoSymbol else target.tpe member nme.EQ
        if (opSym == NoSymbol) ANY_==(other)
        else fn(target, opSym, other)
      }
      def ANY_EQ  (other: Tree)     = OBJ_EQ(other AS ObjectClass.tpe)
      def ANY_==  (other: Tree)     = fn(target, Any_==, other)
      def ANY_!=  (other: Tree)     = fn(target, Any_!=, other)
      def OBJ_==  (other: Tree)     = fn(target, Object_==, other)
      def OBJ_!=  (other: Tree)     = fn(target, Object_!=, other)
      def OBJ_EQ  (other: Tree)     = fn(target, Object_eq, other)
      def OBJ_NE  (other: Tree)     = fn(target, Object_ne, other)

      def INT_|   (other: Tree)     = fn(target, getMember(IntClass, nme.OR), other)
      def INT_&   (other: Tree)     = fn(target, getMember(IntClass, nme.AND), other)
      def INT_>=  (other: Tree)     = fn(target, getMember(IntClass, nme.GE), other)
      def INT_==  (other: Tree)     = fn(target, getMember(IntClass, nme.EQ), other)
      def INT_!=  (other: Tree)     = fn(target, getMember(IntClass, nme.NE), other)

      def BOOL_&& (other: Tree)     = fn(target, Boolean_and, other)
      def BOOL_|| (other: Tree)     = fn(target, Boolean_or, other)

      /** Apply, Select, Match **/
      def APPLY(params: Tree*)      = Apply(target, params.toList)
      def APPLY(params: List[Tree]) = Apply(target, params)
      def MATCH(cases: CaseDef*)    = Match(target, cases.toList)
      def UNAPPLY(params: Tree*)    = UnApply(target, params.toList)

      def DOT(member: Name)         = SelectStart(Select(target, member))
      def DOT(sym: Symbol)          = SelectStart(Select(target, sym))

      def INFIX(name: Name, params: Tree*)  = Infix(target, name, params.toList)
      def INFIX(sym: Symbol, params: Tree*) = Infix(target, sym, params.toList)
      
      def INFIXUNAPPLY(name: Name, params: Tree*)  = InfixUnApply(target, name, params.toList)
      def INFIXUNAPPLY(sym: Symbol, params: Tree*) = InfixUnApply(target, sym, params.toList)
      
      /** Assignment */
      def :=(rhs: Tree)            = Assign(target, rhs)

      /** Methods for sequences **/
      def DROP(count: Int): Tree =
        if (count == 0) target
        else (target DOT nme.drop)(LIT(count))

      /** Casting & type tests -- working our way toward understanding exactly
       *  what differs between the different forms of IS and AS.
       *
       *  See ticket #2168 for one illustration of AS vs. AS_ANY.
       */
      def AS(tpe: Type)       = mkAsInstanceOf(target, tpe, any = true, wrapInApply = false)
      def IS(tpe: Type)       = mkIsInstanceOf(target, tpe, true)
      def IS_OBJ(tpe: Type)   = mkIsInstanceOf(target, tpe, false)

      // XXX having some difficulty expressing nullSafe in a way that doesn't freak out value types
      // def TOSTRING()          = nullSafe(fn(_: Tree, nme.toString_), LIT("null"))(target)
      def TOSTRING()          = fn(target, nme.toString_)
      def GETCLASS()          = fn(target, Object_getClass)
      
      def inPackage(name: Name): PackageDef = PACKAGEHEADER(name) := target
      def inPackage(sym: Symbol): PackageDef = PACKAGEHEADER(sym) := target
      def withComment(comment: String): Commented = Commented(comment, target)
    }

    case class SelectStart(tree: Select) {
      def apply(args: Tree*) = Apply(tree, args.toList)
    }

    class CaseStart(pat: Tree, guard: Tree) {
      def IF(g: Tree): CaseStart    = new CaseStart(pat, g)
      def ==>(body: Tree): CaseDef   = CaseDef(pat, guard, body)
    }
    
    trait DefStart[ResultTreeType <: Tree] {
      def name: Name
      def defaultMods: Modifiers
      def defaultPos: Position

      // type ResultTreeType <: Tree // >
      def mkTree(rhs: Tree): ResultTreeType
      def :=(rhs: Tree): ResultTreeType
      final def empty: ResultTreeType = mkTree(EmptyTree)

      private var _mods: Modifiers = null
      private var _pos: Position = null

      def withFlags(flags: Long*): this.type = {
        if (_mods == null)
          _mods = defaultMods

        _mods = flags.foldLeft(_mods)(_ | _)
        this
      }
      def withPos(pos: Position): this.type = {
        _pos = pos
        this
      }
      
      final def mods = if (_mods == null) defaultMods else _mods
      final def pos  = if (_pos == null) defaultPos else _pos
    }

    /** VODD, if it is not obvious, means ValOrDefDef.  This is the
     *  common code between a tree based on a pre-existing symbol and
     *  one being built from scratch.
     */
    trait VODDStart[ResultTreeType <: Tree] extends DefStart[ResultTreeType] {
      def defaultTpt: Tree

      private var _tpt: Tree = null

      def withType(tp: Type): this.type = {
        _tpt = TypeTree(tp)
        this
      }
      
      final def tpt  = if (_tpt == null) defaultTpt else _tpt
    }
    trait SymVODDStart[ResultTreeType <: Tree] extends VODDStart[ResultTreeType] {
      def sym: Symbol
      def symType: Type

      def name        = sym.name
      def defaultMods = Modifiers(sym.flags)
      def defaultTpt  = TypeTree(symType) // setPos sym.pos.focus
      def defaultPos  = sym.pos

      final def :=(rhs: Tree): ResultTreeType =
        mkTree(rhs) // setSymbol (sym resetFlag mods.flags)
    }
    trait ValCreator { self: VODDStart[ValDef] =>
      
      def mkTree(rhs: Tree): ValDef = ValDef(mods, name, tpt, rhs)
    }
    trait DefCreator { self: VODDStart[DefDef] =>
      def tparams: List[TypeDef]
      def vparamss: List[List[ValDef]]
      
      def mkTree(rhs: Tree): DefDef = DefDef(mods, name, tparams, vparamss, tpt, rhs)
    }

    class DefSymStart(val sym: Symbol) extends SymVODDStart[DefDef] with DefCreator {
      def symType  = sym.tpe.finalResultType
      def tparams  = Nil // sym.typeParams map TypeDef
      def vparamss = sym.paramss map (xs => xs map ValDef)
    }
    class ValSymStart(val sym: Symbol) extends SymVODDStart[ValDef] with ValCreator {
      def symType = sym.tpe
    }
    
    trait TreeDefStart[ResultTreeType <: Tree] extends DefStart[ResultTreeType] {
      def defaultMods = NoMods
      def defaultPos  = NoPosition
      
      final def :=(rhs: Tree): ResultTreeType = mkTree(rhs)
        // if (pos == NoPosition) 
        // else atPos(pos)(mkTree(rhs))
    }

    trait TreeVODDStart[ResultTreeType <: Tree] extends VODDStart[ResultTreeType] with TreeDefStart[ResultTreeType] {
      def defaultTpt  = TypeTree()
    }

    class ValTreeStart(val name: Name) extends TreeVODDStart[ValDef] with ValCreator {
    }
    class DefTreeStart(val name: Name) extends TreeVODDStart[DefDef] with DefCreator {
      private var _vparamss: List[List[ValDef]] = List(Nil)
      private var _tparams: List[TypeDef] = Nil
      
      def withParams(param: ValDef*): this.type = {
        if (_vparamss == List(Nil))
          _vparamss = List(param.toList)
        else 
          _vparamss = _vparamss ::: List(param.toList)
        this
      }
      
      def withTypeParams(tparam: TypeDef*): this.type = {
        _tparams = tparam.toList
        this
      }
      
      def tparams: List[TypeDef] = _tparams
      def vparamss: List[List[ValDef]] = _vparamss
    }

    class IfStart(cond: Tree, thenp: Tree) {
      def THEN(x: Tree)     = new IfStart(cond, x)
      def ELSE(elsep: Tree) = If(cond, thenp, elsep)
      def ENDIF             = If(cond, thenp, EmptyTree)
    }
    class TryStart(body: Tree, catches: List[CaseDef], fin: Tree) {
      def CATCH(xs: CaseDef*) = new TryStart(body, xs.toList, fin)
      def FINALLY(x: Tree)    = Try(body, catches, x)
      def ENDTRY              = Try(body, catches, fin)
    }
    class ForStart(enums: List[Enumerator]) {
      def DO(body: Tree)    = ForTree(enums, body)
      def YEILD(body: Tree) = ForYieldTree(enums, body) 
    }

    def CASE(pat: Tree): CaseStart  = new CaseStart(pat, EmptyTree)
    def DEFAULT: CaseStart          = new CaseStart(WILD.empty, EmptyTree)

    class SymbolMethods(target: Symbol) {
      def BIND(body: Tree) = Bind(target, body)
      def IS_NULL()  = REF(target) OBJ_EQ NULL
      def NOT_NULL() = REF(target) OBJ_NE NULL

      def GET() = fn(REF(target), nme.get)

      // name of nth indexed argument to a method (first parameter list), defaults to 1st
      // def ARG(idx: Int = 0) = Ident(target.paramss.head(idx))
      // def ARGS = target.paramss.head
      // def ARGNAMES = ARGS map Ident
    }
    
    class ValFromStart(val name: Name) extends TreeVODDStart[ValFrom] {
      def mkTree(rhs: Tree): ValFrom = ValFrom(name, tpt, rhs)
    }
    
    class ClassDefStart(val name: TypeName) extends TreeDefStart[ClassDef] {
      private var _parents: List[Tree] = Nil
      private var _tparams: List[TypeDef] = Nil
      private var _vparams: List[ValDef] = Nil
      
      def withParents(parent: Tree*): this.type = {
        _parents = parent.toList
        this
      }
      
      def withTypeParams(tparam: TypeDef*): this.type = {
        _tparams = tparam.toList
        this
      }
      
      def withParams(param: ValDef*): this.type = {
        _vparams = param.toList
        this
      }
      
      def tparams: List[TypeDef] = _tparams
      def vparams: List[ValDef] = _vparams
      def parents: List[Tree] = _parents
      val selfDef: ValDef = emptyValDef
      
      def mkTree(rhs: Tree): ClassDef = rhs match {
        case Block(xs, x) => mkTree(xs ::: List(x))
        case EmptyTree => mkTree(Nil)
        case _ => mkTree(rhs :: Nil)
      }
      
      def mkTree(body: List[Tree]): ClassDef = ClassDef(mods, name, tparams, vparams, Template(parents, selfDef, body))
    }
    
    class TraitDefStart(name: TypeName) extends ClassDefStart(name) {
      override def mkTree(body: List[Tree]): ClassDef =
        ClassDef(mods | Flags.TRAIT, name, tparams, vparams, Template(parents, selfDef, body))
    }
    
    class ModuleDefStart(val name: TermName) extends TreeDefStart[ModuleDef] {
      val parents: List[Tree] = Nil
      val selfDef: ValDef = emptyValDef
      
      def mkTree(rhs: Tree): ModuleDef = rhs match {
        case Block(xs, x) => mkTree(xs ::: List(x))
        case EmptyTree => mkTree(Nil)
        case _ => mkTree(rhs :: Nil)
      }
      
      def mkTree(body: List[Tree]): ModuleDef = ModuleDef(mods, name, Template(parents, selfDef, body))
    }
    
    class PackageDefStart(val name: TermName, val header: Boolean) extends TreeDefStart[PackageDef] {
      override def defaultMods =
        if (header) NoMods | HEADER
        else NoMods
      
      def mkTree(rhs: Tree): PackageDef = rhs match {
        case Block(xs, x) => mkTree(xs ::: List(x))
        case EmptyTree => mkTree(Nil)
        case _ => mkTree(rhs :: Nil)
      }
      
      def mkTree(body: List[Tree]): PackageDef = PackageDef(mods, Ident(name), body)
    }
    
    class TypeDefTreeStart(val name: Name) extends TreeDefStart[TypeDef] {
      def mkTree(rhs: Tree): TypeDef = TypeDef(mods, name.toTypeName, Nil, rhs)
    }
    
    class TypeDefSymStart(val sym: Symbol) extends TreeDefStart[TypeDef] {
      def name        = sym.name.toTypeName
      
      def mkTree(rhs: Tree): TypeDef = TypeDef(mods, name, Nil, rhs) setSymbol sym
    }
    
    /** Top level accessible. */
    def MATCHERROR(arg: Tree) = Throw(New(TypeTree(MatchErrorClass.tpe), List(List(arg))))
    /** !!! should generalize null guard from match error here. */
    def THROW(sym: Symbol): Throw = Throw(New(TypeTree(sym.toType), List(Nil)))
    def THROW(sym: Symbol, msg: String): Throw = Throw(New(TypeTree(sym.toType), List(List(LIT(msg)))))
    def THROW(sym: Symbol, msg: Tree): Throw = Throw(New(TypeTree(sym.toType), List(List(msg.TOSTRING()))))

    def NEW(tp: Type, args: Tree*): Tree = NEW(TypeTree(tp), args: _*)
    def NEW(tpt: Tree, args: Tree*): Tree   = New(tpt, List(args.toList))
    def NEW(sym: Symbol, args: Tree*): Tree = New(sym, args: _*)

    def DEF(name: Name, tp: Type): DefTreeStart     = DEF(name) withType tp
    def DEF(name: Name, classSym: Symbol): DefTreeStart = DEF(name, classSym.toType)
    def DEF(name: Name): DefTreeStart               = new DefTreeStart(name)
    def DEF(sym: Symbol): DefSymStart               = new DefSymStart(sym)

    def VAL(name: Name, tp: Type): ValTreeStart     = VAL(name) withType tp
    def VAL(name: Name, classSym: Symbol): ValTreeStart = VAL(name, classSym.toType)
    def VAL(name: Name): ValTreeStart               = new ValTreeStart(name)
    def VAL(sym: Symbol, tp: Type): ValTreeStart    = VAL(sym.name) withType tp
    def VAL(sym: Symbol, classSym: Symbol): ValTreeStart = VAL(sym, classSym.toType)
    def VAL(sym: Symbol): ValSymStart               = new ValSymStart(sym)

    def VAR(name: Name, tp: Type): ValTreeStart     = VAL(name, tp) withFlags Flags.MUTABLE
    def VAR(name: Name, classSym: Symbol): ValTreeStart = VAR(name, classSym.toType)
    def VAR(name: Name): ValTreeStart               = VAL(name) withFlags Flags.MUTABLE
    def VAR(sym: Symbol): ValSymStart               = VAL(sym) withFlags Flags.MUTABLE

    def LAZYVAL(name: Name, tp: Type): ValTreeStart = VAL(name, tp) withFlags Flags.LAZY
    def LAZYVAL(name: Name, classSym: Symbol): ValTreeStart = LAZYVAL(name, classSym.toType)
    def LAZYVAL(name: Name): ValTreeStart           = VAL(name) withFlags Flags.LAZY
    def LAZYVAL(sym: Symbol): ValSymStart           = VAL(sym) withFlags Flags.LAZY

    def VALFROM(name: Name, tp: Type): ValFromStart = VALFROM(name) withType tp
    def VALFROM(name: Name, classSym: Symbol): ValFromStart = VALFROM(name, classSym.toType)
    def VALFROM(name: Name): ValFromStart           = new ValFromStart(name)

    def CLASSDEF(name: Name): ClassDefStart         = new ClassDefStart(name.toTypeName)
    def CLASSDEF(sym: Symbol): ClassDefStart        = new ClassDefStart(sym.name.toTypeName)
    
    def CASECLASSDEF(name: Name): ClassDefStart     = CLASSDEF(name) withFlags Flags.CASE
    def CASECLASSDEF(sym: Symbol): ClassDefStart    = CLASSDEF(sym) withFlags Flags.CASE

    def TRAITDEF(name: Name): ClassDefStart         = new TraitDefStart(name.toTypeName)
    def TRAITDEF(sym: Symbol): ClassDefStart        = new TraitDefStart(sym.name.toTypeName)

    def MODULEDEF(name: Name): ModuleDefStart       = new ModuleDefStart(name)
    def MODULEDEF(sym: Symbol): ModuleDefStart      = new ModuleDefStart(sym.name)
    
    def CASEMODULEDEF(name: Name): ModuleDefStart   = MODULEDEF(name) withFlags Flags.CASE
    def CASEMODULEDEF(sym: Symbol): ModuleDefStart  = MODULEDEF(sym) withFlags Flags.CASE
    
    def PACKAGEDEF(name: Name): PackageDefStart     = new PackageDefStart(name, false)
    def PACKAGEDEF(sym: Symbol): PackageDefStart    = new PackageDefStart(sym.name, false)
    def PACKAGEHEADER(name: Name): PackageDefStart  = new PackageDefStart(name, true)
    def PACKAGEHEADER(sym: Symbol): PackageDefStart = new PackageDefStart(sym.name, true)
    
    def TYPE(name: Name): TypeDefTreeStart          = new TypeDefTreeStart(name)
    def TYPE(sym: Symbol): TypeDefSymStart          = new TypeDefSymStart(sym)

    def AND(guards: Tree*) =
      if (guards.isEmpty) EmptyTree
      else guards reduceLeft mkAnd

    def OR(guards: Tree*) =
      if (guards.isEmpty) EmptyTree
      else guards reduceLeft mkOr

    def IF(tree: Tree)    = new IfStart(tree, EmptyTree)
    def TRY(tree: Tree)   = new TryStart(tree, Nil, EmptyTree)
    def BLOCK(xs: Tree*)  = Block(xs: _*)
    def NOT(tree: Tree)   = Select(tree, Boolean_not)
    def SOME(xs: Tree*)   = Apply(SomeModule, makeTupleTerm(xs.toList, true))
    def FOR(xs: Enumerator*) = new ForStart(xs.toList)
    def IMPORT(expr: Tree, names: Name*) = Import(expr, names: _*)

    /** Typed trees from symbols. */
    def THIS(sym: Symbol)             = mkAttributedThis(sym)
    def ID(sym: Symbol)               = mkAttributedIdent(sym)
    def ID(name: Name)                = Ident(name)
    def REF(sym: Symbol)              = mkAttributedRef(sym)
    def REF(pre: Type, sym: Symbol)   = mkAttributedRef(pre, sym)
    def REF(name: Name)               = Ident(name)

    def makeTupleTerm(trees: List[Tree], flattenUnary: Boolean = false): Tree = trees match {
      case Nil                        => UNIT
      case List(tree) if flattenUnary => tree
      case _                          => mkTuple(trees) // Apply(TupleClass(trees.length).companionModule, trees: _*)
    }
    def makeTupleType(trees: List[Tree], flattenUnary: Boolean = false): Tree = trees match {
      case Nil                        => scalaUnitConstr
      case List(tree) if flattenUnary => tree
      case _                          => AppliedTypeTree(REF(TupleClass(trees.length)), trees)
    }

    /** Implicits - some of these should probably disappear **/
    implicit def mkTreeMethods(target: Tree): TreeMethods = new TreeMethods(target)
    implicit def mkTreeMethodsFromSymbol(target: Symbol): TreeMethods = new TreeMethods(Ident(target))
    implicit def mkTreeMethodsFromType(target: Type): TreeMethods = new TreeMethods(TypeTree(target))
    implicit def mkSymbolMethodsFromSymbol(target: Symbol): SymbolMethods = new SymbolMethods(target)

    /** (foo DOT bar) might be simply a Select, but more likely it is to be immediately
     *  followed by an Apply.  We don't want to add an actual apply method to arbitrary
     *  trees, so SelectStart is created with an apply - and if apply is not the next
     *  thing called, the implicit from SelectStart -> Tree will provide the tree.
     */
    implicit def mkTreeFromSelectStart(ss: SelectStart): Select = ss.tree
    implicit def mkTreeMethodsFromSelectStart(ss: SelectStart): TreeMethods = mkTreeMethods(ss.tree)
    
    implicit def mkTreeFromDefStart[A <: Tree](start: DefStart[A]): A = start.empty
  }
}
