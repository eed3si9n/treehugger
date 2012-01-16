/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package treehugger

import Flags._
import api.Modifier

trait Trees extends api.Trees { self: Universe =>
  /** @param privateWithin the qualifier for a private (a type name)
   *    or tpnme.EMPTY, if none is given.
   *  @param annotations the annotations for the definition.
   *    '''Note:''' the typechecker drops these annotations,
   *    use the AnnotationInfo's (Symbol.annotations) in later phases.
   */
  case class Modifiers(flags: Long,
                       privateWithin: Name,
                       annotations: List[Tree]) extends AbsModifiers with HasFlags {
    var positions: Map[Long, Position] = Map()

    def setPositions(poss: Map[Long, Position]): this.type = {
      positions = poss; this
    }
    
    /* Abstract types from HasFlags. */
    type FlagsType          = Long
    type AccessBoundaryType = Name
    type AnnotationType     = Tree
    
    def hasAccessBoundary = privateWithin != tpnme.EMPTY
    def hasAllFlags(mask: Long): Boolean = (flags & mask) == mask
    def hasFlag(flag: Long) = (flag & flags) != 0L
    def hasFlagsToString(mask: Long): String = flagsToString(
      flags & mask,
      if (hasAccessBoundary) privateWithin.toString else ""
    )
    def defaultFlagString = hasFlagsToString(-1L)
    def & (flag: Long): Modifiers = {
      val flags1 = flags & flag
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations) setPositions positions
    }
    def &~ (flag: Long): Modifiers = {
      val flags1 = flags & (~flag)
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations) setPositions positions
    }
    def | (flag: Long): Modifiers = {
      val flags1 = flags | flag
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations) setPositions positions
    }    
    
    override def hasModifier(mod: Modifier.Value) =
      hasFlag(flagOfModifier(mod))
    override def allModifiers: Set[Modifier.Value] =
      Modifier.values filter hasModifier
    override def mapAnnotations(f: List[Tree] => List[Tree]): Modifiers =
      Modifiers(flags, privateWithin, f(annotations)) setPositions positions

    override def toString = "Modifiers(%s, %s, %s)".format(defaultFlagString, annotations mkString ", ", positions)
  }
  
  def Modifiers(flags: Long, privateWithin: Name): Modifiers = Modifiers(flags, privateWithin, List())
  def Modifiers(flags: Long): Modifiers = Modifiers(flags, tpnme.EMPTY)

  def Modifiers(mods: Set[Modifier.Value],
                privateWithin: Name,
                annotations: List[Tree]): Modifiers = {
    val flagSet = mods map flagOfModifier
    Modifiers((0L /: flagSet)(_ | _), privateWithin, annotations)
  }

  lazy val NoMods = Modifiers(0)
  
  // ---- values and creators ---------------------------------------
  
  /** @param sym       the class symbol
   *  @return          the implementation template
   */
  // def ClassDef(sym: Symbol, impl: Template): ClassDef =
  //   ClassDef(Modifiers(sym.flags),
  //              sym.name.toTypeName,
  //              sym.typeParams map TypeDef,
  //              impl) setSymbol sym
  
  /**
   *  @param sym       the class symbol
   *  @param impl      the implementation template
   */
  def ModuleDef(sym: Symbol, impl: Template): ModuleDef =
    ModuleDef(Modifiers(sym.flags), sym.name, impl) setSymbol sym
  
  def ValDef(sym: Symbol, rhs: Tree): ValDef =
    ValDef(Modifiers(sym.flags), sym.name,
             TypeTree(sym.tpe), // setPos focusPos(sym.pos),
             rhs) setSymbol sym
  
  def ValDef(sym: Symbol): ValDef = ValDef(sym, EmptyTree)
  
  object emptyValDef extends ValDef(Modifiers(PRIVATE), nme.WILDCARD, TypeTree(NoType), EmptyTree) {
    override def isEmpty = true
    super.setPos(NoPosition)
    override def setPos(pos: Position) = { assert(false); this }
  }
      
  /** A TypeDef node which defines given `sym` with given tight hand side `rhs`. */
  def TypeDef(sym: Symbol, rhs: Tree): TypeDef =
    // TypeDef(Modifiers(sym.flags), sym.name.toTypeName, sym.typeParams map TypeDef, rhs) setSymbol sym
    TypeDef(Modifiers(sym.flags), sym.name.toTypeName, Nil, rhs) setSymbol sym
  
  def TypeDef(sym: Symbol): TypeDef = TypeDef(sym, EmptyTree)
  
  /** A TypeDef node which defines abstract type or type parameter for given `sym` */
  // def TypeDef(sym: Symbol): TypeDef =
  //   TypeDef(sym, TypeBoundsTree(TypeTree(sym.info.bounds.lo), TypeTree(sym.info.bounds.hi)))

  def LabelDef(sym: Symbol, params: List[Symbol], rhs: Tree): LabelDef =
    LabelDef(sym.name, params map Ident, rhs) setSymbol sym
  
  /** casedef shorthand */
  def CaseDef(pat: Tree, body: Tree): CaseDef = CaseDef(pat, EmptyTree, body)

  def Bind(sym: Symbol, body: Tree): Bind =
    Bind(sym.name, body) setSymbol sym
  
  /** Factory method for object creation `new tpt(args_1)...(args_n)`
   *  A `New(t, as)` is expanded to: `(new t).<init>(as)`
   */
  def New(tpt: Tree, argss: List[List[Tree]]): Tree = {
    assert(!argss.isEmpty)
    val superRef: Tree = Select(New(tpt), nme.CONSTRUCTOR)
    (superRef /: argss) (Apply)
  }
  /** 0-1 argument list new, based on a symbol.
   */
  def New(sym: Symbol, args: Tree*): Tree =
    if (args.isEmpty) New(TypeTree(sym.tpe))
    else New(TypeTree(sym.tpe), List(args.toList))
  
  def Apply(sym: Symbol, args: Tree*): Tree =
    Apply(Ident(sym), args.toList)
  
  def Super(sym: Symbol, mix: TypeName): Tree = Super(This(sym), mix)
  
  def Super(sym: Symbol): Tree = Super(sym, EmptyTypeName)
  
  def Super(qual: Tree): Tree = Super(qual, EmptyTypeName)
    
  def This(sym: Symbol): Tree = This(sym.name.toTypeName) setSymbol sym
  
  /** Block factory that flattens directly nested blocks.
   */
  def Block(stats: Tree*): Block = stats match {
    case Seq(b @ Block(_, _)) => b
    case Seq(stat) => Block(Nil, stat)
    case Seq(_, rest @ _*) => Block(stats.init.toList, stats.last)
    case _ => Block(stats.toList, Literal(Constant(())))
  }
  
  def Import(tree: Tree, names: Name*): Import = Import(tree,
    names.toList map { name =>
      ImportSelector(name, -1, name, -1)
    })
}
