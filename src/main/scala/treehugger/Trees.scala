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
  
  object emptyValDef extends ValDef(Modifiers(PRIVATE), nme.WILDCARD, TypeTree(NoType), EmptyTree) {
    override def isEmpty = true
    super.setPos(NoPosition)
    override def setPos(pos: Position) = { assert(false); this }
  }
  
  def Apply(sym: Symbol, args: Tree*): Tree =
    Apply(Ident(sym), args.toList)
}
