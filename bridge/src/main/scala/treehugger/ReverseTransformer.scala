package treehugger

import treehugger.forest._
import definitions._

trait ReverseTransformer[U <: scala.reflect.api.Universe] { self =>
  lazy val u: U = universe
  def universe: U
  type ATree = u.Tree
  type AName = u.Name
  type ATypeName = u.TypeName
  type ATermName = u.TermName
  type AConstant = u.Constant
  type AModifiers = u.Modifiers

  def transformEmptyTree(o: ATree): Tree = EmptyTree
  // def ClassDef(mods: Modifiers, ctormods: Modifiers, name: Name, tparams: List[TypeDef], vparams: List[ValDef], impl: Template): ClassDef
  // def PackageDef(mods: Modifiers, pid: RefTree, stats: List[Tree]): PackageDef
  // def ModuleDef(mods: Modifiers, name: Name, impl: Template): ModuleDef
  def transformValDef(o: ATree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree): Tree =
    ValDef(mods, Typed(Ident(name), tpt), rhs)
  // def DefDef(mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef
  // def AnonFunc(vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) 
  // def TypeDef(mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree): TypeDef
  // def LabelDef(name: Name, param: Tree, rhs: Tree): LabelDef
  // def Import(expr: Tree, selectors: List[ImportSelector]): Import
  // def Template(parents: List[Tree], self: ValDef, body: List[Tree]): Template
  def transformBlock(o: ATree, stats: List[Tree], expr: Tree): Tree =
    Block(stats, expr)
  // def Commented(mods: Modifiers, comment: List[String], expr: Tree): Commented
  // def CaseDef(pat: Tree, guard: Tree, body: Tree): CaseDef
  // def Alternative(trees: List[Tree]): Alternative
  // def Star(elem: Tree): Star
  // def Bind(name: Name, body: Tree): Bind
  // def UnApply(fun: Tree, args: List[Tree]): UnApply
  // def InfixUnApply(qualifier: Tree, name: Name, args: List[Tree]): InfixUnApply
  // def ArrayValue(elemtpt: Tree, trees: List[Tree]): ArrayValue
  // def Function(vparams: List[ValDef], body: Tree): Function
  // def Assign(lhs: Tree, rhs: Tree): Assign
  // def If(cond: Tree, thenp: Tree, elsep: Tree): If
  // def Match(selector: Tree, cases: List[CaseDef]): Match
  // def Return(expr: Tree): Return
  // def Try(block: Tree, catches: List[CaseDef], finalizer: Tree): Try
  // def Throw(expr: Tree): Throw
  // def New(tpt: Tree): New
  def transformTyped(o: ATree, expr: Tree, tpt: Tree): Tree =
    Typed(expr, tpt)
  // def TypeApply(fun: Tree, args: List[Tree]): TypeApply
  def transformApply(o: ATree, fun: Tree, args: List[Tree]): Tree =
    Apply(fun, args)
  // def ApplyDynamic(qual: Tree, args: List[Tree]): ApplyDynamic
  def transformSuper(o: ATree, qual: Tree, mix: TypeName): Tree =
    Super(qual, mix)
  def transformThis(o: ATree, qual: TypeName): Tree =
    This(qual)
  def transformSelect(o: ATree, qualifier: Tree, selector: TermName): Tree =
    Select(qualifier, selector)
  def transformIdent(o: ATree, name: TermName): Tree =
    Ident(name)
  def transformLiteral(o: ATree, value: Constant): Tree =
    Literal(value)
  def transformTypeTree(o: ATree): Tree =
    o match {
      case tpt: u.TypeTree =>
        if (tpt.tpe != null) TypeTree(typeRef(RootClass.newClass(tpt.tpe.toString)))
        else TypeTree()
    }
  // def Annotated(annot: Tree, arg: Tree): Annotated
  // def SingletonTypeTree(ref: Tree): SingletonTypeTree
  // def SelectFromTypeTree(qualifier: Tree, selector: Name): SelectFromTypeTree
  // def CompoundTypeTree(templ: Template): CompoundTypeTree
  // def AppliedTypeTree(tpt: Tree, args: List[Tree]): AppliedTypeTree
  // def TypeBoundsTree(lo: Tree, hi: Tree): TypeBoundsTree
  // def ExistentialTypeTree(tpt: Tree, whereClauses: List[Tree]): ExistentialTypeTree

  def transform(tree: ATree): Tree = tree match {
    case u.EmptyTree => self.transformEmptyTree(tree)
    case u.ValDef(mods, name, tpt, rhs) =>
      self.transformValDef(tree, transformMods(mods), transformTermName(name), transform(tpt), transform(rhs))
    case u.Block(stats, expr) =>
      self.transformBlock(tree, transformTrees(stats), transform(expr))
    case u.Typed(expr, tpt) =>
      self.transformTyped(tree, transform(expr), transform(tpt))
    case u.Apply(fun, args) =>
      self.transformApply(tree, transform(fun), transformTrees(args))
    case u.This(qual) =>
      self.transformThis(tree, transformTypeName(qual))
    case u.Super(qual, mix) =>
      self.transformSuper(tree, transform(qual), transformTypeName(mix))
    case u.Select(qualifier, selector) =>
      self.transformSelect(tree, transform(qualifier), transformTermName(selector))
    case u.Ident(name) =>
      self.transformIdent(tree, transformTermName(name))
    case u.Literal(value) =>
      self.transformLiteral(tree, transformConstant(value))
    case u.TypeTree() =>
      self.transformTypeTree(tree)
  }

  def transformTrees(trees: List[ATree]): List[Tree] =
    trees map {transform}
  def transformTypeName(name: ATypeName): TypeName = newTypeName(name.decoded)
  def transformTermName(name: AName): TermName = newTermName(name.decoded)
  def transformConstant(value: AConstant): Constant = Constant(value.value)
  def transformMods(mods: AModifiers): Modifiers = Modifiers()
}
