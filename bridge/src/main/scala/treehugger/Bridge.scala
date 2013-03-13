package treehugger

import treehugger.forest._

trait Bridge[U <: scala.reflect.api.Universe] {
  lazy val u: U = universe
  def universe: U
  type ATree = u.Tree

  def transformer = new AbstractTransformer[ATree] {  
    type ATypeName = u.TypeName
    type ATermName = u.TermName
    type AConstant = u.Constant
    type AModifiers = u.Modifiers

    override def transform(tree: Tree): ATree =
      super.transform(tree.desugar)

    def transformEmptyTree(o: Tree) =
      u.EmptyTree
    def transformBlock(o: Tree, stats: List[ATree], expr: ATree): ATree =
      u.Block(stats, expr)
    def transformValDef(o: Tree, mods: AModifiers, lhs: ATree, rhs: ATree): ATree =
      lhs match {
        case u.Typed(u.Ident(name), tpt) => u.ValDef(mods, name.toTermName, tpt, rhs)
      }
    def transformDefDef(o: Tree, mods: AModifiers, name: ATermName, tparams: List[ATree],
        vparamss: List[List[ATree]], tpt: ATree, rhs: ATree): ATree =
      u.DefDef(mods, name, tparams  map {
        case tparam: u.TypeDef => tparam
      }, vparamss map { _ map {
        case vparam: u.ValDef => vparam
      }}, tpt, rhs)
    def transformTyped(o: Tree, expr: ATree, tpt: ATree): ATree =
      u.Typed(expr, tpt)
    def transformApply(o: Tree, fun: ATree, args: List[ATree]): ATree =
      u.Apply(fun, args)
    def transformInfix(o: Tree, qualifier: ATree, name: ATermName, args: List[ATree]): ATree =
      matchError(o)
    def transformSuper(o: Tree, qual: ATree, mix: ATypeName): ATree =
      u.Super(qual, mix)
    def transformThis(o: Tree, qual: ATypeName) =
      u.This(qual)
    def transformSelect(o: Tree, qualifier: ATree, selector: ATermName): ATree =
      u.Select(qualifier, selector)
    def transformIdent(o: Tree, name: ATermName): ATree =
      u.Ident(name)
    def transformLiteral(o: Tree, value: AConstant): ATree =
      u.Literal(value)
    def transformTypeTree(o: Tree): ATree = o match {
      case tree: TypeTree =>
        if (tree.hasType) u.Ident(u.newTypeName(tree.tpe.toString))
        else u.TypeTree()
    }
    def transformTypeName(name: TypeName): ATypeName = u.newTypeName(name.name)
    def transformTermName(name: TermName): ATermName = u.newTermName(name.name)
    def transformConstant(value: Constant): AConstant = u.Constant(value.value)
    def transformMods(mods: Modifiers): AModifiers = doTransformMods(mods)
    def matchError(tree: Tree) = throw new MatchError(tree)
  }

  protected def doTransformMods(mods: Modifiers): u.Modifiers
}
