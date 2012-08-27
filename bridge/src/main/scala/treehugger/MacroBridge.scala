package treehugger

import scala.reflect.macros.Context
import treehugger.forest._

case class MacroBridge[C <: Context](context: C) {
  val bridge = new Bridge[context.universe.type] {
    def universe = context.universe
    protected def doTransformMods(mods: Modifiers): u.Modifiers =
      u.Modifiers()    
  }
  def toMacroTree(tree: Tree): context.universe.Tree =
    bridge.transformer.transform(tree)
  
  val reverseBridge = new ReverseTransformer[context.universe.type] {
    override type ATree = context.universe.Tree
    def universe = context.universe
  }
  def fromMacroTree(tree: context.universe.Tree): Tree =
    reverseBridge.transform(tree)
}
