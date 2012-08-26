package treehugger

import treehugger.forest._

case object RuntimeBridge {
  val bridge = new Bridge[reflect.runtime.universe.type] {
    def universe = reflect.runtime.universe
    protected def doTransformMods(mods: Modifiers): u.Modifiers =
      u.Modifiers()  
  }
  def toRuntimeTree(tree: Tree): reflect.runtime.universe.Tree =
    bridge.transformer.transform(tree)

  val reverseBridge = new ReverseTransformer[reflect.runtime.universe.type] {
    override type ATree = reflect.runtime.universe.Tree
    def universe = reflect.runtime.universe
  }
  def fromRuntimeTree(tree: reflect.runtime.universe.Tree): Tree =
    reverseBridge.transform(tree)
}
