package treehugger

import treehugger.forest._

case object RuntimeBridge {
  val bridge = new Bridge[reflect.runtime.universe.type] {
    def universe = reflect.runtime.universe
    protected def doTransformMods(mods: Modifiers): u.Modifiers =
      u.Modifiers()  
  }
  def toRuntimeTree = bridge.transformer
}
