package treehugger

import scala.reflect.macros.Context
import treehugger.forest._

case class MacroBridge(context: Context) {
  val bridge = new Bridge[context.universe.type] {
    def universe = context.universe
    protected def doTransformMods(mods: Modifiers): u.Modifiers =
      u.Modifiers()    
  }
  def toMacroTree = bridge.transformer
}
