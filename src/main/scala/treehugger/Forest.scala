package treehugger

class Forest extends api.Universe
                           with StdNames
                           with Definitions
                           with Symbols
                           with Types
                           with Constants
                           with Scopes
                           with Names
                           with Trees
                           with AnnotationInfos
                           with TreePrinters
                           with TreeGen
                           with TreehuggerDSLs
{
  type Position = String
  val NoPosition: Position = ""
  val forMSIL: Boolean = false
  
  type TreeCopier = TreeCopierOps
  def newStrictTreeCopier: TreeCopier = new StrictTreeCopier
  def newLazyTreeCopier: TreeCopier = new LazyTreeCopier
  
  definitions.init()
  val Flags = treehugger.Flags
}
