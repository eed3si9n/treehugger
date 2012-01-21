package treehugger

object `package` {
  lazy val forest = new Forest
  lazy val definitions = forest.definitions
  lazy val treehuggerDSL = forest.treehuggerDSL
  
  val treeToString  = forest.treeToString _
  val treesToString = forest.treesToString _
  val appliedType   = forest.appliedType _ 
  
  type Name     = forest.Name
  type TermName = forest.TermName
  type TypeName = forest.TypeName
  
  type Symbol   = forest.Symbol
  type ClassSymbol = forest.ClassSymbol
  type TermSymbol  = forest.TermSymbol
  
  type Type        = forest.Type
  val TypeBounds   = forest.TypeBounds
  
  type Tree        = forest.Tree
  type TypeTree    = forest.TypeTree
  type TypeDef     = forest.TypeDef
  type ValDef      = forest.ValDef
  type DefDef      = forest.DefDef
  
  implicit def stringToTermName(s: String): TermName = forest.newTermName(s)
  implicit def typeToTypeTree(typ: Type): TypeTree = forest.TypeTree(typ)
}
