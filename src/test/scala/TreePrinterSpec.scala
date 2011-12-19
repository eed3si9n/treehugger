import org.specs2._

import treehugger._

class TreePrinterSpec extends Specification { def is =
  "This is a specification to check a TreePrinter"                            ^
                                                                              p^
  "The tree printer should"                                                   ^
    """print println("Hello, world!")"""                                      ! e1^
    """print def hello()"""                                                   ! e2^
                                                                              end
  
  lazy val universe = new treehugger.Universe
  import universe._
  import definitions._
  
  def e1 = {  
    val tree = mkMethodCall(sym.println, Literal(Constant("Hello, world!")) :: Nil)
    val s = treeToString(tree); println(s)
    
    s must_== """println("Hello, world!")"""
  }
  
  def e2 = {
    val rhs = Block(Nil, mkMethodCall(sym.println, Literal(Constant("Hello, world!")) :: Nil))
    val tree = DefDef(NoMods, newTermName("hello"), Nil, Nil, TypeTree(typeRef(UnitClass)), rhs)
    val s = treeToString(tree); println(s)
    
    s.lines.toList must contain(
      """def hello: Unit = {""",
      """  println("Hello, world!")""",
      """}"""
    ).inOrder
  }
  
  object sym {
    val println = NoSymbol.newMethod(newTermName("println"))
  }
}
