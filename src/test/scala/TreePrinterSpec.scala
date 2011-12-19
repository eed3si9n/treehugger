import org.specs2._

import treehugger._

class TreePrinterSpec extends Specification { def is =
  "This is a specification to check a TreePrinter"                            ^
                                                                              p^
  "The tree printer should"                                                   ^
    """print println("hello, world!")"""                                      ! e1^
    """print def hello()"""                                                   ! e2^
                                                                              end
  
  lazy val universe = new treehugger.Universe
  import universe._
  import definitions._
  
  def e1 = {  
    val tree = mkMethodCall(sym.println, Literal(Constant("hello, world!")) :: Nil)
    val s = treeToString(tree); println(s)
    s must_== """println("hello, world!")"""
  }
  
  def e2 = {
    val rhs = mkMethodCall(sym.println, Literal(Constant("hello, world!")) :: Nil)
    val tree = DefDef(NoMods, newTermName("hello"), Nil, Nil, TypeTree(typeRef(UnitClass)), rhs)
    val s = treeToString(tree); println(s)
    s must_== """def hello: Unit = println("hello, world!")"""
  }
  
  object sym {
    val println = NoSymbol.newMethod(newTermName("println"))
  }
}
