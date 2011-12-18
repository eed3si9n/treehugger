import org.specs2._

import treehugger._

class TreePrinterSpec extends Specification { def is =
  "This is a specification to check a TreePrinter"                            ^
                                                                              p^
  "The tree printer should"                                                   ^
    """print println("hello, world!")"""                                      ! e1^
                                                                              end
  
  def e1 = {
    import universe._
    
    val tree = mkMethodCall(sym.println, Literal(Constant("hello, world!")) :: Nil)
    treeToString(tree) must_== """println("hello, world!")"""
  }
  
  object sym {
    import universe._
    val println = NoSymbol.newMethod(newTermName("println"))
  }
  
  lazy val universe = new treehugger.Universe
}
