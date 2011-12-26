import org.specs2._

import treehugger._

class TreePrinterSpec extends Specification { def is =
  "This is a specification to check a TreePrinter"                            ^
                                                                              p^
  "The tree printer should"                                                   ^
    """print println("Hello, world!")"""                                      ! e1^
    """print def hello()"""                                                   ! e2^
    """print val greetStrings = new Array[String](3)"""                       ! e3^
                                                                              end
  
  lazy val universe = new treehugger.Universe
  import universe._
  import definitions._
  import CODE._
  
  def e1 = {  
    val tree = sym.println APPLY LIT("Hello, world!")
    val s = treeToString(tree); println(s)
    
    s must_== """println("Hello, world!")"""
  }
  
  def e2 = {
    val tree = DEF("hello", UnitClass.typeConstructor) :=
      BLOCK(sym.println APPLY LIT("Hello, world!"))
    val s = treeToString(tree); println(s)
    
    s.lines.toList must contain(
      """def hello(): Unit = {""",
      """  println("Hello, world!");""",
      """  ()""",
      """}"""
    ).inOrder
  }
  
  // p. 38
  def e3 = {
    val greetStrings = RootClass.newValue("greetStrings")
    def assignGreetStrings(index: Int, value: String): Tree =
      greetStrings APPLY LIT(index) := LIT(value)
    
    val trees = (VAL(greetStrings) := NEW(arrayType(StringClass.typeConstructor), LIT(3))) ::
      assignGreetStrings(0, "Hello") ::
      assignGreetStrings(1, ", ") ::
      assignGreetStrings(2, "world!\n") ::
      ForTree(ValFrom(Ident("i"), Infix(LIT(0), sym.to, LIT(2) :: Nil)) :: Nil,
        sym.print APPLY (greetStrings APPLY Ident("i")) ) ::
      Nil
    
    val s = treesToString(trees); println(s)
    
    s.lines.toList must contain(
      """val greetStrings = new Array[String](3)""",
      """greetStrings(0) = "Hello"""",
      """greetStrings(1) = ", """",
      """greetStrings(2) = "world!\n"""",
      """for (i <- 0 to 2)""",
      """  print(greetStrings(i))"""
    ).inOrder
  }
  
  object sym {
    val println = ScalaPackageClass.newMethod("println")
    val print = ScalaPackageClass.newMethod("print")
    val to = ScalaPackageClass.newMethod("to")
  }
}
