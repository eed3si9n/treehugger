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
  val NL = "\n"
  import universe._
  import definitions._
  
  def e1 = {  
    val tree = mkMethodCall(sym.println, mkLiteral("Hello, world!") :: Nil)
    val s = treeToString(tree); println(s)
    
    s must_== """println("Hello, world!")"""
  }
  
  def e2 = {
    val rhs = Block(mkMethodCall(sym.println, mkLiteral("Hello, world!") :: Nil))
    val tree = DefDef(NoMods, "hello", Nil, Nil, TypeTree(UnitClass.typeConstructor), rhs)
    val s = treeToString(tree); println(s)
    
    s.lines.toList must contain(
      """def hello: Unit = {""",
      """  println("Hello, world!");""",
      """  ()""",
      """}"""
    ).inOrder
  }
  
  // p. 38
  def e3 = {
    val greetStrings = RootClass.newValue("greetStrings")
    val exp1 = ValDef(greetStrings,
      New(Apply(TypeTree(arrayType(StringClass.typeConstructor)), mkLiteral(3) :: Nil)))
    def assignGreetStrings(index: Int, value: String): Tree =
      Assign(Apply(greetStrings, mkLiteral(index)), mkLiteral(value))
    val exp2 = assignGreetStrings(0, "Hello")
    val exp3 = assignGreetStrings(1, ", ")
    val exp4 = assignGreetStrings(2, "world!\n")
    val exp5 = ForTree(ValFrom(Ident("i"), mkMethodCall(mkLiteral(0), sym.to, Nil, mkLiteral(2) :: Nil)) :: Nil,
      mkMethodCall(sym.println, Ident("i") :: Nil))
    
    val s = treeToString(List(exp1, NL, exp2, NL, exp3, NL, exp4, NL, exp5): _*); println(s)
    
    s.lines.toList must contain(
      """val greetStrings = new Array[String](3)""",
      """greetStrings(0) = "Hello"""",
      """greetStrings(1) = ", """",
      """greetStrings(2) = "world!\n""""
    ).inOrder
  }
  
  object sym {
    val println = ScalaPackageClass.newMethod("println")
    val to = ScalaPackageClass.newMethod("to")
  }
}
