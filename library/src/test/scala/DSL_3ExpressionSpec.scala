import org.specs2._

class DSL_3ExpressionSpec extends DSLSpec { def is = sequential               ^
  "This is a specification to check Treehugger DSL"                           ^
                                                                              p^
  "Named terms can be written as"                                             ^
      """`REF(sym|"x")` to refer to values and methods."""                    ! term1^
      """Selections are written either as
`sym1 DOT sym2`, `sym1 DOT "y"`, or `REF("x"") DOT "y"` where `Tree`s are expected.""" ! term2^ 
                                                                              end^
  "References to `this` are written as"                                       ^
      """`THIS` or"""                                                         ! this1^
      """with a qualifier as `THIS(sym|"C")`."""                              ! this2^
                                                                              end^
  "References to `super` are written as"                                      ^
      """`SUPER`, or"""                                                       ! super1^
      """with a qualifier as `SUPER(sym|"C")` where `Tree`s are expected."""  ! super2^
      """A trait qualifier may be added as
`SUPER APPLYTYPE "T"`."""                                                     ! super3^      
                                                                              p^
  "Function applications are written as"                                      ^
      """`sym APPLY (arg, ...)` where `arg` is a tree,"""                     ! apply1^
      """`tree APPLY (arg, ...)`,"""                                          ! apply2^
      """`tree APPLY (arg :: Nil)`, or"""                                     ! apply3^
      """as a shorthand for application on a selection
`(sym1 DOT sym2)(arg, ...)`."""                                               ! apply4^
                                                                              end^
  "Sequence arguments are written as"                                         ^
      """`sym APPLY SEQARG(arg)"` to pass a sequence into vararg."""          ! seqarg1^
                                                                              end^
  "Named arguments are written as"                                            ^
      """`sym APPLY (REF(sym1) := arg)"`."""                                  ! namedarg1^
                                                                              end^
  "Method values are written as"                                              ^
      """`sym APPLY PARTIALLY`."""                                            ! methodvalue1^
                                                                              p^
  "Type applications are written as"                                          ^
      """`sym APPLYTYPE (typ|"T", ...)`."""                                   ! typeapply1^
                                                                              p^
  "Tuples are written as"                                                     ^
      """`TUPLE(tree1, tree2, ...)`."""                                       ! tuple1^
                                                                              p^
  "Parentheses are written as"                                                ^
      """`PAREN(tree)`."""                                                    ! paren1^
                                                                              p^ 
  "Instance creations are written as"                                         ^
      """`NEW(typ|"C")`, or"""                                                ! new1^
      """with arguments to the constructor as
`NEW(typ, arg1, arg2, ...)`"""                                                ! new2^
      """Using `ANONDEF()`, instance creations with a class template are written as
`NEW(ANONDEF(parent1, ...) := BLOCK(stat, ...))` where `parent1` is a type.""" ! new3^  
                                                                              p^
  "Blocks are written as"                                                     ^
      """`BLOCK(stat, ...)`."""                                               ! block1^
                                                                              p^    
                                                                              end

  import treehugger.forest._
  import definitions._
  import treehuggerDSL._
                                                                             
  def term1 = REF("x") must print_as("x")
  
  def term2 = {
    val sym1 = RootClass.newValue("x")
    val sym2 = sym.Addressable.newValue("y")
    
    ((sym1 DOT sym2).tree must print_as("x.y")) and
    ((sym1 DOT "y": Tree) must print_as("x.y")) and
    ((REF("x") DOT "y": Tree) must print_as("x.y"))
  }
  
  def this1 = THIS must print_as("this")
  
  def this2 =
    (THIS(sym.T) must print_as("T.this")) and
    (THIS("T") must print_as("T.this"))
  
  def super1 = (SUPER: Tree) must print_as("super")
  
  def super2 =
    ((SUPER(sym.T): Tree) must print_as("T.super")) and
    ((SUPER("T"): Tree) must print_as("T.super"))
    
  def super3 = (SUPER APPLYTYPE sym.T) must print_as("super[T]")
  
  def apply1 = (Predef_println APPLY LIT("Hello, world!")) must print_as("""println("Hello, world!")""")
  
  def apply2 = (REF("x") DOT "y" APPLY (LIT(0), LIT(1)))  must print_as("""x.y(0, 1)""")
  
  def apply3 = (REF("x") DOT "y" APPLY ((REF("w") DOT "t") :: Nil))  must print_as("""x.y(w.t)""")

  def apply4 = {
    val sym1 = RootClass.newValue("x")
    val sym2 = sym.Addressable.newValue("y")
    
    (((sym1 DOT sym2)(LIT("Hello"))) must print_as("""x.y("Hello")"""))
  }
  
  def seqarg1 = (THIS APPLY SEQARG(REF("list"))) must print_as("this((list: _*))")
  
  def namedarg1 = (REF("put") APPLY (REF("x") := LIT(0))) must print_as("put(x = 0)")
  
  def methodvalue1 = (REF("put") APPLY PARTIALLY) must print_as("put _")

  def typeapply1 =
    (REF("put") APPLYTYPE(IntClass) APPLY(LIT(0))) must print_as("put[Int](0)")
  
  def tuple1 = TUPLE(LIT(0), LIT(1)) must print_as("(0, 1)")

  def paren1 = PAREN(LIT(0)) must print_as("(0)")

  def new1 =
    (NEW(sym.T) must print_as("new T")) and
    (NEW("C") must print_as("new C")) and
    (NEW(REF("B") DOT "C") must print_as("new B.C"))

  def new2 = NEW(sym.T, LIT(0), LIT(1)) must print_as("new T(0, 1)")

  def new3 =
    (NEW(ANONDEF() := BLOCK(
      DEF("name") := LIT("Robert")
    )) must print_as(
      "new {",
      "  def name = \"Robert\"",
      "}"
    )) and
    (NEW(ANONDEF("C") := BLOCK(
      DEF("name") := LIT("Robert")
    )) must print_as(
      "new C {",
      "  def name = \"Robert\"",
      "}"    
    )) and
    (NEW(ANONDEF("C") withEarlyDefs(
      VAL("name") := LIT("Robert")
    )) must print_as(
      "new {",
      "  val name = \"Robert\"",
      "} with C"
    ))
  
  def block1 =
    BLOCK(
      VAL("x") := LIT(1),
      LIT(0)
    ) must print_as(
      """{""",
      """  val x = 1""",
      """  0""",
      """}"""
    )
}
