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
`SUPER TYPEAPPLY "T"`."""                                                     ! super3^      
                                                                              p^
  "Function applications are written as"                                      ^
      """`sym APPLY arg` where `arg` is a tree,"""                            ! apply1^
      """`tree APPLY arg`, or"""                                              ! apply2^
      """as a shorthand for application on a selection
`(sym1 DOT sym2)(arg)`."""                                                    ! apply3^
                                                                              end^
  "Sequence arguments are written as"                                         ^
      """`sym APPLY SEQARG(arg)"` to pass a sequence into vararg."""          ! seqarg1^
                                                                              end^
  "Named arguments are written as"                                            ^
      """`sym APPLY (REF(sym1) := arg)"`."""                                  ! namedarg1^
                                                                              end^
  "Method values are written as"                                              ^
      """`sym APPLY WILDCARD`."""                                             ! methodvalue1^
                                                                              p^
  "Type applications are written as"                                          ^
      """`sym TYPEAPPLY typ`."""                                              ! typeapply1^
                                                                              p^
  "Tuples are written as"                                                     ^
      """`TUPLE(tree1, tree2, ...)`."""                                       ! tuple1^
                                                                              p^
  "Instance creations are written as"                                         ^
      """`NEW(typ)`, or"""                                                    ! new1^
      """with arguments to the constructor as
`NEW(typ, arg1, arg2, ...)`"""                                                ! new2^
      """Using `ANONDEF()`, instance creations with a class template are written as
`NEW(ANONDEF(parent1, ...) := BLOCK(stat, ...))` where `parent1` is a type.""" ! new3^  
                                                                              p^
  "Blocks are written as"                                                     ^
      """`BLOCK(stat, ...)`."""                                               ! block1^
                                                                              p^    
  "Prefix operations are written as"                                          ^
      """`PLUS(tree)`, `MINUS(tree)`, `NOT(tree)`, and `TILDE(tree)`."""      ! unary1^
                                                                              p^
  "Infix operations are written as"                                           ^
      """`tree INFIX(sym|"op") APPLY arg`, or"""                              ! infix1^
      """`tree INFIX(sym|"op", arg, ...)`."""                                 ! infix2^
                                                                              p^
  "Assignments are written as"                                                ^
      """`tree := rhs`."""                                                    ! assignment1^
                                                                              p^
  "Typed expressions are written as"                                          ^
      """`tree withType(type)` or `sym withType(typ)`."""                     ! typed1^
                                                                              p^  
  "Conditional expressions are written as"                                    ^
      """`IF (tree1) THEN tree2 ELSE tree3`, or"""                            ! conditional1^
      """`IF (tree1) THEN tree2 ENDIF`."""                                    ! conditional2^
                                                                              p^
  "While loop expressions are written as"                                     ^
      """`WHILE (tree1) DO tree2`."""                                         ! while1^
                                                                              p^
  "Do while loop expressions are written as"                                  ^
      """`tree1 DO_WHILE(tree2)`."""                                          ! dowhile1^
                                                                              p^
  "For loop expressions are written as"                                       ^
      """`FOR (enum, ...) DO tree` where `enum` is an enumerator such as
`VALFROM(sym|"x") := tree`,"""                                                ! for1^
      """`IF(tree)`, and"""                                                   ! for2^
      """`VAL(sym|"x") := rhs`."""                                            ! for3^
                                                                              end^
  "For comprehensions are written as"                                         ^
      """`FOR (enum, ...) YIELD tree`."""                                     ! for4^
                                                                              p^
  "Return expressions are written as"                                         ^
      """`RETURN tree`."""                                                    ! return1^
                                                                              p^
  "Throw expressions are written as"                                          ^
      """`THROW(typ)` where `typ` is an exception class."""                   ! throw1^
      """An error message can be passed in as
`THROW(typ, "oh no")` or `THROW(typ, tree)`"""                                ! throw2^
                                                                              p^
  "Try expressions are written as"                                            ^
      """`TRY(stat, ...) CATCH(CASE(pat) ==> tree, ...) ENDTRY`, or"""        ! try1^
      """with a finally clause as
`TRY(stat, ...) CATCH(CASE(pat) ==> tree, ...) FINALLY(tree2)`"""             ! try2^
                                                                              p^
                                                                              end
  
  import treehugger._
  import definitions._
  import treehuggerDSL._
                                                                             
  def term1 = REF("x") must print_as("x")
  
  def term2 = {
    val sym1 = RootClass.newValue("x")
    val sym2 = sym.Addressable.newValue("y")
    
    ((sym1 DOT sym2: Tree) must print_as("x.y")) and
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
    
  def super3 = (SUPER TYPEAPPLY sym.T) must print_as("super[T]")
  
  def apply1 = (sym.println APPLY LIT("Hello, world!")) must print_as("""println("Hello, world!")""")
  
  def apply2 = (REF("x") DOT "y" APPLY LIT("Hello, world!")) must print_as("""x.y("Hello, world!")""")
  
  def apply3 = {
    val sym1 = RootClass.newValue("x")
    val sym2 = sym.Addressable.newValue("y")
    
    (((sym1 DOT sym2)(LIT("Hello"))) must print_as("""x.y("Hello")"""))
  }
  
  def seqarg1 = (THIS APPLY SEQARG(REF("list"))) must print_as("this((list: _*))")
  
  def namedarg1 = (REF("put") APPLY (REF("x") := LIT(0))) must print_as("put(x = 0)")
  
  def methodvalue1 = (REF("put") APPLY WILDCARD) must print_as("put(_)")

  def typeapply1 = (REF("put") TYPEAPPLY sym.T) must print_as("put[T]")
  
  def tuple1 = TUPLE(LIT(0), LIT(1)) must print_as("Tuple2(0, 1)")

  def new1 = NEW(sym.T) must print_as("new T")

  def new2 = NEW(sym.T, LIT(0), LIT(1)) must print_as("new T(0, 1)")

  def new3 =
    (NEW(ANONDEF() := BLOCK(
      DEF("get") := LIT(0)
    )) must print_as(
      """new {""",
      """  def get = 0""",
      """}"""
    )) and
    (NEW(ANONDEF(sym.Addressable) := BLOCK(
      DEF("get") := LIT(0)
    )) must print_as(
      """new Addressable {""",
      """  def get = 0""",
      """}"""    
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
  
  def unary1 =
    (PLUS(LIT(1)) must print_as("+(1)")) and
    (MINUS(LIT(1)) must print_as("-(1)")) and
    (NOT(FALSE) must print_as("!(false)")) and
    (TILDE(LIT(1)) must print_as("~(1)"))
  
  def infix1 = LIT(1) INFIX("+") APPLY LIT(2) must print_as("1 + 2")

  def infix2 = REF("x") INFIX("slice", LIT(1), LIT(2)) must print_as("x slice (1, 2)")
  
  def assignment1 = (REF("x") := LIT(0)) must print_as("x = 0")

  def typed1 =
    ((LIT(0) withType(LongClass)) must print_as("(0: Long)")) and
    ((sym.foo withType(LongClass)) must print_as("(foo: Long)"))

  def conditional1 =
    (IF (REF("x") ANY_== REF("y")) THEN REF("x") ELSE LIT(0)) must print_as(
      "if (x == y) x",
      "else 0")

  def conditional2 =
    (IF (REF("x") ANY_== REF("y")) THEN REF("x") ENDIF) must print_as(
      "if (x == y) x")    
  
  def while1 =
    (WHILE(TRUE) DO (
      sym.println APPLY LIT("Hello")
    )) must print_as(
      """while (true) {""",
      """  println("Hello")""",
      """}"""
    )
  
  def dowhile1 =
    BLOCK(
      sym.println APPLY LIT("Hello")
    ) DO_WHILE(REF("x") INT_< LIT(10)) must print_as(
      """do {""",
      """  println("Hello")""",
      """} while (x < 10)"""
    )
    
  def for1 =
    FOR(VALFROM("i") := LIT(0) INT_TO LIT(2)) DO(
      sym.println APPLY LIT("Hello")
    ) must print_as(
      """for (i <- 0 to 2)""",
      """  println("Hello")""" 
    )

  def for2 =
    FOR(
      VALFROM("i") := LIT(0) INT_TO LIT(2),
      IF(REF("x") INT_< LIT(10))
    ) DO(
      sym.println APPLY LIT("Hello")
    ) must print_as(
      """for {""",
      """  i <- 0 to 2""",
      """  if x < 10""",
      """} println("Hello")""" 
    )

  def for3 =
    FOR(
      VALFROM("i") := LIT(0) INT_TO LIT(2),
      VAL("x") := REF("i")
    ) DO(
      sym.println APPLY LIT("Hello")
    ) must print_as(
      """for {""",
      """  i <- 0 to 2""",
      """  x = i""",
      """} println("Hello")""" 
    )

  def for4 =
    FOR(VALFROM("i") := LIT(0) INT_TO LIT(2)) YIELD REF("i") must print_as(
      """for (i <- 0 to 2)""",
      """  yield i"""
    )

  def return1 = RETURN(LIT(0)) must print_as("return 0")

  def throw1 = THROW(IllegalArgumentExceptionClass) must print_as("throw new IllegalArgumentException()")
  
  def throw2 =
    (THROW(IllegalArgumentExceptionClass, "oh no") must print_as(
      """throw new IllegalArgumentException("oh no")""")) and
    (THROW(IllegalArgumentExceptionClass, REF("x")) must print_as(
      """throw new IllegalArgumentException(x.toString())"""))
  
  def try1 =
    (TRY(REF("something") APPLY LIT(0))
    CATCH(
      CASE(WILDCARD) ==> (sym.println APPLY LIT("error")))
    ENDTRY) must print_as(
      """try {""",
      """  something(0)""",
      """} catch {""",
      """  case _ => println("error")""",
      """}"""
    )

  def try2 =
    (TRY(REF("something") APPLY LIT(0))
    CATCH(
      CASE(WILDCARD) ==> (sym.println APPLY LIT("error")))
    FINALLY(sym.println APPLY LIT("finally"))) must print_as(
      """try {""",
      """  something(0)""",
      """} catch {""",
      """  case _ => println("error")""",
      """} finally println("finally")"""
    )
}
