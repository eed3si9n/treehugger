import org.specs2._

class DSL_3ExpressionSpec2 extends DSLSpec { def is = sequential              ^
  "This is a specification to check Treehugger DSL"                           ^
                                                                              p^
  "Prefix operations are written as"                                          ^
      """`PLUS(tree)`, `MINUS(tree)`, `NOT(tree)`, and `TILDE(tree)`."""      ! unary1^
                                                                              p^
  "Postfix operations are written as"                                         ^
      """`tree POSTFIX(sym|"op")`."""                                         ! postfix1^
                                                                              p^
  "Infix operations are written as"                                           ^
      """`tree INFIX(sym|"op") APPLY arg`,"""                                 ! infix1^
      """`tree INFIX(sym|"op", arg, ...)`,"""                                 ! infix2^
      """`tree INFIX_CHAIN(sym|"op", tree, ...)`,"""                          ! infix3^
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
  "Anonymous functions are written as"                                        ^
      """`LAMBDA(PARAM(sym|"x"), ...) ==> rhs`,"""                            ! lambda1^
      """`LAMBDA(PARAM(WILDCARD)) ==> rhs`, or"""                             ! lambda2^
      """by using placeholder syntax
`WILDCARD op tree`."""                                                        ! lambda3^
                                                                              p^
  "Structural types are written as"                                           ^
      """`TYPE_STRUCT(stat, ...)`."""                                         ! struc1^
                                                                              p^
  "Types projections are written as"                                          ^
      """`typ TYPE_#("C")`."""                                                ! proj1^
                                                                              p^
  "Type paths are written as"                                                 ^
      """`TYPE_REF(tree)`."""                                                 ! type3^
                                                                              p^
  "Existential types are written as"                                          ^
      """`typ TYPE_FORSOME(tree)`."""                                         ! type4^
                                                                              p^
  "Refined types are written as"                                              ^
      """`typ TYPE_WITH(typ, ...)`."""                                        ! type5^
                                                                              p^
  "Singleton types are written as"                                            ^
      """`TYPE_SINGLETON(tree)`."""                                           ! type6^
                                                                              p^
                                                                              end
  
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  def unary1 =
    (PLUS(LIT(1)) must print_as("+(1)")) and
    (MINUS(LIT(1)) must print_as("-(1)")) and
    (NOT(FALSE) must print_as("!(false)")) and
    (TILDE(LIT(1)) must print_as("~(1)"))
  
  def postfix1 = LIT(1) POSTFIX(Any_toString) must print_as("1 toString")

  def infix1 = LIT(1) INFIX("+") APPLY LIT(2) must print_as("1 + 2")

  def infix2 = REF("x") INFIX("slice", LIT(1), LIT(2)) must print_as("x slice (1, 2)")

  def infix3 =
    (INFIX_CHAIN("+", LIT(1), LIT(2), LIT(3)) must print_as("1 + 2 + 3")) and
    (INFIX_CHAIN(Any_==, LIT(1) :: LIT(2) :: LIT(3) :: Nil) must print_as("1 == 2 == 3"))
  
  def assignment1 = (REF("x") := LIT(0)) must print_as("x = 0")

  def typed1 =
    ((LIT(0) withType(LongClass)) must print_as("(0: Long)")) and
    ((sym.foo withType(LongClass)) must print_as("(foo: Long)"))

  def conditional1 =
    (IF (REF("x") ANY_== REF("y")) THEN REF("x") ELSE LIT(0)) must print_as(
      "if (x == y) x",
      "else 0")

  def conditional2 =
    (IF (REF("sunny")) THEN (Predef_println APPLY LIT("Hi!")) ENDIF) must print_as(
      "if (sunny) println(\"Hi!\")")    
  
  def while1 =
    (WHILE(TRUE) DO BLOCK(
      Predef_println APPLY LIT("Hello")
    )) must print_as(
      """while (true) {""",
      """  println("Hello")""",
      """}"""
    )
  
  def dowhile1 =
    BLOCK(
      Predef_println APPLY LIT("Hello")
    ) DO_WHILE(REF("x") INT_< LIT(10)) must print_as(
      """do {""",
      """  println("Hello")""",
      """} while (x < 10)"""
    )
    
  def for1 =
    FOR(VALFROM("i") := LIT(0) INT_TO LIT(2)) DO(
      Predef_println APPLY LIT("Hello")
    ) must print_as(
      """for (i <- 0 to 2)""",
      """  println("Hello")""" 
    )

  def for2 =
    FOR(
      VALFROM("i") := LIT(0) INT_TO LIT(2),
      IF(REF("x") INT_< LIT(10))
    ) DO(
      Predef_println APPLY LIT("Hello")
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
      Predef_println APPLY LIT("Hello")
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
      """throw new IllegalArgumentException(x.toString)"""))
  
  def try1 =
    (TRY(REF("something") APPLY LIT(0))
    CATCH(
      CASE(WILDCARD) ==> (Predef_println APPLY LIT("error")))
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
      CASE(WILDCARD) ==> (Predef_println APPLY LIT("error")))
    FINALLY(Predef_println APPLY LIT("finally"))) must print_as(
      """try {""",
      """  something(0)""",
      """} catch {""",
      """  case _ => println("error")""",
      """} finally println("finally")"""
    )
  
  def lambda1 =
    (LAMBDA(PARAM("x")) ==> (REF("x") INT_+ LIT(1)) must print_as("x => x + 1")) and
    (LAMBDA(PARAM("x", IntClass)) ==> BLOCK(REF("x") INT_+ LIT(1)) must print_as(
      "{ (x: Int) =>",
      "  x + 1",
      "}"))
  
  def lambda2 =
    LAMBDA(PARAM(WILDCARD)) ==> (REF("x") INT_+ LIT(1)) must print_as("_ => x + 1")

  def lambda3 = (WILDCARD INT_+ WILDCARD) must print_as("_ + _")

  def struc1 =
    (sym.foo withType(TYPE_STRUCT(
      DEF("x", BigDecimalClass)
    ))) must print_as("(foo: ({ def x: BigDecimal }))")

  def proj1 =
    sym.foo withType(TYPE_STRUCT(
      TYPEVAR("L") withTypeParams(TYPEVAR("A")) := REF("Const") APPLYTYPE ("M", "A")
    ) TYPE_#("L")) must print_as("(foo: ({ type L[A] = Const[M, A] })#L)")

  def type3 =
    VAL("x", TYPE_REF(REF("A") DOT "B")).tree must print_as("val x: A.B")

  def type4 =
    (DEF("exists") withParams(PARAM("arg", TYPE_LIST(TYPE_REF(REF("x") DOT "T")) TYPE_FORSOME(
      VAL("x", "Outer")
    )))).tree must print_as("def exists(arg: List[x.T] forSome { val x: Outer })")

  def type5 =
    VAL("x", TYPE_REF("A") TYPE_WITH "B").tree must print_as("val x: A with B")

  def type6 =
    VAL("x", TYPE_SINGLETON(THIS)).tree must print_as("val x: this.type")
}
