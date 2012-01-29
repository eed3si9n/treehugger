import org.specs2._

class DSL_5PatternMatchingSpec extends DSLSpec { def is = sequential          ^
  "This is a specification to check Treehugger DSL"                           ^
                                                                              p^
  "Variable patterns are written as"                                          ^
      """`ID("x"|sym)`, or"""                                                 ! pattern1^
      """`WILDCARD`."""                                                       ! pattern2^
                                                                              p^          
  "Typed patterns are written as"                                             ^
      """`ID("x"|sym) withType(typ)`, or"""                                   ! pattern3^
      """`WILDCARD withType(typ)`."""                                         ! pattern4^
                                                                              p^
  "Pattern binders are written as"                                            ^
      """`pattern withBinder(sym|"x")`."""                                    ! pattern5^
                                                                              p^    
  "Literal patterns are written as"                                           ^
      """literals such as `LIT(0)`."""                                        ! pattern6^
                                                                              p^
  "Stable identifier patterns are written as"                                 ^
      """`BACKQUOTED("x"|sym)`."""                                            ! pattern7^
                                                                              p^
  "Constructor patterns are written as"                                       ^
      """`sym UNAPPLY(pattern, ...)`, or"""                                   ! pattern8^
      """`REF("C") UNAPPLY(pattern, ...)`."""                                 ! pattern9^
                                                                              p^
  "Tuple patterns are written as"                                             ^
      """`TUPLE(pattern1, ...)`."""                                           ! pattern10^
                                                                              p^
  "Pattern sequences are written as"                                          ^
      """`sym UNAPPLY(SEQ_WILDCARD withBinder(sym2|"xs"))`."""                ! pattern11^
                                                                              p^
  "Infix operation patterns are written as"                                   ^
      """`pattern1 INFIX("op"|sym) UNAPPLY pattern2`."""                      ! pattern12^
                                                                              p^
  "Pattern alternatives are written as"                                       ^
      """`pattern1 OR_PATTERN pattern2`."""                                   ! pattern13^
                                                                              p^
  "Pattern matching expressions are written as"                               ^
      """`tree MATCH (CASE(pattern1) ==> tree1, ...)`, or"""                  ! pattern14^
      """with guards as
`tree MATCH (CASE(pattern1, IF(guard)) ==> tree1, ...)`."""                   ! pattern15^
                                                                              p^
  "Pattern matching anonymous functions are written as"                       ^
      """`BLOCK(CASE(pattern1) ==> tree1, ...)`."""                           ! pattern16^
                                                                              p^
                                                                              end
  
  import treehugger._
  import definitions._
  import treehuggerDSL._
                                                                             
  def pattern1 = (ID("x"): Tree) must print_as("x")

  def pattern2 = (WILDCARD) must print_as("_")

  def pattern3 = ID("x") withType(IntClass) must print_as("(x: Int)")

  def pattern4 = WILDCARD withType(IntClass) must print_as("(_: Int)")

  def pattern5 = WILDCARD withBinder("x") must print_as("(x @ _)")

  def pattern6 = LIT(0) must print_as("0")

  def pattern7 = BACKQUOTED("x") must print_as("`x`")

  def pattern8 = sym.C UNAPPLY(LIT(0)) must print_as("C(0)")

  def pattern9 = REF("C") UNAPPLY(LIT(0)) must print_as("C(0)")

  def pattern10 = TUPLE(LIT(0), LIT(1)) must print_as("(0, 1)")
  
  def pattern11 = sym.C UNAPPLY(SEQ_WILDCARD withBinder("xs")) must print_as("C((xs @ _*))")

  def pattern12 = LIT(0) INFIX(ConsClass) UNAPPLY (NIL) must print_as("0 :: Nil")

  def pattern13 = LIT(0) OR_PATTERN LIT(1) must print_as("0 | 1")

  def pattern14 =
    REF("x") MATCH(
      CASE (LIT(0) OR_PATTERN LIT(1)) ==> TRUE,
      CASE (WILDCARD) ==> FALSE
    ) must print_as(
      """x match {""",
      """  case 0 | 1 => true""",
      """  case _ => false""",
      """}"""
    )

  def pattern15 =
    REF("x") MATCH(
      CASE (ID("x"),
        IF(REF("x") INT_< LIT(10))) ==> TRUE,
      CASE (WILDCARD) ==> FALSE
    ) must print_as(
      """x match {""",
      """  case x if x < 10 => true""",
      """  case _ => false""",
      """}"""
    )
  
  def pattern16 =
    BLOCK(
      CASE (TUPLE(ID("a"), TUPLE(ID("b"), ID("c")))) ==> REF("a") INT_+ REF("b") INT_* REF("c")
    ) must print_as(
      """{""",
      """  case (a, (b, c)) => a + b * c""",
      """}"""
    )
}
