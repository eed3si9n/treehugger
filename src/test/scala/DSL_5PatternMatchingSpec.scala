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
   
}
