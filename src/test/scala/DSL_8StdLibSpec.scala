import org.specs2._

class DSL_8StdLibSpec extends DSLSpec { def is = sequential                   ^
  "This is a specification to check Treehugger DSL"                           ^
                                                                              p^
  "Built-in operators are written as"                                         ^
      """`tree OR tree` for Boolean || method,"""                             ! boolean1^
      """`tree AND tree` for Boolean && method,"""                            ! boolean2^
      """`tree ANY_== tree` for Any == method,"""                             ! any1^
      """`tree ANY_!= tree` for Any != method,"""                             ! any2^
      """`tree ANY_-> tree` for ArrowAssoc -> method,"""                      ! any3^
      """`tree OBJ_EQ tree` for AnyRef eq method,"""                          ! anyref1^
      """`tree OBJ_NE tree` for AnyRef ne method,"""                          ! anyref2^
      """`tree INT_| tree` for numeric | method,"""                           ! int1^
      """`tree INT_& tree` for numeric & method,"""                           ! int2^    
      """`tree INT_>= tree` for numeric >= method,"""                         ! int3^
      """`tree INT_== tree` for numeric == method,"""                         ! int4^
      """`tree INT_!= tree` for numeric != method,"""                         ! int5^
      """`tree INT_<= tree` for numeric <= method,"""                         ! int6^
      """`tree INT_< tree` for numeric < method,"""                           ! int7^
      """`tree INT_> tree` for numeric > method,"""                           ! int8^
      """`tree INT_+ tree` for numeric + method,"""                           ! int9^
      """`tree INT_- tree` for numeric - method,"""                           ! int10^
      """`tree INT_* tree` for numeric * method,"""                           ! int11^
      """`tree INT_/ tree` for numeric / method,"""                           ! int12^
      """`tree INT_TO tree` for RichInt to method,"""                         ! int13^
      """`tree LIST_:: tree` for List :: method,"""                           ! list1^
      """`tree LIST_::: tree` for List ::: method,"""                         ! list2^
      """`tree FOREACH LAMBDA(VAL("x")) ===> BLOCK(stat, ...)` for collection foreach method,"""  ! traversable1^
      """`tree MAP LAMBDA(VAL("x")) ===> BLOCK(stat, ...)` for collection map method,"""  ! traversable2^
      """`tree FILTER LAMBDA(VAL("x")) ===> BLOCK(stat, ...)` for collection filter method,"""  ! traversable3^
      """`tree FLATMAP LAMBDA(VAL("x")) ===> BLOCK(stat, ...)` for collection flatMap method,"""  ! traversable4^
      """`tree COLLECT BLOCK(CASE(pattern) ===> tree, ...)` for collection collect method,"""  ! traversable5^
                                                                              p^
                                                                              end
  
  import treehugger._
  import definitions._
  import treehuggerDSL._
                                                                             
  def boolean1 = (REF("x") OR REF("y")) must print_as("(x) || (y)")

  def boolean2 = (REF("x") AND REF("y")) must print_as("(x) && (y)")
  
  def any1 = (REF("x") ANY_== REF("y")) must print_as("x == y")
  
  def any2 = (REF("x") ANY_!= REF("y")) must print_as("x != y")
  
  def any3 = (REF("x") ANY_-> REF("y")) must print_as("x -> y")

  def anyref1 = (REF("x") OBJ_EQ REF("y")) must print_as("x eq y")
  
  def anyref2 = (REF("x") OBJ_NE REF("y")) must print_as("x ne y")
 
  def int1 = (REF("x") INT_| REF("y")) must print_as("x | y")
  
  def int2 = (REF("x") INT_& REF("y")) must print_as("x & y")

  def int3 = (REF("x") INT_>= REF("y")) must print_as("x >= y")
  
  def int4 = (REF("x") INT_== REF("y")) must print_as("x == y")

  def int5 = (REF("x") INT_!= REF("y")) must print_as("x != y")
  
  def int6 = (REF("x") INT_<= REF("y")) must print_as("x <= y")

  def int7 = (REF("x") INT_< REF("y")) must print_as("x < y")
  
  def int8 = (REF("x") INT_> REF("y")) must print_as("x > y")

  def int9 = (REF("x") INT_+ REF("y")) must print_as("x + y")
  
  def int10 = (REF("x") INT_- REF("y")) must print_as("x - y")

  def int11 = (REF("x") INT_* REF("y")) must print_as("x * y")
  
  def int12 = (REF("x") INT_/ REF("y")) must print_as("x / y")

  def int13 = (REF("x") INT_TO REF("y")) must print_as("x to y")
  
  def list1 = (REF("x") LIST_:: NIL) must print_as("x :: Nil")

  def list2 = (REF("x") LIST_::: NIL) must print_as("x ::: Nil")

  def traversable1 =
    (REF("foo") FOREACH LAMBDA(VAL("x")) ==> BLOCK(
      REF("x") APPLY())) must print_as(
      "foo foreach { x =>",
      "  x()",
      "}"
    )
  
  def traversable2 =
    (REF("foo") MAP LAMBDA(VAL("x")) ==> BLOCK(
      REF("x"))) must print_as(
      "foo map { x =>",
      "  x",
      "}"
    )
  
  def traversable3 =
    (REF("foo") FILTER LAMBDA(VAL("x")) ==> BLOCK(
      REF("x"))) must print_as(
      "foo filter { x =>",
      "  x",
      "}"
    )

  def traversable4 =
    (REF("foo") FLATMAP LAMBDA(VAL("x")) ==> BLOCK(
      SOME(REF("x")))) must print_as(
      "foo flatMap { x =>",
      "  Some(x)",
      "}"
    )

  def traversable5 =
    (REF("foo") COLLECT BLOCK(
      CASE(LIT(0)) ==> LIT(0))) must print_as(
      "foo collect {",
      "  case 0 => 0",
      "}"
    )
}
