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
      """`tree FOREACH LAMBDA(PARAM("x")) ===> BLOCK(stat, ...)`
  for collection foreach method,"""                                           ! traversable1^
      """`tree MAP LAMBDA(PARAM("x")) ===> BLOCK(stat, ...)`
  for collection map method,"""                                               ! traversable2^
      """`tree FILTER LAMBDA(PARAM("x")) ===> BLOCK(stat, ...)`
  for collection filter method,"""                                            ! traversable3^
      """`tree FLATMAP LAMBDA(PARAM("x")) ===> BLOCK(stat, ...)`
  for collection flatMap method,"""                                           ! traversable4^
      """`tree COLLECT BLOCK(CASE(pattern) ===> tree, ...)`
  for collection collect method,"""                                           ! traversable5^
      """`tree SEQ_/: tree` for collection /: method,"""                      ! traversable6^
      """`tree SEQ_\: tree` for collection \: method,"""                      ! traversable7^
                                                                              p^
  "Built-in constructors are written as"                                      ^
      """`LIST(tree, ...)` for List"""                                        ! listctor1^
      """`NIL` for Nil"""                                                     ! listctor2^
      """`tree UNLIST_::: tree` for ::"""                                     ! listctor3^
      """`SOME(tree, ...)` for Some"""                                        ! optionctor1^
      """`NONE` for None"""                                                   ! optionctor2^
      """`ARRAY(tree, ...)` for Array"""                                      ! arrayctor1^
      """`SEQ(tree, ...)` for Seq"""                                          ! seqctor1^
      """`VECTOR(tree, ...)` for Vector"""                                    ! vectorctor1^
      """`MAKE_MAP(key ANY_-> value, ...)` for Map"""                         ! mapctor1^
                                                                              p^
  "Built-in type constructors are written as"                                 ^
      """`TYPE_LIST(typ)` for List"""                                         ! listtype1^
                                                                              end
  
  import treehugger.forest._
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
    (REF("foo") FOREACH LAMBDA(PARAM("x")) ==> BLOCK(
      REF("x") APPLY())) must print_as(
      "foo foreach { x =>",
      "  x()",
      "}"
    )
  
  def traversable2 =
    ((REF("foo") MAP LAMBDA(PARAM("x")) ==> BLOCK(
      REF("x") INT_+ LIT(1))) must print_as(
      "foo map { x =>",
      "  x + 1",
      "}"
    )) and
    ((REF("foo") MAP (WILDCARD INT_+ LIT(1))) must print_as(
      "foo.map(_ + 1)"
    ))

  def traversable3 =
    (REF("foo") FILTER LAMBDA(PARAM("x")) ==> BLOCK(
      REF("x"))) must print_as(
      "foo filter { x =>",
      "  x",
      "}"
    )

  def traversable4 =
    (REF("foo") FLATMAP LAMBDA(PARAM("x")) ==> BLOCK(
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
  
  def traversable6 =
    ((LIT(0) SEQ_/: REF("foo")) APPLY (WILDCARD INT_+ WILDCARD)) must print_as(
      "(0 /: foo)(_ + _)"
    )

  def traversable7 =
    ((REF("foo") SEQ_:\ LIT(0)) APPLY (WILDCARD INT_+ WILDCARD)) must print_as(
      "(foo :\\ 0)(_ + _)"
    )  

  def listctor1 = LIST(LIT(0)) must print_as("List(0)")

  def listctor2 = NIL must print_as("Nil")
  
  def listctor3 = (REF("x") UNLIST_:: NIL) must print_as("x :: Nil")
  
  def optionctor1 = SOME(LIT(0)) must print_as("Some(0)")

  def optionctor2 = NONE must print_as("None")

  def arrayctor1 = ARRAY(LIT(0)) must print_as("Array(0)")

  def seqctor1 = SEQ(LIT(0)) must print_as("Seq(0)")

  def vectorctor1 = VECTOR(LIT(0)) must print_as("Vector(0)")

  def mapctor1 = MAKE_MAP(LIT(0) ANY_-> LIT(1)) must print_as("Map(0 -> 1)")

  def listtype1 =
    VAL("x", TYPE_LIST(IntClass)).tree must print_as("val x: List[Int]")
}
