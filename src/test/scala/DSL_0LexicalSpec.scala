import org.specs2._

class DSL_0LexicalSpec extends DSLSpec { def is = sequential                  ^
  "This is a specification to check Treehugger DSL"                           ^
                                                                              p^
  "Literals are written as"                                                   ^
    """`LIT("Hello")` for Strings, `LIT('H')` for Char"""                     ! literal1^
    """`LIT(1)` for Ints, `LIT(1L)` for Longs, `LIT(1.23)` for Doubles,
`LIT(1.23F)` for Floats,"""                                                   ! literal2^
    """and LIT('Symbol) for symbols."""                                       ! literal3^
    """The predefined constants are `TRUE`, `FALSE`, `NULL`, and `UNIT`"""    ! literal4^
                                                                              p^
  "Comments are written as"                                                   ^
    """`tree withComment("a", ...)` where `tree` is an arbitrary tree."""     ! comment1^
                                                                              p^
  "Scaladoc style comments are written as"                                    ^
    """`tree withDoc("a")` or withDoc("a", DocTag.See(IntClass), ...).""" ! comment2^
                                                                              end
  
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  def literal1 =
    (LIT("Hello") must print_as("\"Hello\"")) and
    (LIT('H') must print_as("'H'"))
  
  def literal2 =
    (LIT(1)    must print_as("1")) and
    (LIT(1.23) must print_as("1.23")) and
    (LIT(1L)   must print_as("1L")) and
    (LIT(1.23F) must print_as("1.23F"))
  
  def literal3 =
    LIT('Symbol) must print_as("'Symbol")

  def literal4 =
    (TRUE  must print_as("true")) and
    (FALSE must print_as("false")) and
    (NULL  must print_as("null")) and
    (UNIT  must print_as("()"))
    
  def comment1 =
    (LIT(2) withComment("if you have to explain yourself", "in comments...")) must print_as(
      "// if you have to explain yourself",
      "// in comments...",
      "2"
    )

  def comment2 =
    ((DEF("x").tree withDoc("does something")) must print_as(
      "/** does something */",
      "def x"
    )) and
    ((DEF("x").tree withDoc("does \nsomething",
        DocTag.See(IntClass), DocTag.ToDo(IntClass, "foo"))) must print_as(
      "/**",
      " * does ",
      " * something",
      " * @see scala.Int",
      " * @todo scala.Int foo",
      " */",
      "def x"
    ))
}
