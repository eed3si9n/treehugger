import org.specs2._

class DSL_1BasicDeclSpec extends DSLSpec { def is = sequential                ^
  "This is a specification to check Treehugger DSL"                           ^
                                                                              p^
  "Literals are written as"                                                   ^
    """`LIT("Hello")` for Strings,"""                                         ! literal1^
    """`LIT(1)` for Ints, `LIT(1.23)` for Doubles."""                         ! literal2^
    """The predefined constants are `TRUE`, `FALSE`, `NULL`, and `UNIT`"""    ! literal3^
                                                                              p^
  "Comments are written as"                                                   ^
    """`tree withComment("a", ...)` where `tree` is an arbitrary tree."""     ! comment1^
                                                                              p^
  "Value declarations are written as"                                         ^
    """`VAL(sym, typ).empty` where `sym` is a symbol created from `RootClass` or another symbol,
and `typ` is a type of the value. This also be written as `VAL(sym, typ)` where `Tree` is expected.
By implicit conversion `empty` call is automatically injected .
A value can also be declared without using a symbol like `VAL("bar", typ)`.""" ! value1^
                                                                              end^
  "Constant value definitions are written as"                                 ^
    """`VAL(sym|"bar", [typ]) := rhs` where `rhs` is a tree such as a literal.
Notation `sym|"bar"` denotes that `VAL` takes either a symbol or a String as the first argument.
Also, `[typ]` denotes that the type is optional."""                           ! value2^
                                                                              end^
  "Lazy value definitions are written as"                                     ^
    """`LAZYVAL(sym|"bar", [typ]) := rhs`."""                                 ! lazyvalue1^
                                                                              end^
  "Constant value definitions are written as"                                 ^
    """`VAL(sym|"bar", [typ]) withFlags(Flags.FINAL) := rhs`."""              ! constantvalue1^
                                                                              end^
  "Variable declarations are written as"                                      ^
    """`VAR(sym, typ)` or `VAR("bar", typ)`."""                               ! variable1^
                                                                              end^
  "Variable definitions are written as"                                       ^
    """`VAR(sym|"bar", [typ]) := rhs`."""                                     ! variable2^
    """`VAR(sym, typ) := WILDCARD` intoduces mutable field initialized to
the default value of the type (for example `0` for Int)."""                   ! variable3^
                                                                              p^
  "Type declarations are written as"                                          ^
    """`TYPE(sym|"T") LOWER(lo)` where `lo` is a lower bound Type,"""         ! type1^
    """`TYPE(sym|"T") UPPER(hi)` where `hi` is a uppper bound Type,"""        ! type2^
    """`TYPE(sym|"T") LOWER(lo) UPPER(hi)` specifying both lower and upper bounds,""" ! type3^
                                                                              end^
  "Type definitions are written as"                                           ^
    """`TYPE(sym|"T") := typ` or"""                                           ! type4^
    """`TYPE(sym|"T") withTypeParams(TYPE(typ1)) := typ2`."""                 ! type5^    
                                                                              p^
  "Function declarations are written as"                                      ^
     """`DEF(sym|"get", typ)` where `sym` is the name of the function
and `typ` is the result type."""                                              ! function1^
     """Parameter lists may be added to the declaration as
`DEF(sym|"put", typ) withParams(PARAM("x", typ1)), ...`."""                   ! function2^
     """Type parameter lists may be added as
`DEF(sym|"get", typ) withTypeParams(TYPE(typ1)), ...`."""                     ! function3^
                                                                              end^
  "View bound type parameters are written as"                                 ^
    """`DEF(sym|"get", typ) withTypeParams(TYPE(typ) VIEWBOUNDS(trg))`
where `trg` is a target Type,"""                                              ! bounds1^
                                                                              end^
  "Context bound type parameters are written as"                                 ^                                                                            
    """`DEF(sym|"get", typ) withTypeParams(TYPE(typ) CONTEXTBOUNDS(tycon))`
where `tycon` is the type constructor Type."""                                ! bounds2^
                                                                              end^
  "Function definitions are written as"                                       ^
     """`DEF(sym|"get", typ) := rhs`."""                                      ! function4^
     """The result type `typ` may be omitted as
`DEF(sym|"get") := rhs`."""                                                   ! function5^
                                                                              end^
  "Parameter lists"                                                           ^
     """with default arguments are written as
`withParams(PARAM(sym|"x", typ) := arg)`."""                                  ! param1^
     """By-name parameters are written as
`withParams(PARAM(sym|"x", BYNAME(typ)))`"""                                  ! param2^
     """Repeated parameters are written as
`withParams(PARAM(sym|"x", STAR(typ)))`."""                                   ! param3^
                                                                              end^
  "Procedure declarations are written as"                                     ^
      """`DEF(sym|"write")` by omitting the result type of the function."""   ! procedure1^
                                                                              end^
  "Procedure definitions are written as"                                      ^
      """`DEF(sym|"write") := BLOCK(stat, ...)` where `stat` is a tree."""    ! procedure2^
                                                                              p^
  "Import clauses are written as"                                             ^
      """`IMPORT(sym)` or `IMPORT("scala.collection.mutable")`."""            ! import1^
      """`IMPORT(sym, "Map", ...)` and `IMPORT("scala.collection.mutable", "Map", ...)`
limit them to some members."""                                                ! import2^
      """Using `RENAME("x") ==> "y"`, a member can be renamed as
`IMPORT(sym, RENAME("Map") ==> "MutableMap")` or be suppressed."""            ! import3^
                                                                              end
  
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._
        
  def literal1 =
    LIT("Hello") must print_as("\"Hello\"")
  
  def literal2 =
    (LIT(1)    must print_as("1")) and
    (LIT(1.23) must print_as("1.23"))
  
  def literal3 =
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
  
  def value1 = {
    // They convert to trees implicitly
    val tree1: Tree = VAL(sym.foo, IntClass)
    val tree2: Tree = VAL("bar", listType(StringClass))
    
    (tree1 must print_as("val foo: Int")) and
    (tree2 must print_as("val bar: List[String]"))
  }
  
  def value2 =
    ((VAL(sym.foo, IntClass) := LIT(3)) must print_as("val foo: Int = 3")) and
    ((VAL("bar") := FALSE) must print_as("val bar = false"))
  
  def lazyvalue1 =
    ((LAZYVAL(sym.foo, IntClass) := LIT(3)) must print_as("lazy val foo: Int = 3")) and
    ((LAZYVAL("bar") := FALSE) must print_as("lazy val bar = false"))
      
  def constantvalue1 =
    ((VAL(sym.foo, IntClass) withFlags(Flags.FINAL) := LIT(3)) must print_as("final val foo: Int = 3")) and
    ((VAL("bar") withFlags(Flags.FINAL) := FALSE) must print_as("final val bar = false"))
  
  def variable1 = {
    // They convert to trees implicitly
    val tree1: Tree = VAR(sym.foo, IntClass)
    val tree2: Tree = VAR("bar", listType(StringClass))
    
    (tree1 must print_as("var foo: Int")) and
    (tree2 must print_as("var bar: List[String]"))
  }
  
  def variable2 =
    ((VAR(sym.foo, IntClass) := LIT(3)) must print_as("var foo: Int = 3")) and
    ((VAR("bar") := FALSE) must print_as("var bar = false"))
  
  // _ initializes var to 0 
  def variable3 = ((VAR(sym.foo, IntClass) := WILDCARD) must print_as("var foo: Int = _"))
  
  def type1 = (TYPE("T") LOWER(IntClass): Tree) must print_as("type T >: Int")
  
  def type2 = {
    val ComparableTClass = appliedType(ComparableClass.typeConstructor, List(sym.T)) 
    val X = RootClass.newTypeParameter("X")
    val CovX = RootClass.newTypeParameter("X") setFlag(Flags.COVARIANT)
    
    ((TYPE("T") UPPER(ComparableTClass): Tree) must print_as("type T <: Comparable[T]")) and
    ((TYPE("MyCollection") withTypeParams(TYPE(CovX)) UPPER(iterableType(X)): Tree) must print_as("type MyCollection[X] <: Iterable[X]"))
  }
  
  def type3 = (TYPE("T") LOWER(IntClass) UPPER(sym.Addressable): Tree) must print_as("type T >: Int <: Addressable")
  
  def type4 = (TYPE("IntList") := listType(IntClass)) must print_as("type IntList = List[Int]")
  
  def type5 = (TYPE("Two") withTypeParams(TYPE(sym.A)) := tupleType(sym.A, sym.A)) must print_as("type Two[A] = (A, A)")
  
  def function1 = {
    // This converts to a tree implicitly
    val tree: Tree = DEF("get", IntClass)
    tree must print_as("def get: Int")
  }
  
  def function2 = {
    // This converts to a tree implicitly
    val tree: Tree = DEF("put", UnitClass) withParams(PARAM("x", IntClass))
    tree must print_as("def put(x: Int): Unit")
  }
  
  def function3 = {
    // This converts to a tree implicitly
    val tree: Tree = DEF("put", UnitClass) withTypeParams(TYPE(sym.T)) withParams(PARAM("x", sym.T))
    tree must print_as("def put[T](x: T): Unit")
  }

  def bounds1 = {
    val tree: Tree = DEF("put", UnitClass).
      withTypeParams(TYPE(sym.A) VIEWBOUNDS(orderedType(sym.B))).
      withParams(PARAM("x", sym.B))
    
    tree must print_as("def put[A <% Ordered[B]](x: B): Unit")
  }
    
  def bounds2 = {
    val tree: Tree = DEF("put", UnitClass).
      withTypeParams(TYPE(sym.A) CONTEXTBOUNDS(FullManifestClass)).
      withParams(PARAM("x", sym.A))
    
    tree must print_as("def put[A : Manifest](x: A): Unit")
  }  
  
  def function4 = (DEF("get", IntClass) := LIT(0)) must print_as("def get: Int = 0")
  
  def function5 = (DEF("get") := LIT(0)) must print_as("def get = 0")
  
  def param1 = (DEF("put", UnitClass) withParams(PARAM("x", IntClass) := LIT(0)) := UNIT) must print_as(
    "def put(x: Int = 0): Unit = ()")
  
  def param2 = {
    // This converts to a tree implicitly
    val tree: Tree = DEF("whileLoop", UnitClass).
      withParams(PARAM("cond", BYNAME(BooleanClass))).
      withParams(PARAM("stat", BYNAME(UnitClass)))
    tree must print_as("def whileLoop(cond: => Boolean)(stat: => Unit): Unit")
  }
  
  def param3 = {
    // This converts to a tree implicitly
    val tree: Tree = DEF("sum", IntClass) withParams(PARAM("args", STAR(IntClass)))
    tree must print_as("def sum(args: Int*): Int")
  }
  
  def procedure1 = {
    val tree: Tree = DEF("write") withParams(PARAM("str", StringClass))
    tree must print_as("def write(str: String)")
  }
  
  def procedure2 =
    (DEF("write") withParams(PARAM("str", StringClass)) := BLOCK(
      UNIT
    )) must print_as(
      """def write(str: String) {""",
      """  ()""",
      """}"""
    )
  
  def import1 =
    (IMPORT(MutablePackage)             must print_as("import scala.collection.mutable")) and
    (IMPORT("scala.collection.mutable") must print_as("import scala.collection.mutable"))
  
  def import2 =
    (IMPORT(MutablePackage, "Map", "Set")      must print_as("import scala.collection.mutable.{Map, Set}")) and
    (IMPORT("scala.collection.mutable", "Map") must print_as("import scala.collection.mutable.Map"))
  
  def import3 =
    IMPORT(MutablePackage, RENAME("Map") ==> "MutableMap") must print_as("import scala.collection.mutable.{Map => MutableMap}")
}
