import org.specs2._

class DSL_1BasicDeclSpec extends DSLSpec { def is = sequential                ^
  "This is a specification to check Treehugger DSL"                           ^
                                                                              p^
  "Value declarations are written as"                                         ^
    """`VAL(sym, typ).tree` where `sym` is a symbol created from `RootClass` or another symbol,
and `typ` is a type of the value. This also be written as `VAL(sym, typ)` where `Tree` is expected.
By implicit conversion `tree` call is automatically injected .
A value can also be declared without using a symbol like `VAL("bar", typ)`.""" ! value1^
                                                                              end^
  "Constant value definitions are written as"                                 ^
    """`VAL(sym|"bar", [typ|"Int"]) := rhs` where `rhs` is a tree such as a literal.
Notation `sym|"bar"` denotes that `VAL` takes either a symbol or a String as the first argument.
Also, `[typ|"Int"]` denotes that the type is optional."""                     ! value2^
                                                                              end^
  "Lazy value definitions are written as"                                     ^
    """`LAZYVAL(sym|"bar", [typ|"Int"]) := rhs`."""                           ! lazyvalue1^
                                                                              end^
  "Constant value definitions are written as"                                 ^
    """`VAL(sym|"bar", [typ|"Int"]) withFlags(Flags.FINAL) := rhs`."""        ! constantvalue1^
                                                                              end^
  "Variable declarations are written as"                                      ^
    """`VAR(sym|"bar", typ|"Int")`."""                                        ! variable1^
                                                                              end^
  "Variable definitions are written as"                                       ^
    """`VAR(sym|"bar", [typ|"Int"]) := rhs`."""                               ! variable2^
    """`VAR(sym, typ|"Int") := WILDCARD` intoduces mutable field initialized to
the default value of the type (for example `0` for Int)."""                   ! variable3^
                                                                              p^
  "Type declarations are written as"                                          ^
    """`TYPEVAR(sym|"T") LOWER(lo)` where `lo` is a lower bound Type,"""      ! type1^
    """`TYPEVAR(sym|"T") UPPER(hi)` where `hi` is a uppper bound Type,"""     ! type2^
    """`TYPEVAR(sym|"T") LOWER(lo) UPPER(hi)` specifying both lower and upper bounds,""" ! type3^
                                                                              end^
  "Type definitions are written as"                                           ^
    """`TYPEVAR(sym|"T") := typ` or"""                                        ! type4^
    """`TYPEVAR(sym|"T") withTypeParams(TYPEVAR(typ1)) := typ2`."""           ! type5^    
                                                                              p^
  "Variance annotions are written as"                                         ^
    """`withTypeParams(TYPEVAR(COVARIANT(sym|"T")))` or
`withTypeParams(TYPEVAR(CONTRAVARIANT(sym|"T")))`."""                         ! variance1^    
                                                                              p^
  "Function declarations are written as"                                      ^
     """`DEF(sym|"get", typ|"Int")` where `sym` is the name of the function
and `typ` is the result type."""                                              ! function1^
     """Parameter lists may be added to the declaration as
`DEF(sym|"put", typ|"Int") withParams(PARAM("x", typ1)), ...`."""             ! function2^
     """Type parameter lists may be added as
`DEF(sym|"get", typ|"Int") withTypeParams(TYPEVAR(typ1)), ...`."""            ! function3^
                                                                              end^
  "View bound type parameters are written as"                                 ^
    """`DEF(sym|"get", typ|"Int") withTypeParams(TYPEVAR(typ) VIEWBOUNDS(trg))`
where `trg` is a target Type,"""                                              ! bounds1^
                                                                              end^
  "Context bound type parameters are written as"                                 ^                                                                            
    """`DEF(sym|"get", typ|"Int") withTypeParams(TYPEVAR(typ) CONTEXTBOUNDS(tycon))`
where `tycon` is the type constructor Type."""                                ! bounds2^
                                                                              end^
  "Function definitions are written as"                                       ^
     """`DEF(sym|"get", typ|"") := rhs`."""                                   ! function4^
     """The result type `typ` may be omitted as
`DEF(sym|"get") := rhs`."""                                                   ! function5^
                                                                              end^
  "Parameter lists"                                                           ^
     """with default arguments are written as
`withParams(PARAM(sym|"x", typ|"Int") := arg)`."""                            ! param1^
     """By-name parameters are written as
`withParams(PARAM(sym|"x", TYPE_BYNAME(typ|"C")))`"""                         ! param2^
     """Repeated parameters are written as
`withParams(PARAM(sym|"x", TYPE_*(typ|"C")))`."""                             ! param3^
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
  
  def value1 = {
    // They convert to trees implicitly
    val tree1: Tree = VAL(sym.foo, IntClass)
    val tree2: Tree = VAL("bar", TYPE_LIST(StringClass))
    
    (tree1 must print_as("val foo: Int")) and
    (tree2 must print_as("val bar: List[String]"))
  }
  
  def value2 =
    ((VAL(sym.foo, "Int") := LIT(3)) must print_as("val foo: Int = 3")) and
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
    val tree2: Tree = VAR("bar", TYPE_LIST(StringClass))
    
    (tree1 must print_as("var foo: Int")) and
    (tree2 must print_as("var bar: List[String]"))
  }
  
  def variable2 =
    ((VAR(sym.foo, IntClass) := LIT(3)) must print_as("var foo: Int = 3")) and
    ((VAR("bar") := FALSE) must print_as("var bar = false"))
  
  // _ initializes var to 0 
  def variable3 = ((VAR(sym.foo, IntClass) := WILDCARD) must print_as("var foo: Int = _"))
  
  def type1 =
    ((TYPEVAR("T"): Tree) must print_as("type T")) and
    ((TYPEVAR("T") LOWER(IntClass): Tree) must print_as("type T >: Int"))
  
  def type2 = {
    val ComparableTClass = appliedType(ComparableClass.typeConstructor, sym.T) 
    val X = RootClass.newAliasType("X")

    ((TYPEVAR("T") UPPER(ComparableTClass): Tree) must print_as("type T <: Comparable[T]")) and
    ((TYPEVAR("MyCollection") withTypeParams(TYPEVAR(COVARIANT(X))) UPPER(iterableType(X)): Tree) must print_as("type MyCollection[+X] <: Iterable[X]"))
  }
  
  def type3 = (TYPEVAR("T") LOWER(IntClass) UPPER(sym.Addressable): Tree) must print_as("type T >: Int <: Addressable")
  
  def type4 = (TYPEVAR("IntList") := listType(IntClass)) must print_as("type IntList = List[Int]")
  
  def type5 = (TYPEVAR("Two") withTypeParams(TYPEVAR(sym.A)) := TYPE_TUPLE(sym.A, sym.A)) must print_as("type Two[A] = (A, A)")
  
  def variance1 = {
    val A = RootClass.newTypeParameter("A")
    ((TYPEVAR("M") withTypeParams(TYPEVAR(COVARIANT(A))): Tree) must print_as("type M[+A]")) and
    ((TYPEVAR("M") withTypeParams(TYPEVAR(CONTRAVARIANT(A))): Tree) must print_as("type M[-A]")) 
  }

  def function1 =
    (DEF("get", IntClass): Tree) must print_as("def get: Int")
    
  def function2 =
    ((DEF("put", UnitClass) withParams(PARAM("x", IntClass)): Tree) must print_as("def put(x: Int): Unit")) and
    ((DEF(sym.run, UnitClass) withParams(PARAM("x", IntClass) := LIT(0)): Tree) must print_as("def run(x: Int = 0): Unit"))

  def function3 =
    (DEF("compare", BooleanClass)
      withTypeParams(TYPEVAR(sym.T))
      withParams(PARAM("a", sym.T) := LIT(0))
      withParams(PARAM("b", sym.T) := LIT(0)): Tree) must print_as(
        "def compare[T](a: T = 0)(b: T = 0): Boolean")

  def bounds1 = {
    val tree: Tree = DEF("maxList", "T").
      withTypeParams(TYPEVAR("T") VIEWBOUNDS TYPE_ORDERED("T")).
      withParams(PARAM("elements", TYPE_LIST("T")))
    
    tree must print_as("def maxList[T <% Ordered[T]](elements: List[T]): T")
  }
    
  def bounds2 = {
    val tree: Tree = DEF("put", UnitClass).
      withTypeParams(TYPEVAR(sym.A) CONTEXTBOUNDS FullManifestClass).
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
      withParams(PARAM("cond", TYPE_BYNAME(BooleanClass))).
      withParams(PARAM("stat", TYPE_BYNAME(UnitClass)))
    tree must print_as("def whileLoop(cond: => Boolean)(stat: => Unit): Unit")
  }
  
  def param3 = {
    // This converts to a tree implicitly
    val tree: Tree = DEF("sum", IntClass) withParams(PARAM("args", TYPE_*(IntClass)))
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
