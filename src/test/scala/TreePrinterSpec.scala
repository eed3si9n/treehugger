import org.specs2._

class TreePrinterSpec extends Specification { def is = sequential             ^
  "This is a specification to check a TreePrinter"                            ^
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
    """`VAL(sym, typ)` or `VAL("bar", typ)` where `sym` is a symbol created
from `RootClass` or another symbol, and `typ` is a type of the value."""      ! value1^
                                                                              end^
  "Value definitions are written as"                                          ^
    """`VAL(sym|"bar", [typ]) := rhs` where `rhs` is a tree such as a literal.
Notation `sym|"bar"` denotes that `VAL` takes either a symbol or a String as the first argument.
Also, `[typ]` denotes that the type is optional"""                            ! value2^
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
    """`VAR(sym, typ) := UNDERSCORE` intoduces mutable field initialized to
the default value of the type (for example `0` for Int)."""                   ! variable3^
                                                                              p^                                                                            
  "Type declarations are written as"                                          ^
    """`TYPE(sym|"T") LOWER(typ)`,"""                                         ! type1^
    """`TYPE(sym|"T") HIGHER(typ)`, or"""                                     ! type2^
    """`TYPE(sym|"T") TYPEBOUNDS(lo, hi)`."""                                 ! type3^
                                                                              end^                                                                            
  "Type definitions are written as"                                           ^
    """`TYPE(sym|"T") := typ` or"""                                           ! type4^
    """`TYPE(sym|"T") withTypeParams(TYPE(typ1)) := typ2`."""                 ! type5^
                                                                              p^                                                                            
  "Function declarations are written as"                                      ^
     """`DEF(sym|"get", typ)` where `sym` is the name of the function
and `typ` is the result type."""                                              ! function1^
     """Parameter lists may be added to the declaration as
`DEF(sym|"put", typ) withParams(VAL("x", typ1)), ...`."""                     ! function2^
     """Type parameter lists may be added as
`DEF(sym|"get", typ) withTypeParams(TYPE(typ1)), ...`."""                     ! function3^
                                                                              end^
  "Function definitions are written as"                                       ^
     """`DEF(sym|"get", typ) := rhs`."""                                      ! function4^
     """The result type `typ` may be omitted as
`DEF(sym|"get") := rhs`."""                                                   ! function5^
                                                                              end^
  "Parameters with default arguments are written as"                          ^
     """`withParams(VAL(sym|"x", typ) := arg)`."""                            ! param1^
                                                                              end^
  "By-name parameters are written as"                                         ^
     """`withParams(VAL(sym|"x", BYNAME(typ)))`"""                            ! param2^
                                                                              end^
  "Repeated parameters are written as"                                        ^
     """`withParams(VAL(sym|"x", STAR(typ)))`."""                             ! param3^
                                                                              end^
  "Procedure declarations are written as"                                     ^
      """`DEF(sym|"write")` by omitting the result type of the function."""   ! procedure1^
                                                                              end^
  "Procedure definitions are written as"                                      ^
      """`DEF(sym|"write") := BLOCK(stat, ...)` where `stat` is a tree."""    ! procedure2^
                                                                              p^
  "The tree printer should"                                                   ^
    """print println("Hello, world!")"""                                      ! e1^
    """print def hello"""                                                     ! e2^
    """print val greetStrings = new Array[String](3)"""                       ! e3^
    """print object ChecksumAccumulator"""                                    ! e4^
    """print abstract class IntQueue"""                                       ! e5^
    """print package scala"""                                                 ! e6^
    """print case List(x) => x"""                                             ! e7^
    """print case class [T <% List[T]]Address()"""                            ! e8^
    """print new Addressable {}"""                                            ! e9^
                                                                              end
  
  import treehugger._
  import definitions._
  import treehuggerDSL._
  import treehugger.Flags.{PRIVATE, ABSTRACT, IMPLICIT, OVERRIDE}
      
  def literal1 =
    LIT("Hello") must print_as(""""Hello"""")
  
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
  def variable3 = ((VAR(sym.foo, IntClass) := UNDERSCORE) must print_as("var foo: Int = _"))
  
  def type1 = (TYPE("T") LOWER(IntClass)) must print_as("type T >: Int")
  
  def type2 = {
    val ComparableTClass = appliedType(ComparableClass.typeConstructor, List(sym.T)) 
    val X = RootClass.newTypeParameter("X".toTypeName)
    val CovX = RootClass.newTypeParameter("X".toTypeName) setFlag(Flags.COVARIANT)
    
    (TYPE("T") UPPER(ComparableTClass) must print_as("type T <: Comparable[T]")) and
    (TYPE("MyCollection") withTypeParams(TYPE(CovX)) UPPER(iterableType(X)) must print_as("type MyCollection[X] <: Iterable[X]"))
  }
  
  def type3 = (TYPE("T") TYPEBOUNDS(IntClass, sym.Addressable)) must print_as("type T >: Int <: Addressable")
  
  def type4 = (TYPE("IntList") := listType(IntClass)) must print_as("type IntList = List[Int]")
  
  def type5 = (TYPE("Two") withTypeParams(TYPE(sym.A)) := tupleType(sym.A, sym.A)) must print_as("type Two[A] = (A, A)")
  
  def function1 = {
    // This converts to a tree implicitly
    val tree: Tree = DEF("get", IntClass)
    tree must print_as("def get: Int")
  }
  
  def function2 = {
    // This converts to a tree implicitly
    val tree: Tree = DEF("put", UnitClass) withParams(VAL("x", IntClass))
    tree must print_as("def put(x: Int): Unit")
  }
  
  def function3 = {
    // This converts to a tree implicitly
    val tree: Tree = DEF("put", UnitClass) withTypeParams(TYPE(sym.T)) withParams(VAL("x", sym.T))
    tree must print_as("def put[T](x: T): Unit")
  }
  
  def function4 = (DEF("get", IntClass) := LIT(0)) must print_as(
    "def get: Int =",
    "  0")
  
  def function5 = (DEF("get") := LIT(0)) must print_as(
    "def get =",
    "  0")
  
  def param1 = (DEF("put", UnitClass) withParams(VAL("x", IntClass) := LIT(0)) := UNIT) must print_as(
    "def put(x: Int = 0): Unit =",
    "  ()")
  
  def param2 = {
    // This converts to a tree implicitly
    val tree: Tree = DEF("whileLoop", UnitClass).
      withParams(VAL("cond", BYNAME(BooleanClass))).
      withParams(VAL("stat", BYNAME(UnitClass)))
    tree must print_as("def whileLoop(cond: => Boolean)(stat: => Unit): Unit")
  }
  
  def param3 = {
    // This converts to a tree implicitly
    val tree: Tree = DEF("sum", IntClass) withParams(VAL("args", STAR(IntClass)))
    tree must print_as("def sum(args: Int*): Int")
  }
  
  def procedure1 = {
    val tree: Tree = DEF("write") withParams(VAL("str", StringClass))
    tree must print_as("def write(str: String)")
  }
  
  def procedure2 =
    (DEF("write") withParams(VAL("str", StringClass)) := BLOCK(
      UNIT
    )) must print_as(
      """def write(str: String) {""",
      """  ()""",
      """}"""
    )
  
  def e1 = {  
    val tree: Tree = sym.println APPLY LIT("Hello, world!"); println(tree)
    val s = treeToString(tree); println(s)
    
    s must_== """println("Hello, world!")"""
  }
  
  def e2 = {
    val tree = DEF("hello") := BLOCK(
      sym.println APPLY LIT("Hello, world!"))
    val s = treeToString(tree); println(s)
    
    s.lines.toList must contain(
      """def hello {""",
      """  println("Hello, world!")""",
      """}"""
    ).inOrder
  }
  
  // p. 38
  def e3 = {
    val greetStrings = RootClass.newValue("greetStrings")
    def assignGreetStrings(index: Int, value: String): Tree =
      greetStrings APPLY LIT(index) := LIT(value)
    
    val trees = (VAL(greetStrings) := NEW(arrayType(StringClass), LIT(3))) ::
      assignGreetStrings(0, "Hello") ::
      assignGreetStrings(1, ", ") ::
      assignGreetStrings(2, "world!\n") ::
      (FOR(VALFROM("i") := LIT(0) INFIX (sym.to, LIT(2))) DO
        (sym.print APPLY (greetStrings APPLY REF("i"))) ) ::
      Nil
    
    val s = treesToString(trees); println(s)
    
    s.lines.toList must contain(
      """val greetStrings = new Array[String](3)""",
      """greetStrings(0) = "Hello"""",
      """greetStrings(1) = ", """",
      """greetStrings(2) = "world!\n"""",
      """for (i <- 0 to 2)""",
      """  print(greetStrings(i))"""
    ).inOrder
  }
  
  // p. 66
  def e4 = {
    val ChecksumAccumulator = RootClass.newClass("ChecksumAccumulator".toTypeName)
    val cache = ChecksumAccumulator.newValue("cache")
    val s = RootClass.newValue("s")
    
    val tree = (MODULEDEF(ChecksumAccumulator) := BLOCK(
        VAL(cache) withFlags(PRIVATE) := mutableMapType(StringClass, IntClass) APPLY (),
        DEF("calculate", IntClass) withParams(VAL(s, StringClass)) :=
          (IF(REF(cache) DOT "contains" APPLY REF(s)) THEN REF(cache).APPLY(REF(s)) 
          ELSE BLOCK(
            VAL("acc") := NEW(ChecksumAccumulator),
            FOR(VALFROM("c") := REF(s)) DO
              (REF("acc") DOT "add" APPLY (REF("c") DOT "toByte")),
            VAL("cs") := REF("acc") DOT "checksum" APPLY (),
            REF(cache) INFIX ("+=", REF(s) INFIX ("->", REF("cs"))),
            REF("cs")
          ))
      )) withComment("In file ChecksumAccumulator.scala")
    
    val out = treeToString(tree); println(out)
    out.lines.toList must contain(
      """// In file ChecksumAccumulator.scala""",
      """object ChecksumAccumulator {""",
      """  private val cache = scala.collection.mutable.Map[String,Int]()""",
      """  def calculate(s: String): Int =""",
      """    if (cache.contains(s)) cache(s)""",
      """    else {""",
      """      val acc = new ChecksumAccumulator""",
      """      for (c <- s)""",
      """        acc.add(c.toByte)""",
      """      val cs = acc.checksum()""",
      """      cache += (s -> cs)""",
      """      cs""",
      """    }""",
      """}"""
    ).inOrder
  }
  
  // p. 227
  def e5 = {
    val IntQueue: ClassSymbol = RootClass.newClass("IntQueue".toTypeName)
    val BasicIntQueue: ClassSymbol = RootClass.newClass("BasicIntQueue".toTypeName)
    val Doubling: ClassSymbol = RootClass.newClass("Doubling".toTypeName)
    val buf: TermSymbol = BasicIntQueue.newValue("buf")
    def arrayBufferType(arg: Type)  = appliedType(ArrayBufferClass.typeConstructor, List(arg))
    
    val trees =
      (CLASSDEF(IntQueue) withFlags(ABSTRACT) := BLOCK(
        DEF("get", IntClass),
        DEF("put") withParams(VAL("x", IntClass))
      )) ::
      (CLASSDEF(BasicIntQueue) withParents(IntQueue) := BLOCK(
        VAL(buf) withFlags(PRIVATE) := NEW(arrayBufferType(IntClass)),
        DEF("get", IntClass) := (REF(buf) DOT "remove" APPLY()),
        DEF("put") withParams(VAL("x", IntClass)) := BLOCK(
          REF(buf) INFIX ("+=", REF("x"))
          )
      )) ::
      (TRAITDEF(Doubling) withParents(IntQueue) := BLOCK(
        DEF("put") withFlags(ABSTRACT, OVERRIDE) withParams(VAL("x", IntClass)) := BLOCK(
          SUPER DOT "put" APPLY (LIT(2) INFIX("*", REF("x")))
          )
      )) ::
      Nil
    
    val out = treesToString(trees); println(out)
    out.lines.toList must contain(
      """abstract class IntQueue {""",
      """  def get: Int""",
      """  def put(x: Int)""",
      """}""",
      """class BasicIntQueue extends IntQueue {""",
      """  private val buf = new scala.collection.mutable.ArrayBuffer[Int]""",
      """  def get: Int =""",
      """    buf.remove()""",
      """  def put(x: Int) {""",
      """    buf += x""",
      """  }""",
      """}""",
      """trait Doubling extends IntQueue {""",
      """  override abstract def put(x: Int) {""",
      """    super.put(2 * x)""",
      """  }""",
      """}"""
    ).inOrder  
  }
  
  // p. 453
  def e6 = {
    val A = ArrowAssocClass.newTypeParameter("A".toTypeName)
    val arrow = ArrowAssocClass.newMethod("->")
    val B = arrow.newTypeParameter("B".toTypeName)
    val tuple2AB = tupleType(A.toType :: B.toType :: Nil)
    val ArrowAssocA = appliedType(ArrowAssocClass, A.toType :: Nil)
    
    val tree =
      (MODULEDEF(PredefModule) := BLOCK(
        (CLASSDEF(ArrowAssocClass) withTypeParams(TYPE(A)) withParams(VAL("x", A)) := BLOCK(
          DEF(arrow.name, tuple2AB) withTypeParams(TYPE(B)) withParams(VAL("y", B)) :=
            makeTupleTerm(REF("x") :: REF("y") :: Nil)
        )),
        
        DEF("any2ArrowAssoc", ArrowAssocA)
            withFlags(IMPLICIT) withTypeParams(TYPE(A)) withParams(VAL("x", A)) :=
          NEW(ArrowAssocA, REF("x"))
      )) inPackage(ScalaPackageClass)
    
    val out = treeToString(tree); println(out)
    out.lines.toList must contain(
      """package scala""",
      """object Predef {""",
      """  class ArrowAssoc[A](x: A) {""",
      """    def ->[B](y: B): (A, B) =""",
      """      Tuple2(x, y)""",
      """  }""",
      """  implicit def any2ArrowAssoc[A](x: A): scala.Predef.ArrowAssoc[A] =""",
      """    new scala.Predef.ArrowAssoc[A](x)""",
      """}"""
    ).inOrder  
  }
  
  // p. 457
  def e7 = {
    val maxListUpBound = RootClass.newMethod("maxListUpBound")
    val T = maxListUpBound.newTypeParameter("T".toTypeName)
    
    val trees =
      (DEF(maxListUpBound.name, T)
          withTypeParams(TYPE(T) UPPER orderedType(T)) withParams(VAL("elements", listType(T))) :=
        REF("elements") MATCH(
          CASE(ListClass UNAPPLY()) ==> THROW(IllegalArgumentExceptionClass, "empty list!"),
          CASE(ListClass UNAPPLY(ID("x"))) ==> REF("x"),
          CASE(ID("x") INFIXUNAPPLY("::", ID("rest"))) ==> BLOCK(
            VAL("maxRest") := maxListUpBound APPLY(REF("rest")),
            IF(REF("x") INFIX (">", REF("maxRest"))) THEN REF("x")
            ELSE REF("maxRest") 
          )
        ))::
      Nil
    
    val out = treesToString(trees); println(out)
    out.lines.toList must contain(
      """def maxListUpBound[T <: Ordered[T]](elements: List[T]): T =""",
      """  elements match {""",
      """    case List() => throw new IllegalArgumentException("empty list!")""",
      """    case List(x) => x""",
      """    case x :: rest => {""",
      """      val maxRest = maxListUpBound(rest)""",
      """      if (x > maxRest) x""",
      """      else maxRest""",
      """    }""",
      """  }"""
    ).inOrder
  }
  
  def e8 = {
    val Address = RootClass.newClass("Address".toTypeName)
    val T = Address.newTypeParameter("T".toTypeName)
    val list = Address.newValue("list")
    
    val tree: Tree =
      (CASECLASSDEF(Address)
          withTypeParams(TYPE(T) VIEWBOUNDS listType(T))
          withParams(VAL("name", optionType(T)) := REF(NoneModule)) := BLOCK(
        DEF("stringOnly", Address) withParams(VAL("ev", tpEqualsType(T, StringClass)) withFlags(IMPLICIT)) :=
          Address APPLY(THIS(Address) DOT "name" MAP LAMBDA(VAL("nm", StringClass)) ==> BLOCK(
            VAL(list, listType(T)) := REF("nm"),
            (list MAP LAMBDA(VAL("x")) ==>
              (REF("x") INFIX(StringAdd_+, LIT("x")))) DOT "mkString" APPLY LIT(" ")
          )),
        DEF("star") withParams(VAL("n", STAR(IntClass))) :=
          Address TYPEAPPLY(StringClass) APPLY SOME(LIT("foo")).MAP(UNDERSCORE INFIX(StringAdd_+, LIT("x")))
      ))
                
    val out = treeToString(tree); println(out)
    out.lines.toList must contain(
      """case class Address[T <% List[T]](name: Option[T] = None) {""",
      """  def stringOnly(implicit ev: =:=[T,String]): Address =""",
      """    Address(this.name map { (nm: String) =>""",
      """      val list: List[T] = nm""",
      """      list.map((x) => x + "x").mkString(" ")""",
      """    })""",
      """  def star(n: Int*) =""",
      """    Address[String](Some("foo").map(_ + "x"))""",
      """}"""
    ).inOrder
  }
  
  def e9 = {
    val Addressable = RootClass.newClass("Addressable".toTypeName)
    val street = Addressable.newMethod("street")
    
    val tree: Tree =
      NEW(ANONDEF(Addressable) := BLOCK(
        VAL(street) := LIT("123 Drive")
      ))
    
    val out = treeToString(tree); println(out)
    out.lines.toList must contain(
      """new Addressable {""",
      """  val street = "123 Drive"""",
      """}"""
    ).inOrder
  }
  
  object sym {
    val println = ScalaPackageClass.newMethod("println")
    val print = ScalaPackageClass.newMethod("print")
    val to = ScalaPackageClass.newMethod("to")
    
    val foo = RootClass.newValue("foo")
    val Addressable = RootClass.newClass("Addressable".toTypeName)
    val A = RootClass.newTypeParameter("A".toTypeName)
    val T = RootClass.newTypeParameter("T".toTypeName)
  }
  
  def print_as(expected: String*): matcher.Matcher[Tree] =
    (actual: Tree) => (expected.toList match {
      case List(x) =>
        val s = treeToString(actual); println(s)
        (s == x, s + " doesn't equal " + x)
      case list    => 
        val s = treeToString(actual); println(s)
        (s.lines.toList == list, s.lines.toList + " doesn't equal " + list)
    })
}
