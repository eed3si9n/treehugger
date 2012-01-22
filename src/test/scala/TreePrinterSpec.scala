import org.specs2._

class TreePrinterSpec extends Specification { def is =
  "This is a specification to check a TreePrinter"                            ^
                                                                              p^
  "The tree printer should"                                                   ^
    """print println("Hello, world!")"""                                      ! e1^
    """print def hello"""                                                     ! e2^
    """print val greetStrings = new Array[String](3)"""                       ! e3^
    """object ChecksumAccumulator"""                                          ! e4^
    """abstract class IntQueue"""                                             ! e5^
    """package scala"""                                                       ! e6^
    """case List(x) => x"""                                                   ! e7^
    """case class [T <% List[T]]Address()"""                                  ! e8^
                                                                              end
  
  import treehugger._
  import definitions._
  import treehuggerDSL._
  import treehugger.Flags.{PRIVATE, ABSTRACT, IMPLICIT, OVERRIDE}
  
  def e1 = {  
    val tree: Tree = sym.println APPLY LIT("Hello, world!"); println(tree)
    val s = treeToString(tree); println(s)
    
    s must_== """println("Hello, world!")"""
  }
  
  def e2 = {
    val tree = DEF("hello", UnitClass) := BLOCK(
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
      """      val acc = new ChecksumAccumulator()""",
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
        DEF("put", UnitClass) withParams(VAL("x", IntClass))
      )) ::
      (CLASSDEF(BasicIntQueue) withParents(IntQueue) := BLOCK(
        VAL(buf) withFlags(PRIVATE) := NEW(arrayBufferType(IntClass)),
        DEF("get", IntClass) := (REF(buf) DOT "remove" APPLY()),
        DEF("put", UnitClass) withParams(VAL("x", IntClass)) := BLOCK(
          REF(buf) INFIX ("+=", REF("x"))
          )
      )) ::
      (TRAITDEF(Doubling) withParents(IntQueue) := BLOCK(
        DEF("put", UnitClass) withFlags(ABSTRACT, OVERRIDE) withParams(VAL("x", IntClass)) := BLOCK(
          SUPER DOT "put" APPLY (LIT(2) INFIX("*", REF("x")))
          )
      )) ::
      Nil
    
    val out = treesToString(trees); println(out)
    out.lines.toList must contain(
      """abstract class IntQueue {""",
      """  def get: Int""",
      """  def put(x: Int): Unit""",
      """}""",
      """class BasicIntQueue extends IntQueue {""",
      """  private val buf = new scala.collection.mutable.ArrayBuffer[Int]()""",
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
  
  object sym {
    val println = ScalaPackageClass.newMethod("println")
    val print = ScalaPackageClass.newMethod("print")
    val to = ScalaPackageClass.newMethod("to")
  }
}
