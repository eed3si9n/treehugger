import org.specs2._

import treehugger._

class TreePrinterSpec extends Specification { def is =
  "This is a specification to check a TreePrinter"                            ^
                                                                              p^
  "The tree printer should"                                                   ^
    """print println("Hello, world!")"""                                      ! e1^
    """print def hello()"""                                                   ! e2^
    """print val greetStrings = new Array[String](3)"""                       ! e3^
    """object ChecksumAccumulator"""                                          ! e4^
    """abstract class IntQueue"""                                             ! e5^
    """package scala"""                                                       ! e6^
                                                                              end
  
  lazy val universe = new treehugger.Universe
  import universe._
  import definitions._
  import CODE._
  import Flags._
  
  def e1 = {  
    val tree = sym.println APPLY LIT("Hello, world!")
    val s = treeToString(tree); println(s)
    
    s must_== """println("Hello, world!")"""
  }
  
  def e2 = {
    val tree = DEF("hello", UnitClass.toType) :=
      BLOCK(sym.println APPLY LIT("Hello, world!"))
    val s = treeToString(tree); println(s)
    
    s.lines.toList must contain(
      """def hello() {""",
      """  println("Hello, world!")""",
      """  ()""",
      """}"""
    ).inOrder
  }
  
  // p. 38
  def e3 = {
    val greetStrings = RootClass.newValue("greetStrings")
    def assignGreetStrings(index: Int, value: String): Tree =
      greetStrings APPLY LIT(index) := LIT(value)
    
    val trees = (VAL(greetStrings) := NEW(arrayType(StringClass.toType), LIT(3))) ::
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
    
    val trees = (MODULEDEF(ChecksumAccumulator) BODY (
        VAL(cache) withFlags(PRIVATE) := TypeTree(mutableMapType(StringClass.toType, IntClass.toType)) APPLY (),
        DEF("calculate", IntClass.toType) withParams(VAL(s, StringClass.toType).empty) :=
          (IF(REF(cache) DOT "contains" APPLY REF(s)) THEN REF(cache).APPLY(REF(s)) 
          ELSE BLOCK(
            VAL("acc") := NEW(ChecksumAccumulator.toType),
            FOR(VALFROM("c") := REF(s)) DO
              (REF("acc") DOT "add" APPLY (REF("c") DOT "toByte")),
            VAL("cs") := REF("acc") DOT "checksum" APPLY (),
            REF(cache) INFIX ("+=", REF(s) INFIX ("->", REF("cs"))),
            REF("cs")
          ))
      )) ::
      Nil
    
    val out = treesToString(trees); println(out)
    out.lines.toList must contain(
      """object ChecksumAccumulator {""",
      """  private val cache = scala.collection.mutable.Map[String,Int]()""",
      """  def calculate(s: String): Int =""",
      """    if (this.cache.contains(s)) this.cache(s)""",
      """    else {""",
      """      val acc = new ChecksumAccumulator()""",
      """      for (c <- s)""",
      """        acc.add(c.toByte)""",
      """      val cs = acc.checksum()""",
      """      this.cache += (s -> cs)""",
      """      cs""",
      """    }""",
      """}"""
    ).inOrder
  }
  
  // p. 227
  def e5 = {
    val IntQueue: ClassSymbol = RootClass.newClass("IntQueue".toTypeName)
    val BasicIntQueue: ClassSymbol = RootClass.newClass("BasicIntQueue".toTypeName)
    val buf: TermSymbol = BasicIntQueue.newValue("buf")
    def arrayBufferType(arg: Type)  = appliedType(ArrayBufferClass.typeConstructor, List(arg))
    
    val trees =
      (CLASSDEF(IntQueue) withFlags(ABSTRACT) BODY (
        DEF("get", IntClass.toType).empty,
        DEF("put", UnitClass.toType) withParams(VAL("x", IntClass.toType).empty) empty
      )) ::
      (CLASSDEF(BasicIntQueue) withParents(TypeTree(IntQueue.toType)) BODY (
        VAL(buf) withFlags(PRIVATE) := NEW(arrayBufferType(IntClass.toType)),
        DEF("get", IntClass.toType) := (REF(buf) DOT "remove" APPLY()),
        DEF("put", UnitClass.toType) withParams(VAL("x", IntClass.toType).empty) := BLOCK(
          REF(buf) INFIX ("+=", REF("x"))
          )
      )) ::
      Nil
    
    val out = treesToString(trees); println(out)
    out.lines.toList must contain(
      """abstract class IntQueue {""",
      """  def get(): Int""",
      """  def put(x: Int): Unit""",
      """}""",
      """class BasicIntQueue extends IntQueue {""",
      """  private val buf = new scala.collection.mutable.ArrayBuffer[Int]()""",
      """  def get(): Int =""",
      """    this.buf.remove()""",
      """  def put(x: Int) {""",
      """    this.buf += x""",
      """    ()""",
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
    
    val trees =
      (PACKAGEDEF(ScalaPackageClass) := (MODULEDEF(PredefModule) := BLOCK(
        (CLASSDEF(ArrowAssocClass) withTypeParams(TypeDef(A)) withParams(VAL("x", A.toType).empty) BODY (
          DEF(arrow.name, tuple2AB) withTypeParams(TypeDef(B)) withParams(VAL("y", B.toType).empty) :=
            makeTupleTerm(REF("x") :: REF("y") :: Nil)
        )),
        
        DEF("put", UnitClass.toType) withParams(VAL("x", IntClass.toType).empty) empty
      ))) ::
      Nil
    
    val out = treesToString(trees); println(out)
    out.lines.toList must contain(
      """package scala {""",
      """  object Predef {""",
      """    class ArrowAssoc[A](x: A) {""",
      """      def ->[B](y: B): (A, B) =""",
      """        Tuple2(x, y)""",
      """    }""",
      """    def put(x: Int): Unit""",
      """  }""",
      """}"""
    ).inOrder  
  }
  
  object sym {
    val println = ScalaPackageClass.newMethod("println")
    val print = ScalaPackageClass.newMethod("print")
    val to = ScalaPackageClass.newMethod("to")
  }
}
