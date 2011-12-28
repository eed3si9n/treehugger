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
      """def hello(): Unit = {""",
      """  println("Hello, world!");""",
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
    
    val trees = IMPORT(ScalaPackageClass DOT "collection" DOT "mutable", "Map") ::
      (MODULEDEF(ChecksumAccumulator) BODY (
        VAL(cache) withFlags(PRIVATE) := TypeTree(mapType(StringClass.toType, IntClass.toType)) APPLY (),
        DEF("calculate", IntClass.toType) withParams(VAL(s, StringClass.toType).empty) :=
          (IF(cache DOT "contains" APPLY REF(s)) THEN cache.APPLY(REF(s)) 
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
      """import scala.collection.mutable.Map""",
      """object ChecksumAccumulator {""",
      """  private val cache = Map[String,Int]();""",
      """  def calculate(s: String): Int =""",
      """    if (cache.contains(s)) cache(s)""",
      """    else {""",
      """      val acc = new ChecksumAccumulator();""",
      """      for (c <- s)""",
      """        acc.add(c.toByte);""",
      """      val cs = acc.checksum();""",
      """      cache += (s -> cs);""",
      """      cs""",
      """    }""",
      """}"""
    ).inOrder
  }
  
  def e5 = {
    val IntQueue = RootClass.newClass("IntQueue".toTypeName)
    
    val trees =
      (CLASSDEF(IntQueue) withFlags(ABSTRACT) BODY (
        DEF("get", IntClass.toType).empty,
        DEF("put", IntClass.toType) withParams(VAL("x", IntClass.toType).empty) empty
      )) ::
      Nil
    
    val out = treesToString(trees); println(out)
    out.lines.toList must contain(
      """abstract class IntQueue {""",
      """  def get(): Int;""",
      """  def put(x: Int): Int""",
      """}"""
    ).inOrder
    
  }
  
  object sym {
    val println = ScalaPackageClass.newMethod("println")
    val print = ScalaPackageClass.newMethod("print")
    val to = ScalaPackageClass.newMethod("to")
  }
}
