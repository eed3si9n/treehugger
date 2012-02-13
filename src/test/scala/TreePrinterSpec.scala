import org.specs2._

class TreePrinterSpec extends DSLSpec { def is = sequential                   ^
  "This is a specification to check a TreePrinter"                            ^
                                                                              p^
  "The tree printer should"                                                   ^
    """print def hello"""                                                     ! e2^
    """print val greetStrings = new Array[String](3)"""                       ! e3^
    """print object ChecksumAccumulator"""                                    ! e4^
    """print abstract class IntQueue"""                                       ! e5^
    """print package scala"""                                                 ! e6^
    """print case List(x) => x"""                                             ! e7^
    """print case class [T <% List[T]]Address()"""                            ! e8^
    """print new Addressable {}"""                                            ! e9^
    """print mkPointed"""                                                     ! e10^
                                                                              end
  
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._
  
  def e2 = {
    val tree = DEF("hello") := BLOCK(
      Predef_println APPLY LIT("Hello, world!"))
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
    
    val tree = BLOCK(
      VAL(greetStrings) := NEW(arrayType(StringClass), LIT(3)),
      assignGreetStrings(0, "Hello"),
      assignGreetStrings(1, ", "),
      assignGreetStrings(2, "world!\n"),
      FOR(VALFROM("i") := LIT(0) INT_TO LIT(2)) DO
        (Predef_print APPLY (greetStrings APPLY REF("i")))) withoutPackage
    
    val s = treeToString(tree); println(s)
    
    s.lines.toList must contain(
      "val greetStrings = new Array[String](3)",
      "",
      "greetStrings(0) = \"Hello\"",
      "",
      "greetStrings(1) = \", \"",
      "",
      "greetStrings(2) = \"world!\\n\"",
      "",
      "for (i <- 0 to 2)",
      "  print(greetStrings(i))"
    ).inOrder
  }
  
  // p. 66
  def e4 = {
    val ChecksumAccumulator = RootClass.newClass("ChecksumAccumulator")
    val cache = ChecksumAccumulator.newValue("cache")
    val s = RootClass.newValue("s")
    
    val tree = (OBJECTDEF(ChecksumAccumulator) := BLOCK(
        VAL(cache) withFlags(Flags.PRIVATE) :=
          mutableMapType(StringClass, IntClass) APPLY (),
        DEF("calculate", IntClass) withParams(PARAM(s, StringClass)) :=
          (IF(REF(cache) DOT "contains" APPLY REF(s)) THEN REF(cache).APPLY(REF(s)) 
          ELSE BLOCK(
            VAL("acc") := NEW(ChecksumAccumulator),
            FOR(VALFROM("c") := REF(s)) DO
              (REF("acc") DOT "add" APPLY (REF("c") DOT "toByte")),
            VAL("cs") := REF("acc") DOT "checksum" APPLY (),
            REF(cache) INFIX ("+=") APPLY REF(s) INFIX ("->") APPLY REF("cs"),
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
      """      cache += s -> cs""",
      """      cs""",
      """    }""",
      """}"""
    ).inOrder
  }
  
  // p. 227
  def e5 = {
    val IntQueue: ClassSymbol = RootClass.newClass("IntQueue")
    val BasicIntQueue: ClassSymbol = RootClass.newClass("BasicIntQueue")
    val Doubling: ClassSymbol = RootClass.newClass("Doubling")
    val buf: TermSymbol = BasicIntQueue.newValue("buf")
    def arrayBufferType(arg: Type)  = appliedType(ArrayBufferClass.typeConstructor, arg)
    
    val trees =
      (CLASSDEF(IntQueue) withFlags(Flags.ABSTRACT) := BLOCK(
        DEF("get", IntClass),
        DEF("put") withParams(PARAM("x", IntClass))
      )) ::
      (CLASSDEF(BasicIntQueue) withParents(IntQueue) := BLOCK(
        VAL(buf) withFlags(Flags.PRIVATE) :=
          NEW(ArrayBufferClass TYPE_OF IntClass),
        DEF("get", IntClass) :=
          REF(buf) DOT "remove" APPLY(),
        DEF("put") withParams(PARAM("x", IntClass)) := BLOCK(
          REF(buf) INFIX("+=") APPLY REF("x")
          )
      )) ::
      (TRAITDEF(Doubling) withParents(IntQueue) := BLOCK(
        DEF("put") withFlags(Flags.ABSTRACT, Flags.OVERRIDE) withParams(PARAM("x", IntClass)) := BLOCK(
          SUPER DOT "put" APPLY (LIT(2) INT_* REF("x"))
          )
      )) ::
      Nil
    
    val out = treeToString(trees: _*); println(out)
    out.lines.toList must contain(
      """abstract class IntQueue {""",
      """  def get: Int""",
      """  def put(x: Int)""",
      """}""",
      """class BasicIntQueue extends IntQueue {""",
      """  private val buf = new scala.collection.mutable.ArrayBuffer[Int]""",
      """  def get: Int = buf.remove()""",
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
    val A = ArrowAssocClass.newTypeParameter("A")
    val arrow = ArrowAssocClass.newMethod("->")
    val B = arrow.newTypeParameter("B")
    val tuple2AB = tupleType(A.toType :: B.toType :: Nil)
    val ArrowAssocA = appliedType(ArrowAssocClass, A.toType)
    
    val tree =
      (OBJECTDEF(PredefModule) := BLOCK(
        (CLASSDEF(ArrowAssocClass) withTypeParams(TYPEVAR(A)) withParams(PARAM("x", A)) := BLOCK(
          DEF(arrow.name, tuple2AB) withTypeParams(TYPEVAR(B)) withParams(PARAM("y", B)) :=
            TUPLE(REF("x"), REF("y"))
        )),
        
        DEF("any2ArrowAssoc", ArrowAssocA)
            withFlags(Flags.IMPLICIT) withTypeParams(TYPEVAR(A)) withParams(PARAM("x", A)) :=
          NEW(ArrowAssocA, REF("x"))
      )) inPackage(ScalaPackageClass)
    
    val out = treeToString(tree); println(out)
    out.lines.toList must contain(
      """package scala""",
      """object Predef {""",
      """  class ArrowAssoc[A](x: A) {""",
      """    def ->[B](y: B): (A, B) = (x, y)""",
      """  }""",
      """  implicit def any2ArrowAssoc[A](x: A): scala.Predef.ArrowAssoc[A] = new scala.Predef.ArrowAssoc[A](x)""",
      """}"""
    ).inOrder  
  }
  
  // p. 457
  def e7 = {
    val maxListUpBound = RootClass.newMethod("maxListUpBound")
    val T = maxListUpBound.newTypeParameter("T")
    
    val tree =
      (DEF(maxListUpBound.name, T)
          withTypeParams(TYPEVAR(T) UPPER TYPE_ORDERED(T))
          withParams(PARAM("elements", TYPE_LIST(T))) :=
        REF("elements") MATCH(
          CASE(ListClass UNAPPLY()) ==> THROW(IllegalArgumentExceptionClass, "empty list!"),
          CASE(ListClass UNAPPLY(ID("x"))) ==> REF("x"),
          CASE(ID("x") LIST_:: ID("rest")) ==> BLOCK(
            VAL("maxRest") := maxListUpBound APPLY(REF("rest")),
            IF(REF("x") INT_> REF("maxRest")) THEN REF("x")
            ELSE REF("maxRest") 
          )
        ))
    
    val out = treeToString(tree); println(out)
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
    val Address = RootClass.newClass("Address")
    val T = Address.newTypeParameter("T")
    val list = Address.newValue("list")
    
    val tree: Tree =
      (CASECLASSDEF(Address)
          withTypeParams(TYPEVAR(T) VIEWBOUNDS TYPE_LIST(T))
          withParams(PARAM("name", TYPE_OPTION(T)) := REF(NoneModule)) := BLOCK(
        DEF("stringOnly", Address) withParams(PARAM("ev", TYPE_=:=(T, StringClass)) withFlags(Flags.IMPLICIT)) :=
          Address APPLY(THIS DOT "name" MAP LAMBDA(VAL("nm", StringClass)) ==> BLOCK(
            VAL(list, TYPE_LIST(T)) := REF("nm"),
            (list MAP LAMBDA(VAL("x")) ==>
              (REF("x") INFIX(StringAdd_+) APPLY LIT("x"))) DOT "mkString" APPLY LIT(" ")
          )),
        DEF("star") withParams(PARAM("n", TYPE_*(IntClass))) :=
          Address APPLYTYPE(StringClass) APPLY SOME(LIT("foo")).MAP(WILDCARD INFIX(StringAdd_+) APPLY LIT("x"))
      ))
      
    val out = treeToString(tree); println(out)
    out.lines.toList must contain(
      """case class Address[T <% List[T]](name: Option[T] = None) {""",
      """  def stringOnly(implicit ev: =:=[T,String]): Address = Address(this.name map { (nm: String) =>""",
      """    val list: List[T] = nm""",
      """    list.map(x => x + "x").mkString(" ")""",
      """  })""",
      """  def star(n: Int*) = Address[String](Some("foo").map(_ + "x"))""",
      """}"""
    ).inOrder
  }
  
  def e9 = {
    val Addressable = RootClass.newClass("Addressable")
    val street = Addressable.newMethod("street")
    
    val tree: Tree =
      NEW(ANONDEF(Addressable) := BLOCK(
        VAL(street) := LIT("123 Drive")
      ))
    
    val out = treeToString(tree); println(out)
    out.lines.toList must contain(
      "new Addressable {",
      "  val street = \"123 Drive\"",
      "}"
    ).inOrder
  }

  def e10 = {
    object sym {
      val mkPointed = RootClass.newMethod("mkPointed")
      val Const = RootClass.newClass("Const")
      val Pointed = RootClass.newClass("Pointed")
      val Monoid = RootClass.newClass("Monoid")
    }

    val tree: Tree =
      (DEF(sym.mkPointed)
          withFlags(Flags.IMPLICIT)
          withTypeParams(TYPEVAR("M") CONTEXTBOUNDS sym.Monoid) :=
            NEW(ANONDEF(sym.Pointed TYPE_OF (TYPE_STRUCT(
              TYPEVAR("L") withTypeParams(TYPEVAR("A")) := sym.Const APPLYTYPE ("M", "A")
            ) TYPE_#("L"))) := BLOCK(
        DEF("point")
            withTypeParams(TYPEVAR("A"))
            withParams(PARAM("a", TYPE_BYNAME("A"))) :=
          (sym.Const APPLYTYPE ("M", "A") APPLY(
            Predef_implicitly APPLYTYPE(sym.Monoid TYPE_OF "M") DOT "z"))
      )))
    
    val out = treeToString(tree); println(out)
    out.lines.toList must contain(
      "implicit def mkPointed[M : Monoid] = new Pointed[({ type L[A] = Const[M, A] })#L] {",
      "  def point[A](a: => A) = Const[M, A](implicitly[Monoid[M]].z)",
      "}"
    ).inOrder
  }
}
