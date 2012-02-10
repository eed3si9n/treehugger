import org.specs2._

class DSL_2ClassSpec extends DSLSpec { def is = sequential                    ^
  "This is a specification to check Treehugger DSL"                           ^
                                                                              p^
  "Class definitions are written as"                                          ^
      """`CLASSDEF(sym|"C")`, or"""                                           ! class1^
      """with the class body as `CLASSDEF(sym|"C") := BLOCK(stat, ...)`."""   ! class2^
      """`CLASSDEF(sym|"C") withParams(PARAM("x", typ1), VAL("y", typ2), VAR("z": typ3), ...)`
where `PARAM(...)` declares a parameter while 
`VAL(...)` and `VAR(...)` declare parameters with an accessor."""             ! class3^
      """Polymorphic classes are written as
`CLASSDEF(sym|"C") withTypeParams(TYPE(typ))`."""                             ! class4^
      """Classes with base classes are written as
`CLASSDEF(sym|"C") withParents(typ|"B", ...)`."""                             ! class5^
      """Using `withFlags(flag, ...)`, classes with access modifier can be written as
`CLASSDEF(sym|"C") withFlags(Flags.PRIVATE)`."""                              ! class6^
      """Other uses of `withFlags(flag)` are abstract classes withFlags(Flags.ABSTRACT)`,
final classes `withFlags(Flags.FINAL)`,
sealed classes `withFlags(Flags.SEALED)`."""                                  ! class7^
                                                                              end^
  "Case class definitions are written as"                                     ^
      """`CASECLASSDEF(sym|"C")`, or with the class body, parameters, and parents as
`CASECLASSDEF(sym|"C")` withParams(PARAM("x", typ1), ...) withParents(typ, ...) := BLOCK(stat, ...).""" ! caseclass1^
                                                                              end^
  "Trait definitions are written as"                                          ^
      """`TRAITDEF(sym|"D")`."""                                              ! trait1^
                                                                              end^
  "Object definitions are written as"                                         ^
      """`MODULEDEF(sym|"E")`."""                                             ! object1^
                                                                              end^
  "Class members can"                                                         ^
      """be defined by placing value defitions and function definitions within the class body as
`CLASSDEF(sym|"C") := BLOCK(DEF(sym|"get", typ) := rhs, ...)`."""             ! member1^
      """Class members with access modifier can be written as
`DEF(sym|"get", typ) withFlags(Flags.PROTECTED) := rhs`,"""                   ! member2^
      """`DEF(sym|"get", typ) withFlags(Flags.OVERRIDE) := rhs`,"""           ! member3^
      """`DEF(sym|"get", typ) withFlags(PRIVATEWITHIN("this")) := rhs`."""    ! member4^
                                                                              p^
  "Early definitions can be written as"                                       ^
      """`CLASSDEF(sym|"C") withEarlyDefs(stat, ...) withParents(typ, ...) := BLOCK(stat, ...)`.""" ! early1^
                                                                              end
  
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._
                                                                             
  def class1 = (CLASSDEF("C"): Tree) must print_as("class C")
  
  def class2 =
    (CLASSDEF("C") := BLOCK(
      VAL("x") := LIT(0)
    )) must print_as(
      """class C {""",
      """  val x = 0""",
      """}"""
    )
  
  def class3 = {
    val tree: Tree = CLASSDEF("C") withParams(PARAM("x", IntClass), VAL("y", StringClass), VAR("z", listType(StringClass)))
    tree must print_as("class C(x: Int, val y: String, var z: List[String])")
  }
  
  def class4 = (CLASSDEF("C") withTypeParams(TYPE(sym.T)): Tree) must print_as("class C[T]")
  
  def class5 = (CLASSDEF("C") withParents(sym.Addressable): Tree) must print_as("class C extends Addressable")
    
  def class6 = (CLASSDEF("C") withFlags(Flags.PRIVATE): Tree) must print_as("private class C")
    
  def class7 =
    ((CLASSDEF("C") withFlags(Flags.ABSTRACT): Tree) must print_as("abstract class C")) and
    ((CLASSDEF("C") withFlags(Flags.FINAL): Tree) must print_as("final class C")) and
    ((CLASSDEF("C") withFlags(Flags.SEALED): Tree) must print_as("sealed class C"))
      
  def caseclass1 =
    ((CASECLASSDEF("C"): Tree) must print_as("case class C")) and
    ((CASECLASSDEF("C") withParams(PARAM("x", IntClass)) withParents(sym.Addressable) := BLOCK(
      DEF("y") := LIT(0)
    ))
      must print_as(
        """case class C(x: Int) extends Addressable {""",
        """  def y = 0""",
        """}"""
      ))
  
  def trait1 = (TRAITDEF("D"): Tree) must print_as("trait D")
  
  def object1 = (MODULEDEF("E"): Tree) must print_as("object E")
  
  def member1 =
    (CLASSDEF("C") := BLOCK(
      DEF("get") := LIT(0)
    )) must print_as(
      """class C {""",
      """  def get = 0""",
      """}"""
    )
  
  def member2 =
    (CLASSDEF("C") := BLOCK(
      DEF("get") withFlags(Flags.PROTECTED) := LIT(0)
    )) must print_as(
      """class C {""",
      """  protected def get = 0""",
      """}"""
    )
  
  def member3 =
    (CLASSDEF("C") := BLOCK(
      DEF("get") withFlags(Flags.OVERRIDE) := LIT(0)
    )) must print_as(
      """class C {""",
      """  override def get = 0""",
      """}"""
    )
  
  def member4 =
    (CLASSDEF("C") := BLOCK(
      DEF("get") withFlags(PRIVATEWITHIN("this")) := LIT(0)
    )) must print_as(
      """class C {""",
      """  private[this] def get = 0""",
      """}"""
    )
    
  def early1 =
    (CLASSDEF("C") withEarlyDefs(
      VAL("name") := LIT("Bob")
    ) withParents("B") := BLOCK(
      sym.print APPLY REF("msg")
    )) must print_as(
    "class C extends {",
    "  val name = \"Bob\"",
    "} with B {",
    "  print(msg)",
    "}"
  )
}
