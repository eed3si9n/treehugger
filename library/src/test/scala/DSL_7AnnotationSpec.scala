import org.specs2._

class DSL_7AnnotationSpec extends DSLSpec { def is =                          s2"""
  This is a specification to check Treehugger DSL

  Declaration annotations are written as
    `CLASSDEF("C") withAnnots(ANNOT(typ|"C", arg, ...), ...)`, or             $declannot1
    `TYPEVAR(typ|"T") withAnnots(ANNOT(typ|"C", arg, ...), ...)`.             $declannot2

  Expression annotations are written as
    `tree withAnnots(ANNOT(typ), ...)`.                                       $exp1
                                                                              """
  
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._
  
  def declannot1 =
    (CLASSDEF("C") withAnnots(ANNOT(SerializableAttr)) := BLOCK(
      DEF("get", IntClass)
    )) must print_as(
      """@serializable class C {""",
      """  def get: Int""",
      """}"""
    )

  def declannot2 =
    (TRAITDEF("Function0")
        withTypeParams(TYPEVAR("T") withAnnots(ANNOT("specialized", REF(IntClass)))) := BLOCK(
      DEF("apply", "T")
    )) must print_as(
      "trait Function0[@specialized(Int) T] {",
      "  def apply: T",
      "}"
    )

  def exp1 =
    ((REF("e") withAnnots(ANNOT(UncheckedClass))) must print_as("(e: @unchecked)")) and
    ((REF("e") withAnnots(ANNOT("unchecked"))) must print_as("(e: @unchecked)"))
}
