import org.specs2._

class DSL_7AnnotationSpec extends DSLSpec { def is = sequential               ^
  "This is a specification to check Treehugger DSL"                           ^
                                                                              p^
  "Declaration annotations are written as"                                    ^
      """`CLASSDEF("C") withAnnotation(ANNOT(typ), ...) := BLOCK()`."""       ! declannot1^
                                                                              p^
  "Expression annotations are written as"                                     ^
      """`tree withAnnotation(ANNOT(typ), ...)`."""                           ! exp1^
                                                                              p^
                                                                              end
  
  import treehugger._
  import definitions._
  import treehuggerDSL._
  
  def declannot1 =
    (CLASSDEF("C") withAnnotation(ANNOT(SerializableAttr)) := BLOCK(
      DEF("get", IntClass)
    )) must print_as(
      """@scala.annotation.serializable class C {""",
      """  def get: Int""",
      """}"""
    )

  def exp1 = (REF("e") withAnnotation(ANNOT(UncheckedClass))) must print_as("(e: @unchecked)")
}
