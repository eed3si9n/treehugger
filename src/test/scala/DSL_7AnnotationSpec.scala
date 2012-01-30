import org.specs2._

class DSL_7AnnotationSpec extends DSLSpec { def is = sequential               ^
  "This is a specification to check Treehugger DSL"                           ^
                                                                              p^
  "Expression annotations are written as"                                     ^
      """`tree withAnnotation(ANNOT(typ))`."""                                ! exp1^
                                                                              p^
                                                                              end
  
  import treehugger._
  import definitions._
  import treehuggerDSL._
  
  def exp1 = (REF("e") withAnnotation(ANNOT(UncheckedClass))) must print_as("(e: @unchecked)")
}
