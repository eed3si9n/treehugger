import org.specs2._

class DSL_4ImplicitSpec extends DSLSpec { def is = sequential                 ^
  "This is a specification to check Treehugger DSL"                           ^
                                                                              p^
  "Implicit members are written as"                                           ^
      """`VAL(sym|"x", typ)` withFlags(Flags.IMPLICIT)"""                     ! implicit1^
                                                                              p^
  "Implicit parameters are written as"                                        ^
      """`withParams(VAL(sym|"x", typ) withFlags(Flags.IMPLICIT))`"""         ! implicit2^                                                                            
                                                                              p^
                                                                              end
  
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._
                                                                             
  def implicit1 =
    (VAL("x", sym.Addressable) withFlags(Flags.IMPLICIT): Tree) must print_as(
      "implicit val x: Addressable")
  
  def implicit2 =
    (DEF("put", UnitClass) withParams(PARAM("x", IntClass) withFlags(Flags.IMPLICIT)) := UNIT) must print_as(
    "def put(implicit x: Int): Unit = ()")
}
