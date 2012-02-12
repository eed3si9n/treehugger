import org.specs2._

class DSL_4ImplicitSpec extends DSLSpec { def is = sequential                 ^
  "This is a specification to check Treehugger DSL"                           ^
                                                                              p^
  "Implicit members are written as"                                           ^
      """`VAL(sym|"x", typ|"Int")` withFlags(Flags.IMPLICIT)"""               ! implicit1^
                                                                              p^
  "Implicit parameters are written as"                                        ^
      """`withParams(VAL(sym|"x", typ|"Int") withFlags(Flags.IMPLICIT))`"""   ! implicit2^                                                                            
                                                                              p^
                                                                              end
  
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._
                                                                             
  def implicit1 =
    (DEF("intToRational") withFlags(Flags.IMPLICIT)
      withParams(PARAM("x", IntClass)) := NEW("Rational", REF("x"))) must print_as(
    "implicit def intToRational(x: Int) = new Rational(x)")
  
  def implicit2 =
    (DEF("greet")
      withParams(PARAM("name", StringClass))
      withParams(PARAM("config", "Config") withFlags(Flags.IMPLICIT)) := BLOCK(
      sym.println APPLY(REF("config") APPLY REF("name"))
    )) must print_as(
    "def greet(name: String)(implicit config: Config) {",
    "  println(config(name))",
    "}")
}
