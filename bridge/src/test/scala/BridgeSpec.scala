import org.specs2._

class BridgeSpec extends Specification { def is = sequential                  ^
  "This is a specification to check Treehugger DSL"                           ^
                                                                              p^
  "transform should"                                                          ^
    """handle ID("x")"""                                                      ! transform1^
    """handle LIT(1)"""                                                       ! transform2^
    """handle ID("foo") APPLY(LIT(0))"""                                      ! transform3^
    """handle LIT(1) INFIX("+") APPLY(LIT(1))"""                              ! transform4^
    """handle REF("x")"""                                                     ! transform5^
    """handle THIS"""                                                         ! transform6^
    """handle SUPER"""                                                        ! transform7^
    """handle BLOCK()"""                                                      ! transform8^
    """handle VAL("x")"""                                                     ! transform9^
    """handle DEF("get", IntClass) := LIT(0)"""                               ! transform10^
                                                                              end
  
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  val bridge = treehugger.RuntimeBridge 

  def transform1 =
    (bridge toRuntimeTree ID("x")).toString must_== "x"
  def transform2 =
    (bridge toRuntimeTree LIT(1)).toString must_== "1"
  def transform3 =
    (bridge toRuntimeTree (ID("foo") APPLY(LIT(1)))).toString must_== "foo(1)"
  def transform4 =
    (bridge toRuntimeTree (LIT(1) INFIX("+") APPLY(LIT(1)))).toString must_== "1.+(1)"
  def transform5 =
    (bridge toRuntimeTree REF("x")).toString must_== "x"
  def transform6 =
    (bridge toRuntimeTree THIS).toString must_== "this"
  def transform7 =
    (bridge toRuntimeTree SUPER("foo")).toString must_== "foo.super"
  def transform8 =
    (bridge toRuntimeTree BLOCK(REF("x"))).toString must_== """{
  x
}"""
  def transform9 =
    (bridge toRuntimeTree VAL("x", IntClass)).toString must_== "val x: Int = _"
  def transform10 =
    (bridge toRuntimeTree (DEF("get", IntClass) := LIT(0))).toString must_== "def get(): Int = 0"
   
}
