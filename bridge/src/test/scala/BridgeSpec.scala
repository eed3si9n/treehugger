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
                                                                              end
  
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  val rbt = treehugger.RuntimeBridge.toRuntimeTree

  def transform1 =
    (ID("x") transform rbt).toString must_== "x"
  def transform2 =
    (LIT(1) transform rbt).toString must_== "1"
  def transform3 =
    ((ID("foo") APPLY(LIT(1))) transform rbt).toString must_== "foo(1)"
  def transform4 =
    ((LIT(1) INFIX("+") APPLY(LIT(1))) transform rbt).toString must_== "1.+(1)"
  def transform5 =
    (REF("x") transform rbt).toString must_== "x"
  def transform6 =
    (THIS transform rbt).toString must_== "this"
  def transform7 =
    (SUPER("foo") transform rbt).toString must_== "foo.super"
  def transform8 =
    (BLOCK(REF("x")) transform rbt).toString must_== """{
  x
}"""
  def transform9 =
    (VAL("x", IntClass) transform rbt).toString must_== "val x: Int = _"
}
