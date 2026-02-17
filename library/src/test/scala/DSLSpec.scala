import org.specs2._

trait DSLSpec extends Specification {
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  object sym {
    val foo         = RootClass.newValue("foo")
    val Addressable = RootClass.newClass("Addressable")
    val A           = RootClass.newAliasType("A")
    val B           = RootClass.newAliasType("B")
    val C           = RootClass.newClass("C")
    val T           = RootClass.newAliasType("T")
    val run         = RootClass.newMethod("run")
  }

  def print_as(expected: String*): matcher.Matcher[Tree] =
    (actual: Tree) =>
      (expected.toList match {
        case List(x) =>
          val s = treeToString(actual); println(s)
          (s == x, s + " doesn't equal " + x)
        case list =>
          val s = treeToString(actual); println(s)
          (s.linesIterator.toList == list, s.linesIterator.toList + " doesn't equal " + list)
      })
}
