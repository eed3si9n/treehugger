import org.specs2._

trait DSLSpec extends Specification { 
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._
  
  object sym {
    val println = ScalaPackageClass.newMethod("println")
    val print = ScalaPackageClass.newMethod("print")
    
    val foo = RootClass.newValue("foo")
    val Addressable = RootClass.newClass("Addressable")
    val A = RootClass.newTypeParameter("A")
    val B = RootClass.newTypeParameter("B")
    val C = RootClass.newClass("C")
    val T = RootClass.newTypeParameter("T")
  }
  
  def print_as(expected: String*): matcher.Matcher[Tree] =
    (actual: Tree) => (expected.toList match {
      case List(x) =>
        val s = treeToString(actual); println(s)
        (s == x, s + " doesn't equal " + x)
      case list    => 
        val s = treeToString(actual); println(s)
        (s.lines.toList == list, s.lines.toList + " doesn't equal " + list)
    }) 
}
