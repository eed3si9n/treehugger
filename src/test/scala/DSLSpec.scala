import org.specs2._

trait DSLSpec extends Specification { 
  import treehugger._
  import definitions._
  import treehuggerDSL._
  
  object sym {
    val println = ScalaPackageClass.newMethod("println")
    val print = ScalaPackageClass.newMethod("print")
    
    val foo = RootClass.newValue("foo")
    val Addressable = RootClass.newClass("Addressable".toTypeName)
    val A = RootClass.newTypeParameter("A".toTypeName)
    val B = RootClass.newTypeParameter("B".toTypeName)
    val T = RootClass.newTypeParameter("T".toTypeName)
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
