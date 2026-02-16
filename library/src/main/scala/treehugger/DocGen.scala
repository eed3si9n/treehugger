package treehugger

trait DocGen { self: Forest =>
  import Flags._

  sealed trait DocElement
  case class DocText(text: String) extends DocElement {
    override def toString: String = text
  }
  case object DocTag {
    def apply(tag: String, args: Any*): DocTag = DocTag(tag, args.toList)

    def Param(args: Any*)   = DocTag("@param", args.toList)
    def Tparam(args: Any*)  = DocTag("@tparam", args.toList)
    def Return(args: Any*)  = DocTag("@return", args.toList)
    def Throws(args: Any*)  = DocTag("@throws", args.toList)
    def See(args: Any*)     = DocTag("@see", args.toList)
    def Note(args: Any*)    = DocTag("@note", args.toList)
    def Example(args: Any*) = DocTag("@example", args.toList)
    def UseCase(args: Any*) = DocTag("@usecase", args.toList)
    def Author(args: Any*)  = DocTag("@author", args.toList)
    def Version(args: Any*) = DocTag("@version", args.toList)
    def Since(args: Any*)   = DocTag("@since", args.toList)
    def ToDo(args: Any*)    = DocTag("@todo", args.toList)
  }
  case class DocTag(tag: String, args: List[Any]) extends DocElement {
    override def toString: String =
      tag + (args match {
        case Nil => ""
        case _   =>
          " " + (args map {
            case sym: Symbol => sym.fullName
            case x           => x.toString
          }).mkString(" ")
      })
  }
  def mkScalaDoc(target: Tree, elems: List[DocElement]): Commented =
    Commented(Modifiers(SCALADOC_COMMENT), elems.map(_.toString), target)
}
