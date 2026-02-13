package treehugger

trait Names {
  implicit def promoteTermNamesAsNecessary(name: Name): TermName =
    name.toTermName

  trait Name {
    def name: String
    def isTermName: Boolean = false
    def isTypeName: Boolean = false
    def toTermName: TermName = newTermName(name)
    def toTypeName: TypeName = newTypeName(name)

    final def toChars: Array[Char] = name.toArray

    /** The length of this name. */
    final def len: Int = length
    final def length: Int = name.length
    final def isEmpty = length == 0
    final def nonEmpty = !isEmpty

    /** @return the string representation of this name */
    final override def toString(): String = name

    /** @return the i'th Char of this name */
    final def apply(i: Int): Char = name.charAt(i)

    /** @return
      *   the index of first occurrence of char c in this name, length if not
      *   found
      */
    final def pos(s: String): Int = name indexOf s

    /** Returns the index of last occurrence of char c in this name, -1 if not
      * found.
      *
      * @param c
      *   the character
      * @return
      *   the index of the last occurrence of c
      */
    final def lastPos(c: Char): Int = lastPos(c, len - 1)

    /** Returns the index of the last occurrence of char c in this name from
      * start, -1 if not found.
      *
      * @param c
      *   the character
      * @param start
      *   ...
      * @return
      *   the index of the last occurrence of c
      */
    final def lastPos(c: Char, start: Int): Int = {
      var i = start
      while (i >= 0 && name.charAt(i) != c) i -= 1
      i
    }

    /** Return the subname with characters from from to to-1. */
    def subName(from: Int, to: Int): Name

    final def containsName(subname: String): Boolean = containsName(
      newTermName(subname)
    )
    final def containsName(subname: Name): Boolean = name contains subname.name

    /** Does this name start with prefix? */
    final def startsWith(prefix: Name): Boolean = name startsWith prefix.name

    /** Does this name end with suffix? */
    final def endsWith(suffix: Name): Boolean = name endsWith suffix.name

    /** Some thoroughly self-explanatory convenience functions. They assume that
      * what they're being asked to do is known to be valid.
      */
    final def startChar: Char = apply(0)
    final def endChar: Char = apply(len - 1)
    final def startsWith(char: Char): Boolean = len > 0 && startChar == char
    final def startsWith(name: String): Boolean = startsWith(newTermName(name))
    final def endsWith(char: Char): Boolean = len > 0 && endChar == char
    final def endsWith(name: String): Boolean = endsWith(newTermName(name))
    final def stripStart(prefix: Name): Name = subName(prefix.length, len)
    final def stripStart(prefix: String): Name = subName(prefix.length, len)
    final def stripEnd(suffix: Name): Name = subName(0, len - suffix.length)
    final def stripEnd(suffix: String): Name = subName(0, len - suffix.length)

    def lastIndexOf(ch: Char) = toChars lastIndexOf ch

    def append(suffix: String): Name
    def append(suffix: Name): Name
  }

  case class TermName(name: String) extends Name {
    override def isTermName: Boolean = true

    def subName(from: Int, to: Int): TermName =
      newTermName(name.slice(from, to))

    def append(suffix: String): TermName = newTermName(this + suffix)
    def append(suffix: Name): TermName = append(suffix.toString)

    override def toTermName: TermName = this
  }

  case class TypeName(name: String) extends Name {
    override def isTypeName: Boolean = true

    def subName(from: Int, to: Int): TypeName =
      newTypeName(name.slice(from, to))

    def append(suffix: String): TypeName = newTypeName(this + suffix)
    def append(suffix: Name): TypeName = append(suffix.toString)

    override def toTypeName: TypeName = this
  }

  def newTermName(name: String) = TermName(name)
  def newTypeName(name: String) = TypeName(name)
  def EmptyTypeName: TypeName = newTypeName("")
}
