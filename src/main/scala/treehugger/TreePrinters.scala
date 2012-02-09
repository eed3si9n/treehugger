/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package treehugger

import java.io.{ OutputStream, PrintWriter, StringWriter, Writer }
import Flags._

trait TreePrinters extends api.TreePrinters { self: Forest =>

  //nsc import treeInfo.{ IsTrue, IsFalse }

  final val showOuterTests = false

  /** Adds backticks if the name is a scala keyword. */
  def quotedName(name: Name, decode: Boolean): String = {
    val s = name.name
    val term = name.toTermName
    if (nme.keywords(term) && term != nme.USCOREkw) "`%s`" format s
    else s
  }
  def quotedName(name: Name): String = quotedName(name, false)

  /** Turns a path into a String, introducing backquotes
   *  as necessary.
   */
  def backquotedPath(t: Tree): String = t match {
    case Select(qual, name) => "%s.%s".format(backquotedPath(qual), quotedName(name))
    case id: Ident if id.symbol != NoSymbol => id.symbol.fullName
    case Ident(name)        => quotedName(name)
    case _                  => t.toString
  }

  class TreePrinter(out: PrintWriter) extends super.TreePrinter {
    protected var indentMargin = 0
    protected val indentStep = 2
    protected var indentString = "                                        " // 40

    typesPrinted = false // settings.printtypes.value
    uniqueIds = false // settings.uniqid.value
    protected def doPrintPositions = false // settings.Xprintpos.value

    def indent() = indentMargin += indentStep
    def undent() = indentMargin -= indentStep

    def printPosition(tree: Tree) = () // if (doPrintPositions) print(showPos(tree.pos))

    def println() {
      out.println()
      while (indentMargin > indentString.length())
        indentString += indentString
      if (indentMargin > 0)
        out.write(indentString, 0, indentMargin)
    }

    def printSeq[a](ls: List[a])(printelem: a => Unit)(printsep: => Unit) {
      ls match {
        case List() =>
        case List(x) => printelem(x)
        case x :: rest => printelem(x); printsep; printSeq(rest)(printelem)(printsep)
      }
    }

    def printColumn(ts: List[Tree], start: String, sep: String, end: String) {
      print(start); indent; println()
      printSeq(ts){print(_)}{print(sep); println()}; undent; println(); print(end)
    }

    def printRow(ts: List[Tree], start: String, sep: String, end: String) {
      print(start); printSeq(ts){print(_)}{print(sep)}; print(end)
    }

    def printRow(ts: List[Tree], sep: String) { printRow(ts, "", sep, "") }

    def printTypeParams(ts: List[TypeDef]) {
      if (!ts.isEmpty) {
        print("["); printSeq(ts){ t =>
          printAnnotations(t)
          printParam(t)
        }{print(", ")}; print("]")
      }
    }
    
    def printValueParams(ts: List[ValDef]) {
      printValueParams(ts, false)
    }
    
    def printValueParams(ts: List[ValDef], isclass: Boolean) {
      print("(")
      if (!ts.isEmpty) printFlags(ts.head.mods.flags & IMPLICIT, "")
      printSeq(ts){printParam(_, isclass)}{print(", ")}
      print(")")
    }

    def printLambdaParams(ts: List[ValDef]) {
      if (ts.size == 1 &&
        !ts.head.mods.hasFlag(IMPLICIT) &&
        !(ts.head.lhs.isInstanceOf[Typed]) ) printParam(ts.head, false)
      else printValueParams(ts)
    }

    def printParam(tree: Tree, isclass: Boolean = false) {
      tree match {
        case ValDef(mods, lhs, rhs) =>
          printPosition(tree)
          if (isclass && !mods.hasFlag(Flags.PARAM)) {
            if (mods.hasFlag(Flags.MUTABLE)) print("var ")
            else print("val ")
          } // if
          printAnnotations(tree)

          lhs match {
            case Typed(expr, tpt) => print(expr, ": ", tpt)
            case _ => print(lhs)
          }
          printOpt(" = ", rhs)
        case TypeDef(mods, name, tparams, rhs) =>
          printPosition(tree)
          print(symName(tree, name))
          printTypeParams(tparams)
          if (!rhs.isEmpty) print(" ", rhs)
      }
    }

    def printBlock(tree: Tree) {
      tree match {
        case Block(_, _) =>
          print(tree)
        case _ =>
          printColumn(List(tree), "{", ";", "}")
      }
    }

    private def symFn[T](tree: Tree, f: Symbol => T, orElse: => T): T = tree.symbol match {
      case null | NoSymbol  => orElse
      case sym              => f(sym)
    }
    private def ifSym(tree: Tree, p: Symbol => Boolean) = symFn(tree, p, false)

    private def symNameInternal(tree: Tree, name: Name, decoded: Boolean): String = {
      def nameFn(sym: Symbol) = {
        val prefix =
          if (sym.isCovariant) "+"
          else if (sym.isContravariant) "-"
          // if (sym.isMixinConstructor) "/*%s*/".format(quotedName(sym.owner.name, decoded))
          else ""
        val suffix = if (uniqueIds) "#"+sym.id else ""
        prefix + tree.symbol.decodedName + suffix
      }
      symFn(tree, nameFn, quotedName(name, decoded))
    }

    def decodedSymName(tree: Tree, name: Name) = symNameInternal(tree, name, true)
    def symName(tree: Tree, name: Name) = symNameInternal(tree, name, false)

    def printOpt(prefix: String, tree: Tree) {
      if (!tree.isEmpty) { print(prefix, tree) }
    }

    def printModifiers(tree: Tree, mods: Modifiers): Unit = printFlags(
       if (tree.symbol == NoSymbol) mods.flags else tree.symbol.flags, "" + (
         if (tree.symbol == NoSymbol) mods.privateWithin
         else if (tree.symbol.hasAccessBoundary) tree.symbol.privateWithin.name
         else ""
      )
    )

    def printFlags(flags: Long, privateWithin: String) {
      var mask: Long = PrintableFlags // if (settings.debug.value) -1L else PrintableFlags
      val s = flagsToString(flags & mask, privateWithin)
      if (s != "") print(s + " ")
    }

    def printAnnotations(tree: Tree) {
      val annots = tree.symbol.annotations match {
        case Nil  => tree.asInstanceOf[MemberDef].mods.annotations
        case anns => anns
      }
      annots foreach (annot => print("@"+annot+" "))
    }

    private var currentOwner: Symbol = NoSymbol
    private var selectorType: Type = NoType
    
    def typeTreeToString(tt: TypeTree): String =
      if ((tt.tpe eq null) || (doPrintPositions && tt.original != null)) {
        if (tt.original != null) "<type: " + tt.original + ">"
        else "<type ?>"
      } else if ((tt.tpe.typeSymbol ne null) && tt.tpe.typeSymbol.isAnonymousClass) {
        tt.tpe.typeSymbol.toString
      } else {
        tt.tpe.toString
      }

    private def isinline(tree: Tree): Boolean =
      tree match {
        case x: Infix => true
        case x: Apply => true
        case x: Literal => true
        case x: Ident => true
        case x: Select => true
        case x: Assign => true
        case x: Return => true
        case x: Throw => true
        case x: New => true
        case x: Typed => true
        case x: TypeApply => true
        
        case _ => false
      }

    private def unaryop(name: Name): Option[String] =
      Map(nme.UNARY_! -> "!", nme.UNARY_+ -> "+", nme.UNARY_- -> "-", nme.UNARY_~ -> "~").get(name)

    def printTree(tree: Tree) {
      tree match {
        case EmptyTree =>
          print("")
        
        case classdef: ClassDef if classdef.name == tpnme.ANON_CLASS_NAME =>
           print(classdef.impl)
        
        case ClassDef(mods, name, tparams, vparams, impl) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          val word =
            if (mods.hasTraitFlag) "trait"
            else if (ifSym(tree, _.isModuleClass)) "object"
            else "class"

          print(word, " ", symName(tree, name))
          printTypeParams(tparams)
          if (vparams != Nil) printValueParams(vparams, true)
          
          print(if (mods.isDeferred) " <: "
                else if (impl.parents.isEmpty) ""
                else " extends ", impl)

        case PackageDef(mods, packaged, stats) =>
          printAnnotations(tree)
          
          if (packaged != NoPackage)
            print("package ", packaged)

          if (mods.isHeader) {
            if (packaged != NoPackage) {
              println(); println()
            }

            printSeq(stats){print(_)}{println(); println()}
          }
          else printColumn(stats, " {", "", "}")

        case ModuleDef(mods, name, impl) =>
          printAnnotations(tree)
          printModifiers(tree, mods);
          print("object " + symName(tree, name))
          if (impl.parents.isEmpty) print("") 
          else print(" extends ")
          print(impl)

        case ValDef(mods, lhs, rhs) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          print(if (mods.isMutable) "var " else "val ")
          
          // , symName(tree, name)
          lhs match {
            case Typed(expr, tpt) => print(expr, ": ", tpt)
            case _ => print(lhs)
          }
          if (!mods.isDeferred && !rhs.isEmpty)
            print(" = ", rhs)

        case DefDef(mods, name, tparams, vparamss, tp, b: Block) if tp.isEmpty =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          print("def " + symName(tree, name))
          printTypeParams(tparams)
          vparamss match {
            case List() | List(List()) => //
            case _ => vparamss foreach printValueParams
          }
          print(" ", b)
            
        case DefDef(mods, name, tparams, vparamss, tp, rhs) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          print("def " + symName(tree, name))
          printTypeParams(tparams)
          vparamss match {
            case List() | List(List()) => //
            case _ => vparamss foreach printValueParams
          }
          printOpt(": ", tp)
          if (!rhs.isEmpty)
            rhs match {
              case b: Block => print(" = ", b)
              case _ =>
                if (isinline(rhs)) print(" = ", rhs)
                else {
                  print(" ="); indent; println(); print(rhs); undent
                }
            }
        
        case AnonFunc(vparamss, tp: TypeTree, rhs: Block) =>
          print("{ ")
          if (vparamss.size == 1) printLambdaParams(vparamss.head)
          else vparamss foreach printValueParams
          print(" =>")
          printColumn(rhs.stats ::: List(rhs.expr), "", "", "")
          print("}")
          
        case AnonFunc(vparamss, tp: TypeTree, rhs) =>
          if (vparamss.size == 1) printLambdaParams(vparamss.head)
          else vparamss foreach printValueParams
          printOpt(": ", tp)
          if (!rhs.isEmpty) print(" => ", rhs)
                              
        case TypeDef(mods, name, tparams, rhs) =>
          if (mods hasFlag (PARAM | DEFERRED)) {
            printAnnotations(tree)
            printModifiers(tree, mods); print("type "); printParam(tree)
          } else {
            printAnnotations(tree)
            printModifiers(tree, mods); print("type " + symName(tree, name))
            printTypeParams(tparams)
            rhs match {
              case tt: TypeTree =>
                tt.tpe match {
                  case bounds: TypeBounds => print(" ", bounds)
                  case _ => printOpt(" = ", rhs)
                }
              case _ => printOpt(" = ", rhs)
            }
          }

        case LabelDef(name, param, rhs) =>
          name match {
            case x if x == nme.WHILEkw =>
              print("while (", param, ") ")
              printBlock(rhs)
            case x if x == nme.DOkw =>
              print("do ")
              printBlock(rhs)
              print(" while (", param, ")")
            case _ =>
              print(symName(tree, name))
              print(" (", param, ") ")
              printBlock(rhs)
          }
        case Import(expr, selectors) =>
          // Is this selector remapping a name (i.e, {name1 => name2})
          def isNotRemap(s: ImportSelector) : Boolean = (s.name == nme.WILDCARD || s.name == s.rename)
          def selectorToString(s: ImportSelector): String = {
            val from = quotedName(s.name)
            if (isNotRemap(s)) from
            else from + " => " + quotedName(s.rename)
          }
          print("import ", backquotedPath(expr))
          
          if (selectors.isEmpty) print("")
          else selectors match {
            case List(s) =>
              print(".")
              // If there is just one selector and it is not remapping a name, no braces are needed
              if (isNotRemap(s)) print(selectorToString(s))
              else print("{", selectorToString(s), "}")
              // If there is more than one selector braces are always needed
            case many =>
              print(".", many.map(selectorToString).mkString("{", ", ", "}"))
          }

       case Template(parents, self, body) =>
          val currentOwner1 = currentOwner
          if (tree.symbol != NoSymbol) currentOwner = tree.symbol.owner
          printRow(parents, " with ")
          if (!body.isEmpty) {
            if (self.name != nme.WILDCARD) {
              print(" { ", self.name); printOpt(": ", self.tpt); print(" => ")
            } else if (!self.tpt.isEmpty) {
              print(" { _ : ", self.tpt, " => ")
            } else {
              print(" {")
            }
            printColumn(body, "", "", "}")
          }
          currentOwner = currentOwner1

        case Block(stats, expr) =>
          printColumn(stats ::: List(expr), "{", "", "}")

        case Commented(comments, expr) =>
          for {
            comment <- comments
            line <- comment.lines
          } {
            print("// ", line); println()
          }
          print(expr)

        case Match(selector, cases) =>
          val selectorType1 = selectorType
          selectorType = selector.tpe
          print(selector); printColumn(cases, " match {", "", "}")
          selectorType = selectorType1

        case CaseDef(pat, guard, body) =>
          print("case ")
          def patConstr(pat: Tree): Tree = pat match {
            case Apply(fn, args) => patConstr(fn)
            case _ => pat
          }
          // if (showOuterTests &&
          //    needsOuterTest(
          //      patConstr(pat).tpe.finalResultType, selectorType, currentOwner))
          //  print("???")
          print(pat); printOpt(" if ", guard)
          print(" => ", body)

        case Alternative(trees) =>
          printRow(trees, "(", "| ", ")")

        case Star(elem) =>
          print("(", elem, ")*")

        case Bind(name, t) =>
          print("(", symName(tree, name), " @ ", t, ")")

        case UnApply(fun, args) =>
          print(fun); printRow(args, "(", ", ", ")")

        case ArrayValue(elemtpt, trees) =>
          print("Array[", elemtpt); printRow(trees, "]{", ", ", "}")

        case Function(vparams, body) =>
          print("("); printValueParams(vparams); print(" => ", body, ")")
          if (uniqueIds && tree.symbol != null) print("#"+tree.symbol.id)

        case Assign(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case If(cond, thenp, elsep) =>
          print("if (", cond, ") ")
          print(thenp)
          if (!elsep.isEmpty) {
            println(); print("else "); print(elsep)
          }

        case Return(expr) =>
          print("return ", expr)

        case Try(block, catches, finalizer) =>
          print("try "); printBlock(block)
          if (!catches.isEmpty) printColumn(catches, " catch {", "", "}")
          printOpt(" finally ", finalizer)

        case Throw(expr) =>
          print("throw ", expr)

        case New(classdef: ClassDef)
        if classdef.name == tpnme.ANON_CLASS_NAME &&
        classdef.impl.parents.isEmpty  =>
          print("new", classdef)

        case New(tpe) =>
          print("new ", tpe)

        case Typed(expr, tp) =>
          print("(", expr, ": ", tp, ")")

        case TypeApply(fun, targs) =>
          print(fun); printRow(targs, "[", ", ", "]")
        
        case Apply(fun, vargs) =>
          if (!isTupleTree(tree)) print(fun)

          printRow(vargs, "(", ", ", ")")

        case ApplyDynamic(qual, vargs) =>
          print("<apply-dynamic>(", qual, "#", tree.symbol.nameString)
          printRow(vargs, ", (", ", ", "))")

        case Super(This(qual), mix) =>
          if (!qual.isEmpty || tree.symbol != NoSymbol) print(symName(tree, qual) + ".")
          print("super")
          if (!mix.isEmpty)
            print("[" + mix + "]")

        case Super(qual, mix) =>
          if (!qual.isEmpty) print(qual + ".")
          print("super")
          if (!mix.isEmpty)
            print("[" + mix + "]")

        case This(qual) =>
          if (!qual.isEmpty) print(symName(tree, qual) + ".")
          print("this")
        
        case Select(qualifier, name) if unaryop(name).isDefined =>
          print(unaryop(name).get, "(", qualifier, ")")

        case Select(qual @ New(tpe), name) => // if (!settings.debug.value) =>
          print(qual)

        case Select(Literal(x), name) =>
          print(x.escapedStringValue, ".", symName(tree, name))
        
        case Select(qualifier, name) =>
          print(qualifier, ".", symName(tree, name))
          // print(backquotedPath(qualifier), ".", symName(tree, name))
                
        case Ident(name) =>
          tree match {
            case BackQuotedIdent(name) =>
              print("`", symName(tree, name), "`")
            case _ =>
              print(symName(tree, name))
          }
        case Literal(x) =>
          print(x.escapedStringValue)

        case tt: TypeTree =>
          print(typeTreeToString(tt))
          
        case Annotated(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), tree) =>
          def printAnnot() {
            print("@", tpt)
            if (!args.isEmpty)
              printRow(args, "(", ", ", ")")
          }
          print(tree, if (tree.isType) " " else ": ")
          printAnnot()

        case SingletonTypeTree(ref) =>
          print(ref, ".type")

        case SelectFromTypeTree(qualifier, selector) =>
          print(qualifier, "#", symName(tree, selector))

        case CompoundTypeTree(templ) =>
          print(templ)

        case AppliedTypeTree(tp, args) =>
          print(tp); printRow(args, "[", ", ", "]")

        case TypeBoundsTree(lo, hi) =>
          printOpt(" >: ", lo); printOpt(" <: ", hi)

        case ExistentialTypeTree(tpt, whereClauses) =>
          print(tpt);
          printColumn(whereClauses, " forSome { ", ";", "}")

// treehugger extensions
        case ForTree(enumerators, body) =>
          print("for ")
          if (enumerators.size == 1) {
            printRow(enumerators, "(", ";", ")")
            indent(); println();
            print(body)
            undent()
          } 
          else {
            printColumn(enumerators, "{", "", "} ")
            print(body)
          }
        case ForYieldTree(enumerators, body) =>
          print("for ")
          if (enumerators.size == 1) {
            printRow(enumerators, "(", ";", ")")
            indent(); println();
            print("yield "); print(body)
            undent()
          } 
          else {
            printColumn(enumerators, "{", "", "} ")
            print("yield "); print(body)
          }          
        case ForValFrom(_, name, tp, rhs) =>
          print(symName(tree, name))
          printOpt(": ", tp)
          print(" <- ", rhs)  
        case ForValDef(_, name, tp, rhs) =>
          print(symName(tree, name))
          printOpt(": ", tp)
          print(" = ", rhs)  
        case ForFilter(_, test: Tree) =>
          print("if ", test)
        case Infix(Literal(x), name, args) =>
          print(x.escapedStringValue, " ", symName(tree, name), " ")
          if (args.size == 1) print(args(0))
          else printRow(args, "(", ", ", ")")
        case Infix(qualifier, name, args) =>
          print(qualifier, " ", symName(tree, name), " ")
          if (args.size == 1) 
            args(0) match {
              case x: Infix => print("(", x, ")") 
              case _        => print(args(0))
            }
          else printRow(args, "(", ", ", ")")
        case InfixUnApply(Literal(x), name, args) =>
          print(x.escapedStringValue, " ", symName(tree, name), " ")
          if (args.size == 1) print(args(0))
          else printRow(args, "(", ", ", ")")
        case InfixUnApply(qualifier, name, args) =>
          print(qualifier, " ", symName(tree, name), " ")
          if (args.size == 1) 
            args(0) match {
              case x: Infix => print("(", x, ")") 
              case _        => print(args(0))
            }
          else printRow(args, "(", ", ", ")")
          
// SelectFromArray is no longer visible in reflect.internal.
// eliminated until we figure out what we will do with both TreePrinters and
// SelectFromArray.
//          case SelectFromArray(qualifier, name, _) =>
//          print(qualifier); print(".<arr>"); print(symName(tree, name))

        case tree =>
          xprintTree(this, tree)
      }
      if (typesPrinted && tree.isTerm && !tree.isEmpty) {
        print("{", if (tree.tpe eq null) "<null>" else tree.tpe.toString, "}")
      }
    }

    def print(args: Any*): Unit = args foreach {
      case tree: Tree =>
        printPosition(tree)
        printTree(tree)
      case name: Name =>
        print(quotedName(name))
      case arg =>
        out.print(arg.toString)
    }
  }

  /** Hook for extensions */
  def xprintTree(treePrinter: TreePrinter, tree: Tree) =
    treePrinter.print(tree.productPrefix+tree.productIterator.mkString("(", ", ", ")"))

  def newTreePrinter(writer: PrintWriter): TreePrinter = new TreePrinter(writer)
  def newTreePrinter(stream: OutputStream): TreePrinter = newTreePrinter(new PrintWriter(stream))
  def newTreePrinter(): TreePrinter = newTreePrinter(new PrintWriter(ConsoleWriter))
    
  def treeToString(args: Any*): String = {
    val sw = new StringWriter
    val writer = new PrintWriter(sw)
    val printer = newTreePrinter(writer)  
    args.toList match {
      case Nil => //
      case List(x) => printer.print(x)
      case x :: xs =>
        printer.print(x)
        xs foreach { arg =>
          printer.println()
          printer.print(arg) 
        }
    }

    sw.toString
  }
  
  /** A writer that writes to the current Console and
   * is sensitive to replacement of the Console's
   * output stream.
   */
  object ConsoleWriter extends Writer {
    override def write(str: String) { Console.print(str) }

    def write(cbuf: Array[Char], off: Int, len: Int) {
      write(new String(cbuf, off, len))
    }

    def close = { /* do nothing */ }
    def flush = { /* do nothing */ }
  }
}
