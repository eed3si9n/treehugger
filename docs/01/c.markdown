---
out:concepts.html
---

  [1]: typelevelexp.html
  [2]: stdtypecon.html

Symbols, Types, and Trees
-------------------------

Having the `scalac` lineage comes with the baggage of having to deal with its data structures.

### Symbol

A _Symbol_ represents an entry in the symbol table, such as values, classes, and type aliases. A compiler would eventually bind all identifiers to a symbol during Typing phase. However, since we are interested in generating the code, the use of symbols are often optional in treehugger.

For example, a unit method declaration may be defined as:

```scala
DEF(sym.get)
```

or:

```scala
DEF("get")
```

Both would yield the same source code at the end. There are several places where the use of symbols are recommended. First, use a built-in symbol if there is one available. Second, consider defining a symbol for repeated reference to a class or a method.

A new symbol may be defined off of an existing symbol as follows:

```scala
object sym {
  val BasicIntQueue = RootClass.newClass("BasicIntQueue")
  val buf = BasicIntQueue.newValue("buf")  
  val A = ArrowAssocClass.newTypeParameter("A")
  val arrow = ArrowAssocClass.newMethod("->")
  val B = arrow.newTypeParameter("B")
  val T = BasicIntQueue.newAliasType("T")
}
```

Defining symbols for every identifiers would double the size of the code, and would likely make the experience of writing it cumbersome.

### Type

A _Type_ represents a Scala type. Unlike symbols, type information cannot be supplied as `String`. However, `ClassSymbol`s and `TypeSymbol`s can automatically be promoted to a `Type` and many of the built-in symbols represent built-in classes.

```scala
VAL("foo", IntClass)
```

In the above code, `IntClass` is a symbol, but it is automatically promoted to a `Type`. Similarly, a string can also be promoted to a `Type`.

```scala
VAL("foo", "Int")
```

Types can also be created using [type-level expressions][1] and [built-in type constructors][2]:

```scala
TYPE_ARRAY(StringClass)
TYPE_REF(REF("board") DOT "Coord")
```

### Tree

A _Tree_ represents a node in a Scala AST. It could be a simple expression such as a literal, or a combination of other trees.

An expression in treehugger DSL eventually evaluates to a `Tree` value.
