Literals and Comments
---------------------

### Literals

Literals are the simplest form of `Tree` and are basic foundation of treehugger DSL. Numeric literals, `String`, and symbols are written by wrapping a Scala literal with `LIT()`:

```scala
LIT(1)     // Int
LIT(1L)    // Long
LIT(1.23)  // Double
LIT(1.23F) // Float
LIT('H')   // Char
LIT("H")   // String
LIT('Sym)  // scala.Symbol
```

Boolean literals, `()` and `null` are written as follows:

```scala
TRUE       // true
FALSE      // false
NULL       // null
UNIT       // ()
```

### Comments

Single line comments can be added to an arbitrary tree as follows:

```scala
tree withComment("comments here", ...)
```

In the above, `...` indicates that the method takes a vararg.

For example,

```scala
LIT(2) withComment("comments are useful",
  "only if they provide more info than the code")
```

print as

```scala
// comments are useful
// only if they provide more info than the code
2
```
