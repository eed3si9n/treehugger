Forest
------

```scala
import treehugger.forest._
import definitions._
import treehuggerDSL._

val tree: Tree = Predef_println APPLY LIT("Hello, world!")

println(treeToString(tree))
```

The entire treehugger system is bundled up as `treehugger.Forest` class. The package object for `treehugger` defines an instance of `Forest` called `forest` for convenience.  Under the `forest`, `definitions` object defines built-in symbols and `treehuggerDSL` object defines the DSL.

In the above code, `object sym` defines optional symbols. By wrapping in `sym` we can avoid conflicting with the real `println` function. Then, the line defining `val tree: Tree` is an example of treehugger DSL.

Finally, `forest` defines `treeToString` method to convert AST into a `String`:

```scala
def treeToString(args: Any*): String
```

`treesToString` takes a vararg of `Any`, and pretty prints `Tree` as Scala source code and everything else using `toString` with a new line in between.
