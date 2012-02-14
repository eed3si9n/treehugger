---
out:turn.html
---

Turning the table
-----------------

Because our end goal is to get Scala source code, we reverse the flow of `scalac` to generate the code starting from AST.

Scala 2.10 adds Reflection API, which allows the user peak into AST for a given code. It also includes code that turns AST into Scala source code. All we need now is to generate an AST.

As part of `scalac`, there's also a trait called [TreeDSL][1], which can describe AST in code-like way. With both combined seems as though we have everything we need.

Unfortunately that is not the case. Because `scalac` is focused towards Java bytecode generation, the parser expands syntax sugars such as `for` loops and infix method applications. In other words, AST contains one of `map`, `flatMap`, `foreach` method in place of a `for` loop. TreeDSL too is missing a few things, for example, an ability to define classes or objects.

treehugger forks `scalac`'s code base and extends the AST and TreeDSL so all legal Scala code can be described using the DSL (the only exception is XML literals).

  [1]: https://github.com/scala/scala/blob/c40be502a2ed4ef34eff726836b76ed13b03da78/src/compiler/scala/tools/nsc/ast/TreeDSL.scala
