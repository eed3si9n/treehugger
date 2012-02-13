Implicits
---------

### Implicit modifier

Implicit values are written using `withFlags(Flags.IMPLICIT)`:

```scala
(DEF("intToRational") withFlags(Flags.IMPLICIT)
  withParams(PARAM("x", IntClass)) := NEW("Rational", REF("x")))
```

This print as:

```scala
implicit def intToRational(x: Int) = new Rational(x)
```

### Implicit parameter

Implicit parameters are also written using `withFlags(Flags.IMPLICIT)`:

```scala
(DEF("greet")
    withParams(PARAM("name", StringClass))
    withParams(PARAM("config", "Config")
      withFlags(Flags.IMPLICIT)) := BLOCK(
  Predef_println APPLY(REF("config") APPLY REF("name"))
))
```

This prints as:

```scala
def greet(name: String)(implicit config: Config) {
  println(config(name))
}
```

### View bounds

View bounds are written by calling `VIEWBOUNDS(typ|"T")` on `TYPEVAR(...)`:

```scala
(DEF("maxList", "T")
  withTypeParams(TYPEVAR("T") VIEWBOUNDS orderedType("T"))
  withParams(PARAM("elements", listType("T"))): Tree)
```

This prints as:

```scala
def maxList[T <% Ordered[T]](elements: List[T]): T
```

### Context bounds

Context bounds are written by calling `CONTEXTBOUNDS(typ|"T")` on `TYPEVAR(...)`:

```scala
(DEF("put", UnitClass)
  withTypeParams(TYPEVAR(sym.A) CONTEXTBOUNDS FullManifestClass)
  withParams(PARAM("x", sym.A)): Tree)
```

This prints as:

```scala
def put[A : Manifest](x: A): Unit
```
