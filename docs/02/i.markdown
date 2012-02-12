Annotations
-----------

Annotations are applied to definitions or declarations, types, or expressions.

### Declaration annotations

Annotations are applied to declarations using `withAnnots(annot, ...)`. This takes vararg of `AnnotationInfo`, which is created using `ANNOT(typ|"C", arg, ...)`.

```scala
CLASSDEF("C")
    withAnnots(ANNOT(SerializableAttr)) := BLOCK(
  DEF("get", IntClass) := LIT(0)
)
```

This prints as:

```scala
@scala.annotation.serializable class C {
  def get: Int = 0
}
```

### Type annotations

Type annotations are written by calling `withAnnots(annot, ...)` on `TYPE(...)`.

```scala
val annot = ANNOT(SpecializedClass, REF(IntClass))

TRAITDEF("Function0")
    withTypeParams(TYPE("T") withAnnots(annot)) := BLOCK(
  DEF("apply", "T")
)
```

This prints as:

```scala
trait Function0[@specialized(Int) T] {
  def apply: T
}
```

### Annotated expressions

Annotated expressions are written as `tree withAnnots(annot, ...)`:

```scala
REF("e") withAnnots(ANNOT(UncheckedClass))
```

This prints as:

```scala
(e: @unchecked)
```
