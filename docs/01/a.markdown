---
out: compilers.html
---

Compilers 101
-------------

Let's go over the basic compiler theory, so we have some kind of foundation even if it's overly simplified. A _compiler_ is a program that translates a source code in a language to another, often machine code such as Java bytecode.

The compilers have been written so many times that there are names to each phases of a typical compiler.

### Scan

_Scanning_ phase, or Lexical Analysis, is responsible for throwing out the white space and comments, and recognizing literals, identifiers, and keywords as tokens. At this point, there is no structural check, except for making sure that the comments and string quotations match up.

### Parse

_Parsing_ phase, or Ayntactic Analysis, is responsible for construcuting an abstract syntax tree (AST) from the linear sequence of tokens according to the rules of the grammar of the language.

### Semantic Analysis

_Typing_ phase, or Semantic Analysis, is responsible for adding the symbol table to the syntax tree, by associating variables and references with their definitions, and performing type checking.

### Backend

_Analysis_ and _Optimization_ phase are specific to the language implementation. Some of the common optimizations are  inlining and dead code elimination.

Finally, during _Code Generation_ phase the target language is generated either from AST or from an intermediate structure.
