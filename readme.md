Nihil
---

>Latin: **nihil** *n* (indeclinable)
>	* indefinite nothing 

Nihil is a small programming language that compiles to C. It is not intended as a *replacement* for C, but rather as a micro-language to add the features I think c most needs.

At the moment the language is *somewhat* usable, it correctly parses and outputs basic programs with type/scope checking, but not for function signatures. The following items still need to be done:

* Type/scope checking for function signatures
* Better optimization
  * Constant folding (should be easy, values are constant by default)
  * Function inlining
* Support for `struct`, `enum`, and `union`
* Better error reporting
  * ERROR [line:character] is pretty lame
* Operator overloading
* External functions
* Broader tests
  * Just more tests in general, for all language features
* `typedef`?
* Maybe actually use a compiler backend like LLVM instead of compiling to C?

### Examples

See data/example.nh for a syntax example