Nihil
---

>Latin: **nihil** *n* (indeclinable)
>	* indefinite nothing 

Nihil is a small programming language that compiles to C. It is not intended as a *replacement* for C, but rather as a micro-language to add the features I think c most needs.

At the moment the language is *somewhat* usable, it correctly parses and outputs basic programs, but does no type/scope checking. The following items still need to be done:

* Type/scope checking
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

See data/test.nh for a syntax example

### License

>Copyright (c) 2018 Hunter Brady
>
>Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
>
>The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
>
>THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
