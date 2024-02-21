# Ælisp-C

An implementation of Lisp whose syntax resembles a blend Emacs Lisp's and Scheme's, implemented in C using a Flex/Bison grammar with a [bestline](https://github.com/jart/bestline) based REPL with just enough built-in OS interaction functions to do a little useful work, a pretty decent set of unit tests by way [acutest](https://github.com/mity/acutest) and a set of in-language unit tests as part of its standard library (try `(require 'tests)`.

Most of the completed feeatures work pretty well and pass their tests, but garbage collection hasn't been implemented yet (soon, I hope), so your programs won't live forever.

Some MacOS-specific features are used to resolve certain paths, so some (fairly small) adjustments would be necessary to get it running on other OSes.
