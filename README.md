# What is this

This project tests differences between haxe backends, in particular it tests the behaviour of the Std lib.
For now it only tests the java backend vs javascript, but can be extended.
It also contains an alternative Std.hx/StringTools.hx implementation that eliminates differences between backends. The alternative implementations for java are mostly taken from Mozilla's rhino project.

# How do I run it

To run the test you need java, haxe, ghc and cabal.

Compile with

```
$ cabal build
```

Run with

```
$ ./dist/build/haxe-qc/haxe-qc
```

Or try the override with

```
$ ./dist/build/haxe-qc/haxe-qc --use-override
```
