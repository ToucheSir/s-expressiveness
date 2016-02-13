(Yet Another) Lisp
==================

A basic, barebones lisp interpreter in Rust.

This is a port of an existing interpreter in C. The hope is to create a working interpreter that maintains the semantics of the original without deviating too far from idiomatic rust code.

Some goals:
-----------
* Remove all global, mutable state from the original: Rust's ownership semantics should help greatly with this.
* Rely to as few heap allocations as possible in the final version: The original C program made virtually no use of dynamic memory,
and replicating that behaviour would be a good challenge.
* Extended character (utf-8) support: Although not a priority, this would be a nice enhancement over the original, ascii-only implementation
* Embeddability (?): While there's little utility in using such a minimal language, distributing the interpreter as a library would certainly be novel.
