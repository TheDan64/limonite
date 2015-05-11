[![Build Status](https://travis-ci.org/TheDan64/limonite.svg?branch=master)](https://travis-ci.org/TheDan64/limonite)

Limonite
========

This is a compiler for the Limonite programming language.

Limonite is a relatively basic programming language written in rust using LLVM as a backend.

* Uses a custom tokenizer and parser.
* Compiles to LLVM IR.
* Syntax is by no means final. See sample.lim for examples.

## Building
1. Make sure you have installed all the dependencies.
	* Rust (nightly)
	* Cargo
	* git
	* LLVM >= 3.6
	* cmake >= 2.6

2. Download and build Limonite.

    Run the following commands

        git clone https://github.com/TheDan64/limonite.git
        cd limonite
        cargo build
