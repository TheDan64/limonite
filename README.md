[![Build Status](https://travis-ci.org/TheDan64/limonite.svg?branch=master)](https://travis-ci.org/TheDan64/limonite)
[![codecov](https://codecov.io/gh/TheDan64/limonite/branch/master/graph/badge.svg)](https://codecov.io/gh/TheDan64/limonite)

Limonite
========

This is a compiler for the Limonite programming language.

Limonite is a relatively basic programming language written in rust using LLVM as a backend.

* Uses a custom tokenizer and parser.
* Compiles to LLVM IR.
* Syntax is by no means final. See sample.lim for examples.

## Building
1. Make sure you have installed all the dependencies.
    * Rust (Stable/Beta)
    * Cargo
    * git (optional)
    * LLVM == 3.7 (optional)
    * cmake (required for LLVM)

2. Download and build Limonite.

    Run the following commands

        git clone https://github.com/TheDan64/limonite.git
        cd limonite
        cargo build

    Without LLVM dependency

        git clone https://github.com/TheDan64/limonite.git
        cd limonite
        cargo build --no-default-features

## Working Features
* Basic variables w/ basic type inference
* Basic ascii/utf8 print statements
* While loops
* Comparison operators
* Addition, Subtraction, Assignment
* Comments

Example:

```
var s = "Spam the world!"
var i = 10

>> NOTE: The following requires tab-indentation

while i > 0,
    i -= 1
    print(s)
```
