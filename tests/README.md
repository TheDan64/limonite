Tests
=====

This directory contains unit tests for testing the compiler in parts and as a whole.
The subdirectory, lang, contains scripts in our language to test incremental builds of the compiler against valid syntax.

| Test                        | Description                                    |
| --------------------------- | ---------------------------------------------- |
| `test_lexer.rs`             | Tests the first component of the compiler      |
| `test_parser.rs`            | Tests the second component of the compiler     |
| `lang/test_hello_world.lim` | Tests the most basic statement in any language |