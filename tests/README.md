tests
=====

This directory will contain unit tests for testing the the compiler in parts and as a whole.
The subdirectory, lang, will contain scripts in our language to test incremental builds of the compiler against valid syntax.

| Test                        | Description                                    |
| --------------------------- | ---------------------------------------------- |
| `test_lexer.rs`             | Tests the first component of the compiler      |
| `test_parser.rs`            | Tests the second components of the compiler    |
| `lang/test_hello_world.lim` | Tests the most basic statement in any language |