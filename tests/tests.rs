extern crate limonite;

#[macro_use]
mod utils;
mod test_lexer;
#[cfg(feature="llvm-backend")]
mod test_llvm_codegen;
mod test_parser;
mod test_type_checker;
