#![allow(unused_variable)] //temp
#![allow(unused_mut)] //temp

use syntax::parser::Parser;

pub mod syntax;
pub mod tests;

fn main() {
    let mut parser = Parser::new("src/tests/lang/test_hello_world.lim");
}
