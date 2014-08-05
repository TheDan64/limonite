use syntax::parser::Parser;

pub mod syntax;
pub mod tests;

fn main() {
    let mut parser = Parser::new("tests/lang/test_hello_world.lim");
    parser.parse()
}
