use syntax::parser::Parser;

pub mod syntax;

fn main() {
    let mut parser = Parser::new("tests/lang/test_hello_world.lim");
    parser.parse()
}
