use syntax::parser::Parser;

pub mod syntax;

fn main() {
    let mut parser = Parser::new("test/lang/test_hello_world.lim");
    parser.parse()
}
