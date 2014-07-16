use syntax::parser::Parser;

pub mod syntax;

fn main() {
    let mut parser = Parser::new("tests/1_HelloWorld.lim");
}
