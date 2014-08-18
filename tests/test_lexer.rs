use std::io::File;
use std::io::BufferedReader;
//use syntax::core::tokens;
//use src::syntax::lexer::Lexer;
//#[path="../src/syntax/lexer.rs"] // temp until I can figure out the above
//#[path="../src/syntax/core/tokens.rs"]

use limonite::syntax;
use syntax;

#[test]
fn test_hello_world() {
    let path = Path::new("tests/lang/test_hello_world.lim");
    let file = BufferedReader::new(File::open(&path));
    let lexer = Lexer::new(file);
    fail!("QEW");
    loop {
        match lexer.get_tok() {
            tokens::EOF => break,
            _ => continue
        }
    }
    fail!("ASD");
}
