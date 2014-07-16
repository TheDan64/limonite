use std::io::File;
use std::io::BufferedReader;

pub mod syntax {
	pub mod lexer;
}

#[test]
fn open_file() {
	let path = Path::new("lang/test_hello_world.lim");
    let file = BufferedReader::new(File::open(&path));

    let mut lexer = syntax::lexer::Lexer::new(file);
    let token = lexer.get_tok();
}