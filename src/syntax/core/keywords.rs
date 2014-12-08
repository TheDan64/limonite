use std::str::FromStr;

#[deriving(Show, PartialEq)]
pub enum Keywords {
    And,
    Or,
    Not,
    Is,
    Class,
    Fn,
    Var,
    Def,
    Assert,
    Else,
    If,
    For,
    While,
    Return,
    Equals,
    Use,
    From,
    As,
    When,
    Throws,
    Break,
    Continue,
    Pass,

    // Print is temporaily a statement until we have functions
    Print
}

impl FromStr for Keywords {
    fn from_str(s: &str) -> Option<Keywords> {
        match s {
            "and"      => Some(Keywords::And),
            "or"       => Some(Keywords::Or),
            "not"      => Some(Keywords::Not),
            "is"       => Some(Keywords::Is),
            "class"    => Some(Keywords::Class),
            "fn"       => Some(Keywords::Fn),
            "var"      => Some(Keywords::Var),
            "def"      => Some(Keywords::Def),
            "assert"   => Some(Keywords::Assert),
            "else"     => Some(Keywords::Else),
            "if"       => Some(Keywords::If),
            "for"      => Some(Keywords::For),
            "while"    => Some(Keywords::While),
            "return"   => Some(Keywords::Return),
            "equals"   => Some(Keywords::Equals),
            "use"      => Some(Keywords::Use),
            "from"     => Some(Keywords::From),
            "as"       => Some(Keywords::As),
            "when"     => Some(Keywords::When),
            "throws"   => Some(Keywords::Throws),
            "break"    => Some(Keywords::Break),
            "continue" => Some(Keywords::Continue),
            "pass"     => Some(Keywords::Pass),
            "print"    => Some(Keywords::Print),
            _          => None
        }
    }
}
