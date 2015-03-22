use std::str::FromStr;

#[derive(Copy, Debug, PartialEq, Clone)]
pub enum Keywords {
    And,
    Or,
    Not,
    Is,
    Class,
    Function,
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
    type Err = ();
    
    fn from_str(s: &str) -> Result<Keywords, ()> {
        match s {
            "and"      => Ok(Keywords::And),
            "or"       => Ok(Keywords::Or),
            "not"      => Ok(Keywords::Not),
            "is"       => Ok(Keywords::Is),
            "class"    => Ok(Keywords::Class),
            "fn"       => Ok(Keywords::Function),
            "var"      => Ok(Keywords::Var),
            "def"      => Ok(Keywords::Def),
            "assert"   => Ok(Keywords::Assert),
            "else"     => Ok(Keywords::Else),
            "if"       => Ok(Keywords::If),
            "for"      => Ok(Keywords::For),
            "while"    => Ok(Keywords::While),
            "return"   => Ok(Keywords::Return),
            "equals"   => Ok(Keywords::Equals),
            "use"      => Ok(Keywords::Use),
            "from"     => Ok(Keywords::From),
            "as"       => Ok(Keywords::As),
            "when"     => Ok(Keywords::When),
            "throws"   => Ok(Keywords::Throws),
            "break"    => Ok(Keywords::Break),
            "continue" => Ok(Keywords::Continue),
            "pass"     => Ok(Keywords::Pass),
            "print"    => Ok(Keywords::Print),
            _          => Err(())
        }
    }
}
