use std::str::FromStr;

#[derive(Copy, Debug, PartialEq, Clone)]
pub enum Keyword {
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
    Pass
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Keyword, ()> {
        match s {
            "and"      => Ok(Keyword::And),
            "or"       => Ok(Keyword::Or),
            "not"      => Ok(Keyword::Not),
            "is"       => Ok(Keyword::Is),
            "class"    => Ok(Keyword::Class),
            "fn"       => Ok(Keyword::Function),
            "var"      => Ok(Keyword::Var),
            "def"      => Ok(Keyword::Def),
            "assert"   => Ok(Keyword::Assert),
            "else"     => Ok(Keyword::Else),
            "if"       => Ok(Keyword::If),
            "for"      => Ok(Keyword::For),
            "while"    => Ok(Keyword::While),
            "return"   => Ok(Keyword::Return),
            "equals"   => Ok(Keyword::Equals),
            "use"      => Ok(Keyword::Use),
            "from"     => Ok(Keyword::From),
            "as"       => Ok(Keyword::As),
            "when"     => Ok(Keyword::When),
            "throws"   => Ok(Keyword::Throws),
            "break"    => Ok(Keyword::Break),
            "continue" => Ok(Keyword::Continue),
            "pass"     => Ok(Keyword::Pass),
            _          => Err(())
        }
    }
}
