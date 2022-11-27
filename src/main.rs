mod lexer {
    use logos::{Lexer, Logos, SpannedIter};

    fn lex_integer<'lexer, 'input: 'lexer>(
        lex: &'lexer mut Lexer<'input, RawToken<'input>>,
    ) -> Result<BuiltinValue, std::num::ParseIntError> {
        lex.slice().parse().map(BuiltinValue::Integral)
    }

    #[derive(Debug, PartialEq)]
    pub enum WSKind {
        Tab,
        Space,
        Newline,
    }

    #[derive(Debug, PartialEq)]
    pub enum BuiltinValue {
        Boolean(bool),
        Integral(i32),
    }

    #[derive(Debug, PartialEq)]
    pub enum BuiltinBoolOp {
        Equal,
        NotEqual,
        Lesser,
        Greater,
        LesserEq,
        GreaterEq,
    }

    #[derive(Debug, PartialEq)]
    pub enum BuiltinArithmeticOp {
        Plus,
        Minus,
        Asterisk,
        Slash,
        SlashSlash,
        Remainder,
    }

    #[derive(Logos, Debug, PartialEq)]
    #[logos(subpattern idstart = r"[a-zA-Z_]")]
    #[logos(subpattern digit = r"[0-9]")]
    #[logos(subpattern idpart = r"((?&idstart)|(?&digit))")]
    pub enum RawToken<'input> {
        #[token("(")]
        LParen,
        #[token(")")]
        RParen,
        #[token("[")]
        LBracket,
        #[token("]")]
        RBracket,
        #[token(":")]
        Colon,
        #[token(",")]
        Comma,
        #[token(".")]
        Dot,
        #[token("=")]
        Assign,
        #[token("->")]
        RArrow,

        #[token("==", |_| BuiltinBoolOp::Equal)]
        #[token("!=", |_| BuiltinBoolOp::NotEqual)]
        #[token("<", |_| BuiltinBoolOp::Lesser)]
        #[token(">", |_| BuiltinBoolOp::Greater)]
        #[token("<=", |_| BuiltinBoolOp::LesserEq)]
        #[token(">=", |_| BuiltinBoolOp::GreaterEq)]
        BoolOperator(BuiltinBoolOp),

        #[token("+", |_| BuiltinArithmeticOp::Plus)]
        #[token("-", |_| BuiltinArithmeticOp::Minus)]
        #[token("*", |_| BuiltinArithmeticOp::Asterisk)]
        #[token("/", |_| BuiltinArithmeticOp::Slash)]
        #[token("//", |_| BuiltinArithmeticOp::SlashSlash)]
        #[token("%", |_| BuiltinArithmeticOp::Remainder)]
        ArithmeticOperator(BuiltinArithmeticOp),

        #[token("def", priority = 2)]
        KWDef,
        #[token("return", priority = 2)]
        KWReturn,

        #[token("True", |_| BuiltinValue::Boolean(true))]
        #[token("False", |_| BuiltinValue::Boolean(false))]
        #[regex("(?&digit)+", lex_integer)]
        Constant(BuiltinValue),

        #[regex("(?&idstart)(?&idpart)+", priority = 1, callback = |lex| lex.slice())]
        Ident(&'input str),

        #[token("\t", |_| WSKind::Tab)]
        #[token(" ", |_| WSKind::Space)]
        #[token("\n", |_| WSKind::Newline)]
        Whitespace(WSKind),

        #[regex(r"#.*", callback = |lex| lex.slice())]
        Comment(&'input str),

        #[error]
        Error,
    }

    impl<'input> RawToken<'_> {
        pub fn spanned_iter_from_str(s: &'_ str) -> SpannedIter<'_, RawToken<'_>> {
            RawToken::lexer(s).spanned()
        }
    }
}

mod parser {}

const SOURCE: &'static str = r##"
def is_zero ( items : [ int ] , idx : int ) -> bool :
    val : int = 0 # Type is explicitly declared
    val = items [ idx ]
    return val == 0

# Stray whitespace is fine
        
def always_true() -> bool:
	 return (not True) == False

def syntax_error() -> bool:
    return not True == False

mylist : [ int ] = None
mylist = [1 , 10 , 999]
print( is_zero( mylist , 1) ) # Prints ’ True ’
"##;

fn main() {
    let lex = lexer::RawToken::spanned_iter_from_str(SOURCE);

    for (token, span) in lex {
        println!("{:?} -> {:?}", span, token)
    }
}
