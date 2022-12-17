#![allow(dead_code)]

mod lexer;
mod utils;

use std::{error::Error, io::stdin};

const _SOURCE: &str = r##"
def is_zero ( items : [ int ] , idx : int ) -> bool :
    val : int = 0 # Type is explicitly declared
    val = items [ idx ]
    return val == 0

def always_true() -> bool:
    return (not True) == False

def syntax_error() -> bool:
    return not True == False

mylist : [ int ] = None
mylist = [1 , 10 , 999]
print( is_zero( mylist , 1) ) # Prints ’ True ’
"##;

fn main() -> Result<(), Box<dyn Error>> {
    pretty_env_logger::init();

    for line in stdin().lines() {
        let content = line?;
        let lex = lexer::Tokenizer::new(&content);
        for token in lex {
            println!("{token:?}");
        }
    }
    Ok(())
}
