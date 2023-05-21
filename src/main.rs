mod lexer;
mod utils;

use std::error::Error;

use lexer::Tokenizer;

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
    let content = std::fs::read_to_string(std::env::args().nth(1).expect("Needs input file"))?;
    let tokens = Tokenizer::new(&content);
    for token in tokens {
        println!("{token:?}");
    }
    Ok(())
}
