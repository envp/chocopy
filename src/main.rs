mod lexer;
mod options;
mod parser;
mod utils;

use std::error::Error;

use lexer::Tokenizer;

fn main() -> Result<(), Box<dyn Error>> {
    let args = options::parse_args();
    for file in args.input_files {
        let content = std::fs::read_to_string(file)?;
        let tokens = Tokenizer::new(&content).map(|tok| {
            if args.dump_tokens {
                eprintln!("{tok:?}");
            }
            tok
        });
        tokens.count();
    }
    Ok(())
}
