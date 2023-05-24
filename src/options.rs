use clap::Parser;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct ProgramArguments {
    /// Output tokens as they are encountered
    #[arg(short, long, default_value = "false")]
    pub dump_tokens: bool,

    /// Input files to the driver
    #[arg()]
    pub input_files: Vec<PathBuf>,
}

pub fn parse_args() -> ProgramArguments {
    ProgramArguments::parse()
}
