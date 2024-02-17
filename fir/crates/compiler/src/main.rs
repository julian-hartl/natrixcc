use std::path::PathBuf;
use clap::Parser;
use firc_core;
use anyhow::Result;
#[derive(Parser, Debug)]
#[clap(name = "firc")]
#[command(version, about)]
struct Args {
    /// The file to compile
    #[arg(value_parser = valid_source_file_extension)]
    source_file: PathBuf
}

fn valid_source_file_extension(file_path: &str) -> Result<PathBuf, String> {
    let file_path = PathBuf::from(file_path);
    let extension = file_path.extension().ok_or("No file extension")?;
    if extension != "fir" {
        return Err(format!("Invalid file extension: {} (expected .fir)", extension.to_string_lossy()));
    }
    Ok(file_path)
}

fn main() -> Result<()>{
    let args = Args::parse();
    let file_path = args.source_file;
    let file_contents = std::fs::read_to_string(file_path)?;
    let module = firc_front::module::parse(&file_contents).map_err(
        |e| anyhow::anyhow!("Failed to parse module: {}", e)
    )?;
    println!("{:?}", module);
    Ok(())
}
