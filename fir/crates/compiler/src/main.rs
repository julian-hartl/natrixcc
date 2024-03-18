use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;
use tracing::debug;

use firc_back::codegen::register_allocator;
use firc_middle::{FrontBridge, optimization};

#[derive(Parser, Debug)]
#[clap(name = "firc")]
#[command(version, about)]
struct Args {
    /// The file to compile
    #[arg(value_parser = valid_source_file_extension)]
    source_file: PathBuf,
}

fn valid_source_file_extension(file_path: &str) -> Result<PathBuf, String> {
    let file_path = PathBuf::from(file_path);
    let extension = file_path.extension().ok_or("No file extension")?;
    if extension != "fir" {
        return Err(format!("Invalid file extension: {} (expected .fir)", extension.to_string_lossy()));
    }
    Ok(file_path)
}

fn main() -> Result<()> {
    tracing_subscriber::fmt().with_max_level(tracing::Level::DEBUG).init();
    let args = Args::parse();
    let file_path = args.source_file;
    let file_contents = std::fs::read_to_string(file_path)?;
    let module = firc_front::module::parse(&file_contents).map_err(
        |e| anyhow::anyhow!("Failed to parse module: {}", e)
    )?;
    println!("{:?}", module);
    let mut module = FrontBridge::new().bridge(module);
    println!("{:?}", module);
    let mut config = optimization::PipelineConfig::o1();
    config.dead_code_elimination = false;
    // module.optimize(config);
    println!("{module}");
    let mut x86_mod = firc_back::codegen::machine::module::Builder::<firc_back::codegen::isa::x86_64::Backend>::new(&mut module).build();
    x86_mod.run_register_allocator();
    x86_mod.run_register_coalescer();
    x86_mod.expand_pseudo_instructions::<firc_back::codegen::isa::x86_64::Backend>();
    debug!("{x86_mod}");
    x86_mod.assemble();
    Ok(())
}
