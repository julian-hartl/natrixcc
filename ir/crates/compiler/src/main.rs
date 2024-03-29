use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;
use natrix_back::{
    codegen::{
        register_allocator,
        targets::x86_64,
    },
    emu::Emulator,
};
use natrix_middle::{optimization, FrontBridge, Verifier};
use tracing::debug;
use natrix_back::codegen::isa::{Architecture, Endianness};

use natrix_back::codegen::register_allocator;
use natrix_back::emu::Emulator;
use natrix_middle::{FrontBridge, optimization, Verifier};

#[derive(Parser, Debug)]
#[clap(name = "natrix")]
#[command(version, about)]
struct Args {
    /// The file to compile
    #[arg(value_parser = valid_source_file_extension)]
    source_file: PathBuf,
}

fn valid_source_file_extension(file_path: &str) -> Result<PathBuf, String> {
    let file_path = PathBuf::from(file_path);
    let extension = file_path.extension().ok_or("No file extension")?;
    if extension != "nx" {
        return Err(format!(
            "Invalid file extension: {} (expected .nx)",
            extension.to_string_lossy()
        ));
    }
    Ok(file_path)
}

fn main() -> Result<()> {
    let start = std::time::Instant::now();
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::DEBUG)
        .init();
    let args = Args::parse();
    let file_path = args.source_file;
    let file_contents = std::fs::read_to_string(file_path)?;
    let module = natrix_front::module::parse(&file_contents)
        .map_err(|e| anyhow::anyhow!("Failed to parse module: {}", e))?;
    debug!("Took {:?} to parse module", start.elapsed());
    let start = std::time::Instant::now();
    println!("{:?}", module);
    let mut module = FrontBridge::new().bridge(module);
    println!("{:?}", module);
    for (_, function) in &module.functions {
        let verifier = Verifier::new(function);
        let verify_result = verifier.verify();
        println!("{:?}", verify_result);
        for error in verify_result {
            println!("{error}");
        }
    }
    let mut config = optimization::PipelineConfig::o1();
    config.dead_code_elimination = false;
    // module.optimize(config);
    debug!("Took {:?} to optimize module", start.elapsed());
    let start = std::time::Instant::now();
    println!("{module}");
    let mut x86_mod =
        natrix_back::codegen::machine::module::Builder::<x86_64::Target>::new(&mut module).build();
    x86_mod.run_register_allocator();
    x86_mod.run_register_coalescer();
    x86_mod.remove_fallthrough_jumps();
    x86_mod.expand_pseudo_instructions();
    debug!("Took {:?} to generate x86 module", start.elapsed());
    let start = std::time::Instant::now();
    debug!("{x86_mod}");
    let base_addr = 0x1000;
    let asm_module = x86_mod.assemble(base_addr);
    debug!("Took {:?} to assemble x86 module", start.elapsed());
    let mut emu = Emulator::new(&asm_module);
    let result = emu
        .run_function(x86_mod.functions().next().unwrap().0, &[30000, 20000])
        .unwrap();
    println!("Result: {}", result);
    Ok(())
}
