#![feature(return_position_impl_trait_in_trait, never_type, option_as_slice)]

use clap::Parser;

mod compile;

#[derive(clap::Parser)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand)]
enum Command {
    Ct(compile::Command),
}

fn main() {
    let args = Args::parse();

    match args.command {
        Command::Ct(command) => command.run(compile::DefaultLoaderProvider, &mut std::io::stdout()),
    };
}
