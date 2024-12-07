use std::fs::{create_dir_all, File};
use std::io::Write;
use std::process::Command;

use chrono::Datelike;
use clap::Parser;

/// Initialize a folder, input and starter code for an Advent of Code puzzle.
#[derive(Parser)]
struct Cli {
    /// Which year to initialize for. Defaults to the current year.
    #[clap(short, long)]
    year: Option<i32>,
    /// Which day to initialize for. Default to the current day.
    #[clap(short, long)]
    day: Option<u32>,
}

fn main() {
    let args = Cli::parse();
    let path = std::env::current_dir().unwrap();
    let date = chrono::Local::now();
    let year = args.year.unwrap_or(date.year());
    let day = args.day.unwrap_or(date.day());

    if !(2015..=date.year()).contains(&year) {
        eprintln!("Year must be between 2015 and {}", date.year());
        std::process::exit(1);
    }
    if !(1..=25).contains(&day) {
        eprintln!("Day must be between 1 and 25");
        std::process::exit(1);
    }

    let dir = path
        .join(format!("aoc-{year}"))
        .join(format!("day-{day:02}"));

    if !dir.parent().unwrap().exists() {
        create_dir_all(dir.parent().unwrap()).unwrap();
    }

    if !dir.exists() {
        Command::new("cargo")
            .arg("new")
            .arg("--bin")
            .arg("--name")
            .arg(format!("day-{day:02}"))
            .arg(format!("day-{day:02}"))
            .current_dir(dir.parent().unwrap())
            .spawn()
            .unwrap()
            .wait()
            .unwrap();
        Command::new("cargo")
            .arg("add")
            .arg("aocd")
            .current_dir(&dir)
            .spawn()
            .unwrap()
            .wait()
            .unwrap();

        File::create(dir.join("src/main.rs"))
            .unwrap()
            .write_all(
            &format!(
                    "use aocd::prelude::*;\n\n#[aocd({year}, {day})]\nfn main() {{\n    let input = input!();\n    println!(\"{{input}}\");\n}}\n"
                ).into_bytes())
            .unwrap();
    }
    if let Ok(mut readme) = File::create_new(dir.join("README.md")) {
        readme
            .write_all(
                &format!(
            "# [Advent of Code {year} - Day {day}](https://adventofcode.com/{year}/day/{day})\n"
        )
                .into_bytes(),
            )
            .unwrap();
    }
    if let Ok(mut gitignore) = File::create_new(dir.join(".gitignore")) {
        gitignore
            .write_all(b"target\ninput.txt\ntest.txt\n")
            .unwrap();
    }
    if let Ok(input) = File::create_new(dir.join("input.txt")) {
        Command::new("cargo")
            .arg("run")
            .current_dir(dir)
            .stdout(std::process::Stdio::from(input))
            .spawn()
            .unwrap()
            .wait()
            .unwrap();
    }
}
