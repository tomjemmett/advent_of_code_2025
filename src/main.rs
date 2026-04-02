use std::env;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;

enum InputKind {
    Both,
    Sample,
    Actual,
}

fn main() {
    let args: Vec<String> = env::args().collect();

    // check for a named argument --actual or --sample
    // if either is present, only run that one, otherwise run both
    let mut input_kind = InputKind::Both;
    let mut day_arg: Option<u32> = None;

    for arg in args.iter().skip(1) {
        match arg.as_str() {
            "--sample" => {
                if matches!(input_kind, InputKind::Actual) {
                    panic!("Cannot pass both --sample and --actual");
                }
                input_kind = InputKind::Sample;
            }
            "--actual" => {
                if matches!(input_kind, InputKind::Sample) {
                    panic!("Cannot pass both --sample and --actual");
                }
                input_kind = InputKind::Actual;
            }
            _ if arg.starts_with("--") => panic!("Unknown flag: {arg}"),
            _ => {
                if day_arg.is_some() {
                    panic!("Only one day number is supported");
                }
                day_arg = Some(arg.parse().expect("Should be able to parse day number"));
            }
        }
    }

    if let Some(day) = day_arg {
        run(day, &input_kind);
    } else {
        (1..=12).for_each(|day| run(day, &input_kind));
    }
}

fn run(d: u32, input_kind: &InputKind) {
    let dayfn = match d {
        1 => day01::run,
        2 => day02::run,
        3 => day03::run,
        4 => day04::run,
        5 => day05::run,
        6 => day06::run,
        7 => day07::run,
        8 => day08::run,
        9 => day09::run,
        10 => day10::run,
        11 => day11::run,
        12 => day12::run,
        _ => panic!("Invalid day number"),
    };

    match input_kind {
        InputKind::Both => {
            dayfn("sample");
            dayfn("actual");
        }
        InputKind::Sample => dayfn("sample"),
        InputKind::Actual => dayfn("actual"),
    }
}
