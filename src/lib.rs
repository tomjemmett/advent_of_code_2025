#[macro_export]
macro_rules! check_day_output {
    ($run_fn:path, $input_kind:expr, $part1:expr, $part2:expr) => {
        assert_eq!(
            $run_fn($input_kind),
            Some(($part1.to_string(), $part2.to_string()))
        );
    };
}

pub mod day01;
pub mod day02;
pub mod day03;
pub mod day04;
pub mod day05;
pub mod day06;
pub mod day07;
pub mod day08;
pub mod day09;
pub mod day10;
pub mod day11;
pub mod day12;
pub mod download_inputs;
