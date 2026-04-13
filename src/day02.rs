use std::collections::HashSet;
use std::fs;

/// Sample
/// ```
/// use advent_of_code_2025::{check_day_output, day02::run};
/// check_day_output!(run, "sample", "1227775554", "4174379265");
/// ```
///
/// Actual
/// ```
/// use advent_of_code_2025::{check_day_output, day02::run};
/// check_day_output!(run, "actual", "18893502033", "26202168557");
/// ```
pub fn run(file_type: &str) -> Option<(String, String)> {
    let contents = fs::read_to_string(format!("inputs/{}/02.txt", file_type))
        .expect("Should have been able to read the file");

    let input = &parse_input(&contents);

    Some((part1(input).to_string(), part2(input).to_string()))
}

fn parse_input(input: &str) -> Vec<(u128, u128)> {
    // the input is a list of pairs of numbers, split by "-", e.g. "1-3, 5-7", separated by commas
    input
        .split(',')
        .map(|line| {
            let (first, second) = line.split_once("-").expect("Should be able to split line");
            let first: u128 = first
                .trim()
                .parse()
                .expect("Should be able to parse first number");
            let second: u128 = second
                .trim()
                .parse()
                .expect("Should be able to parse second number");
            (first, second)
        })
        .collect()
}

fn part1(input: &[(u128, u128)]) -> u128 {
    input
        .iter()
        .map(|&range| find_repeats(Mode::Part1, range))
        .sum()
}

fn part2(input: &[(u128, u128)]) -> u128 {
    input
        .iter()
        .map(|&range| find_repeats(Mode::Part2, range))
        .sum()
}

enum Mode {
    Part1,
    Part2,
}

fn find_repeats(mode: Mode, (lo, hi): (u128, u128)) -> u128 {
    let digits = hi.ilog10() + 1;

    let mut unique = HashSet::new();
    let mut sum = 0;

    for d in 1..=digits {
        let max_r = match mode {
            Mode::Part1 => 2,
            Mode::Part2 => (digits / d) + 1,
        };

        for r in 2..=max_r {
            let p10d = 10u128.pow(d);
            let p10dr = 10u128.pow(d * r);

            let f = (p10dr - 1) / (p10d - 1);
            let k_lo = std::cmp::max(lo / f, 10u128.pow(d - 1));
            let k_hi = std::cmp::min(hi / f, p10d - 1);

            if k_lo > k_hi {
                continue;
            }

            for k in k_lo..=k_hi {
                let x = f * k;

                if x >= lo && x <= hi && unique.insert(x) {
                    sum += x;
                }
            }
        }
    }

    sum
}
