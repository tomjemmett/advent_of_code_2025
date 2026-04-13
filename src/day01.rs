use std::fs;

/// Sample
/// ```
/// use advent_of_code_2025::{check_day_output, day01::run};
/// check_day_output!(run, "sample", "3", "6");
/// ```
///
/// Actual
/// ```
/// use advent_of_code_2025::{check_day_output, day01::run};
/// check_day_output!(run, "actual", "1036", "6228");
/// ```
pub fn run(file_type: &str) -> Option<(String, String)> {
    let contents = fs::read_to_string(format!("inputs/{}/01.txt", file_type))
        .expect("Should have been able to read the file");

    let input = &parse_input(&contents);

    Some((part1(input).to_string(), part2(input).to_string()))
}

fn parse_input(input: &str) -> Vec<(&str, u32)> {
    // the input is a list of L/R, followed by a number of steps, e.g. "R2, L3", separated by newlines
    // if it's an L, the number is a negative number of steps, if it's an R, the number is a positive number of steps
    input
        .lines()
        .map(|line| {
            let (dir, steps) = line.split_at(1);
            let steps: u32 = steps.trim().parse().expect("Should be able to parse steps");
            (dir, steps)
        })
        .collect()
}

fn part1(input: &Vec<(&str, u32)>) -> i32 {
    let mut pos: i32 = 50;
    let mut ctr = 0;
    for (dir, steps) in input {
        let step: i32 = match *dir {
            "L" => -(*steps as i32),
            "R" => *steps as i32,
            _ => panic!("Invalid direction"),
        };
        pos = (pos + step).rem_euclid(100_i32);

        ctr += if pos == 0 { 1 } else { 0 };
    }
    ctr
}

fn part2(input: &Vec<(&str, u32)>) -> i32 {
    let mut pos: i32 = 50;
    let mut ctr = 0;
    for (dir, steps) in input {
        let step: i32 = match *dir {
            "L" => -1_i32,
            "R" => 1_i32,
            _ => panic!("Invalid direction"),
        };

        for _ in 0..*steps {
            pos = (pos + step).rem_euclid(100_i32);

            ctr += if pos == 0 { 1 } else { 0 };
        }
    }
    ctr
}
