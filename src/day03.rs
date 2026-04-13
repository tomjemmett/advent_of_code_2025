use std::fs;

/// Sample
/// ```
/// use advent_of_code_2025::{check_day_output, day03::run};
/// check_day_output!(run, "sample", "357", "3121910778619");
/// ```
///
/// Actual
/// ```
/// use advent_of_code_2025::{check_day_output, day03::run};
/// check_day_output!(run, "actual", "17085", "169408143086082");
/// ```
pub fn run(file_type: &str) -> Option<(String, String)> {
    let contents = fs::read_to_string(format!("inputs/{}/03.txt", file_type))
        .expect("Should have been able to read the file");

    let input = &parse_input(&contents);

    Some((solve(input, 2).to_string(), solve(input, 12).to_string()))
}

fn parse_input(input: &str) -> Vec<Vec<u64>> {
    input
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).expect("Should be able to parse digit") as u64)
                .collect::<Vec<u64>>()
        })
        .collect::<Vec<Vec<u64>>>()
}

fn solve(input: &Vec<Vec<u64>>, length: usize) -> u64 {
    input
        .iter()
        .map(|bank| make_best(length, bank))
        .sum()
}

fn make_best(length: usize, vals: &Vec<u64>) -> u64 {
    let mut to_remove = vals.len() - length;
    let mut best_digits: Vec<u64> = Vec::with_capacity(vals.len());

    for &digit in vals {
        while to_remove > 0
            && best_digits
                .last()
                .is_some_and(|&last_digit| last_digit < digit)
        {
            best_digits.pop();
            to_remove -= 1;
        }

        best_digits.push(digit);
    }

    best_digits.truncate(length);

    best_digits
        .into_iter()
        .fold(0_u64, |acc, digit| acc * 10 + digit)
}
