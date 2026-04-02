use std::fs;

pub fn run(file_type: &str) {
    let contents = fs::read_to_string(format!("inputs/{}/03.txt", file_type))
        .expect("Should have been able to read the file");

    let input = &parse_input(&contents);

    println!("Day  3 {}:", file_type);
    println!("  Part 1: {}", solve(input, 2));
    println!("  Part 2: {}", solve(input, 12));
    println!();
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
        .map(|bank| make_best(length, bank) as u64)
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
