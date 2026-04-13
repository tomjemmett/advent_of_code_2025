use std::fs;

/// Sample
/// ```
/// use advent_of_code_2025::{check_day_output, day06::run};
/// check_day_output!(run, "sample", "4277556", "3263827");
/// ```
///
/// Actual
/// ```
/// use advent_of_code_2025::{check_day_output, day06::run};
/// check_day_output!(run, "actual", "6635273135233", "12542543681221");
/// ```
pub fn run(file_type: &str) -> Option<(String, String)> {
    if let Ok(contents) = fs::read_to_string(format!("inputs/{}/06.txt", file_type)) {
        let input = parse_input(&contents);

        Some((part1(&input).to_string(), part2(&input).to_string()))
    } else {
        None
    }
}

fn parse_input(input: &str) -> Vec<(Vec<String>, char)> {
    let lines: Vec<&str> = input.lines().collect();
    let nums = &lines[..lines.len() - 1];
    let ops = lines[lines.len() - 1]
        .chars()
        .chain(" @".chars())
        .collect::<Vec<char>>();

    let mut res = Vec::new();
    let mut i = 0;

    for (j, op) in ops[1..].iter().enumerate() {
        if *op != ' ' {
            res.push((
                nums.iter()
                    .map(|line| line[i..j].to_string())
                    .collect::<Vec<String>>(),
                ops[i],
            ));
            i = j + 1;
        }
    }

    res
}

fn solve<F, I>(input: &[(Vec<String>, char)], transform: F) -> u128
where
    F: Fn(&[String]) -> I,
    I: IntoIterator<Item = String>,
{
    input.iter().fold(0, |acc, (nums, op)| {
        let values: Vec<u128> = transform(nums)
            .into_iter()
            .map(|num| num.trim().parse::<u128>().unwrap())
            .collect();

        acc + match op {
            '+' => values.iter().sum::<u128>(),
            '*' => values.iter().product::<u128>(),
            _ => panic!("Unknown operator"),
        }
    })
}

fn part1(input: &[(Vec<String>, char)]) -> u128 {
    solve(input, |nums| nums.to_vec())
}

fn part2(input: &[(Vec<String>, char)]) -> u128 {
    solve(input, transpose_strings)
}

fn transpose_strings(v: &[String]) -> Vec<String> {
    if v.is_empty() {
        return vec![];
    }

    let rows = v.len();
    let cols = v[0].len();

    let bytes: Vec<&[u8]> = v.iter().map(|s| s.as_bytes()).collect();

    (0..cols)
        .map(|i| {
            let mut s = String::with_capacity(rows);
            for row in &bytes {
                s.push(row[i] as char);
            }
            s
        })
        .collect()
}
