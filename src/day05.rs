use std::fs;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Interval {
    start: u64,
    end: u64,
}

impl Interval {
    fn new(start: u64, end: u64) -> Self {
        Self { start, end }
    }

    fn length(&self) -> u64 {
        self.end - self.start + 1
    }
}

/// Sample
/// ```
/// use advent_of_code_2025::{check_day_output, day05::run};
/// check_day_output!(run, "sample", "3", "14");
/// ```
///
/// Actual
/// ```
/// use advent_of_code_2025::{check_day_output, day05::run};
/// check_day_output!(run, "actual", "862", "357907198933892");
/// ```
pub fn run(file_type: &str) -> Option<(String, String)> {
    if let Ok(contents) = fs::read_to_string(format!("inputs/{}/05.txt", file_type)) {
        let input = parse_input(&contents);
        let p1 = part1(&input);
        let p2 = part2(&input);

        Some((p1.to_string(), p2.to_string()))
    } else {
        None
    }
}

fn parse_input(input: &str) -> (Vec<Interval>, Vec<u64>) {
    let mut input = input.split("\n\n");

    let mut intervals = input
        .next()
        .unwrap()
        .lines()
        .map(|line| {
            let mut parts = line.split('-');
            let start = parts.next().unwrap().parse::<u64>().unwrap();
            let end = parts.next().unwrap().parse::<u64>().unwrap();

            Interval::new(start, end)
        })
        .collect::<Vec<Interval>>();

    intervals.sort();
    reduce_intervals(&mut intervals);

    let values = input
        .next()
        .unwrap()
        .lines()
        .map(|line| line.parse::<u64>().unwrap())
        .collect();

    (intervals, values)
}

fn reduce_intervals(intervals: &mut Vec<Interval>) {
    if intervals.len() <= 1 {
        return;
    }

    let mut i = 0;
    while i < intervals.len() - 1 {
        let a = &intervals[i];
        let b = &intervals[i + 1];

        if a.end >= b.start {
            intervals[i].end = a.end.max(b.end);
            intervals.remove(i + 1);
        } else {
            i += 1;
        }
    }
}

fn part1(input: &(Vec<Interval>, Vec<u64>)) -> u64 {
    let (intervals, values) = input;
    let mut count = 0;
    for v in values {
        if intervals.iter().any(|i| i.start <= *v && i.end >= *v) {
            count += 1;
        }
    }
    count
}

fn part2(input: &(Vec<Interval>, Vec<u64>)) -> u64 {
    let (intervals, _) = input;
    intervals.iter().map(Interval::length).sum()
}
