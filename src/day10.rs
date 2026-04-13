use itertools::Itertools;
use nom::{
    IResult, Parser,
    character::complete::newline,
    character::complete::{char, digit1, one_of, space1},
    combinator::{map, map_res},
    multi::{many1, separated_list1},
    sequence::delimited,
};
use rayon::prelude::*;
use std::collections::HashMap;
use std::fs;

#[derive(Debug)]
struct Row {
    lights: Vec<u32>,
    jolts: Vec<u32>,
    button_press_costs: HashMap<Vec<u32>, HashMap<Vec<u32>, u32>>,
}

/// Sample
/// ```
/// use advent_of_code_2025::{check_day_output, day10::run};
/// check_day_output!(run, "sample", "7", "33");
/// ```
///
/// Actual
/// ```
/// use advent_of_code_2025::{check_day_output, day10::run};
/// check_day_output!(run, "actual", "432", "18011");
/// ```
pub fn run(file_type: &str) -> Option<(String, String)> {
    if let Ok(contents) = fs::read_to_string(format!("inputs/{}/10.txt", file_type)) {
        let (_, input) = &parse_input(&contents).unwrap();

        let p1 = solve(input, part_1);
        let p2 = solve(input, part_2);

        Some((p1.to_string(), p2.to_string()))
    } else {
        None
    }
}

fn parse_input(input: &str) -> IResult<&str, Vec<Row>> {
    fn parse_number(input: &str) -> IResult<&str, u32> {
        map_res(digit1, str::parse).parse(input)
    }
    fn parse_light(input: &str) -> IResult<&str, Vec<u32>> {
        delimited(
            char('['),
            map(many1(one_of(".#")), |chars| {
                chars
                    .into_iter()
                    .map(|c| (c == '#') as u32)
                    .collect::<Vec<u32>>()
            }),
            char(']'),
        )
        .parse(input)
    }
    fn parse_button(input: &str) -> IResult<&str, Vec<u32>> {
        delimited(
            char('('),
            map(separated_list1(char(','), parse_number), |nums| {
                nums.into_iter().collect::<Vec<u32>>()
            }),
            char(')'),
        )
        .parse(input)
    }
    fn parse_buttons(input: &str) -> IResult<&str, Vec<Vec<u32>>> {
        separated_list1(space1, parse_button).parse(input)
    }
    fn parse_jolt(input: &str) -> IResult<&str, Vec<u32>> {
        delimited(
            char('{'),
            separated_list1(char(','), parse_number),
            char('}'),
        )
        .parse(input)
    }

    fn parse_row(input: &str) -> IResult<&str, Row> {
        let (input, lights) = parse_light(input)?;
        let (input, _) = space1(input)?;
        let (input, buttons) = parse_buttons(input)?;
        let (input, _) = space1(input)?;
        let (input, jolts) = parse_jolt(input)?;

        let buttons = buttons
            .iter()
            .map(|b| {
                (0..lights.len())
                    .map(|i| b.contains(&(i as u32)) as u32)
                    .collect::<Vec<u32>>()
            })
            .collect::<Vec<Vec<u32>>>();

        let button_press_costs = patterns(&buttons);

        Ok((
            input,
            Row {
                lights,
                jolts,
                button_press_costs,
            },
        ))
    }

    separated_list1(newline, parse_row).parse(input)
}

fn patterns(coeffs: &[Vec<u32>]) -> HashMap<Vec<u32>, HashMap<Vec<u32>, u32>> {
    if coeffs.is_empty() {
        return HashMap::new();
    }

    let num_buttons = coeffs.len();
    let num_variables = coeffs[0].len();
    let mut out: HashMap<Vec<u32>, HashMap<Vec<u32>, u32>> = HashMap::new();

    let mut parity_patterns: Vec<Vec<u32>> = vec![Vec::new()];
    for _ in 0..num_variables {
        let mut next = Vec::with_capacity(parity_patterns.len() * 2);
        for prefix in &parity_patterns {
            let mut with_zero = prefix.clone();
            with_zero.push(0);
            next.push(with_zero);

            let mut with_one = prefix.clone();
            with_one.push(1);
            next.push(with_one);
        }
        parity_patterns = next;
    }
    for parity_pattern in parity_patterns {
        out.insert(parity_pattern, HashMap::new());
    }

    for num_pressed_buttons in 0..=num_buttons {
        for buttons in (0..num_buttons).combinations(num_pressed_buttons) {
            let mut pattern = vec![0u32; num_variables];
            for &button_idx in &buttons {
                for (var_idx, slot) in pattern.iter_mut().enumerate() {
                    *slot += coeffs[button_idx].get(var_idx).copied().unwrap_or(0);
                }
            }
            let parity_pattern = pattern.iter().map(|x| x % 2).collect::<Vec<u32>>();

            out.entry(parity_pattern)
                .or_default()
                .entry(pattern)
                .or_insert(num_pressed_buttons as u32);
        }
    }

    out
}

fn solve(input: &[Row], f: fn(&Row) -> u32) -> u32 {
    input.par_iter().map(f).sum()
}

fn part_1(row: &Row) -> u32 {
    row.button_press_costs[&row.lights]
        .values()
        .min()
        .copied()
        .unwrap_or(0)
}

fn part_2(row: &Row) -> u32 {
    let pattern_costs = &row.button_press_costs;

    fn solve(
        goal: Vec<u32>,
        pattern_costs: &HashMap<Vec<u32>, HashMap<Vec<u32>, u32>>,
        memo: &mut HashMap<Vec<u32>, u32>,
    ) -> u32 {
        if let Some(&cached) = memo.get(&goal) {
            return cached;
        }

        if goal.iter().all(|&x| x == 0) {
            memo.insert(goal, 0);
            return 0;
        }

        let parity_key = goal.iter().map(|i| i % 2).collect::<Vec<u32>>();
        let mut answer = u32::MAX;

        if let Some(pattern_map) = pattern_costs.get(&parity_key) {
            for (pattern, pattern_cost) in pattern_map {
                if pattern.iter().zip(goal.iter()).all(|(i, j)| i <= j) {
                    let new_goal = pattern
                        .iter()
                        .zip(goal.iter())
                        .map(|(i, j)| (j - i) / 2)
                        .collect::<Vec<u32>>();
                    let sub = solve(new_goal, pattern_costs, memo);
                    if sub < answer {
                        let candidate = *pattern_cost + 2 * sub;
                        answer = answer.min(candidate);
                    }
                }
            }
        }

        memo.insert(goal, answer);
        answer
    }

    let mut memo = HashMap::new();
    solve(row.jolts.clone(), pattern_costs, &mut memo)
}
