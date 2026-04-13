use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;

struct State {
    grid: Vec<Vec<char>>,
    visited: HashSet<(usize, usize)>,
    cache: HashMap<(usize, usize), u128>,
    end: usize,
}

/// Sample
/// ```
/// use advent_of_code_2025::{check_day_output, day07::run};
/// check_day_output!(run, "sample", "21", "40");
/// ```
///
/// Actual
/// ```
/// use advent_of_code_2025::{check_day_output, day07::run};
/// check_day_output!(run, "actual", "1562", "24292631346665");
/// ```
pub fn run(file_type: &str) -> Option<(String, String)> {
    if let Ok(contents) = fs::read_to_string(format!("inputs/{}/07.txt", file_type)) {
        let input = contents.lines().collect::<Vec<&str>>();

        let start = input[0].find('S').unwrap();
        let mut state = State {
            grid: input.iter().map(|line| line.chars().collect()).collect(),
            visited: HashSet::new(),
            cache: HashMap::new(),
            end: input.len(),
        };

        let result = move_beam(start, 0, &mut state);

        Some((state.visited.len().to_string(), result.to_string()))
    } else {
        None
    }
}

fn move_beam(x: usize, y: usize, state: &mut State) -> u128 {
    if let Some(cache_val) = state.cache.get(&(x, y)) {
        return *cache_val;
    }

    if y == state.end {
        return 1;
    }

    let result = if state.grid[y][x] == '.' {
        move_beam(x, y + 1, state)
    } else {
        state.visited.insert((x, y));
        [x - 1, x + 1]
            .iter()
            .map(|dx| move_beam(*dx, y + 1, state))
            .sum()
    };

    state.cache.insert((x, y), result);

    result
}
