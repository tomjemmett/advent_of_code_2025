use std::collections::HashMap;
use std::fs;

/// Sample
/// ```
/// use advent_of_code_2025::{check_day_output, day11::run};
/// check_day_output!(run, "sample", "1", "2");
/// ```
///
/// Actual
/// ```
/// use advent_of_code_2025::{check_day_output, day11::run};
/// check_day_output!(run, "actual", "428", "331468292364745");
/// ```
pub fn run(file_type: &str) -> Option<(String, String)> {
    if let Ok(contents) = fs::read_to_string(format!("inputs/{}/11.txt", file_type)) {
        let input = parse_input(&contents);

        let mut memo = HashMap::new();
        let p1 = search(&input, "you", 0, &mut memo);
        let p2 = search(&input, "svr", 2, &mut memo);

        Some((p1.to_string(), p2.to_string()))
    } else {
        None
    }
}

fn parse_input(input: &str) -> HashMap<String, Vec<String>> {
    let mut map = HashMap::new();
    for line in input.lines() {
        if let Some((key, value)) = line.split_once(": ") {
            let values = value.split_whitespace().map(|s| s.to_string()).collect();
            map.insert(key.to_string(), values);
        }
    }
    map
}

// We memoize using borrowed node names (`&str`) to avoid allocating `String`s.
// `'a` ties `graph`, `key`, and memo keys together so stored references stay valid.
fn search<'a>(
    graph: &'a HashMap<String, Vec<String>>,
    key: &'a str,
    found: i8,
    memo: &mut HashMap<(&'a str, i8), u64>,
) -> u64 {
    let memo_key = (key, found);
    if let Some(&result) = memo.get(&memo_key) {
        return result;
    }

    if key == "out" {
        return (found == 0) as u64;
    }

    let next_found = if key == "dac" || key == "fft" {
        found - 1
    } else {
        found
    };
    let result = graph.get(key).map_or(0, |neighbors| {
        neighbors
            .iter()
            .map(|n| search(graph, n, next_found, memo))
            .sum()
    });
    memo.insert(memo_key, result);
    result
}
