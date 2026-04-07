use priority_queue::PriorityQueue;
use std::cmp::Reverse;
use std::collections::HashMap;
use std::fs;

use itertools::Itertools;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point3d(i64, i64, i64);

impl Point3d {
    // actually, squared distance, but this is fine
    fn distance(&self, other: &Point3d) -> u64 {
        let dx = (self.0 - other.0).pow(2) as u64;
        let dy = (self.1 - other.1).pow(2) as u64;
        let dz = (self.2 - other.2).pow(2) as u64;
        dx + dy + dz
    }
}

pub fn run(file_type: &str) -> Option<(String, String)> {
    if let Ok(contents) = fs::read_to_string(format!("inputs/{}/08.txt", file_type)) {
        let input = &parse_input(&contents);

        let part1_n = match file_type {
            "sample" => 10,
            "actual" => 1000,
            _ => panic!("Unknown file type: {file_type}"),
        };

        let (p1, p2) = solve(input, part1_n);
        Some((p1.to_string(), p2.to_string()))
    } else {
        None
    }
}

fn parse_input(input: &str) -> Vec<Point3d> {
    input
        .lines()
        .map(|line| {
            let mut parts = line.split(',').map(|part| part.trim().parse().unwrap());
            Point3d(
                parts.next().unwrap(),
                parts.next().unwrap(),
                parts.next().unwrap(),
            )
        })
        .collect()
}

fn solve(points: &[Point3d], part1_n: usize) -> (u64, u64) {
    let mut uf: HashMap<Point3d, Point3d> = points.iter().map(|&p| (p, p)).collect();

    let mut edges = build_edges(points);

    let mut circuit_count = points.len();
    let p1 = part_1(&mut uf, &mut edges, part1_n, &mut circuit_count);
    let p2 = part_2(&mut uf, &mut edges, &mut circuit_count);

    (p1, p2)
}

fn build_edges(points: &[Point3d]) -> PriorityQueue<(Point3d, Point3d), Reverse<u64>> {
    let mut edges = PriorityQueue::new();

    for (i, &p1) in points.iter().enumerate() {
        for &p2 in &points[i + 1..] {
            if p1 != p2 {
                let d = p1.distance(&p2);
                const MAX_DISTANCE: u64 = (15000 as u64).pow(2);
                if d < MAX_DISTANCE {
                    edges.push((p1, p2), Reverse(d));
                }
            }
        }
    }
    edges
}

fn union_find(uf: &mut HashMap<Point3d, Point3d>, i: Point3d) -> Point3d {
    let j = *uf.get(&i).unwrap();

    if j == i {
        return i;
    }

    let r = union_find(uf, j);
    uf.insert(i, r);
    r
}

fn mix(uf: &mut HashMap<Point3d, Point3d>, i: Point3d, j: Point3d) {
    let ri = union_find(uf, i);
    let rj = union_find(uf, j);

    uf.insert(ri, rj);
}

fn part_1(
    uf: &mut HashMap<Point3d, Point3d>,
    edges: &mut PriorityQueue<(Point3d, Point3d), Reverse<u64>>,
    n: usize,
    circuit_count: &mut usize,
) -> u64 {
    for _ in 0..n {
        let ((p1, p2), _) = edges.pop().unwrap();

        let r1 = union_find(uf, p1);
        let r2 = union_find(uf, p2);

        if r1 != r2 {
            mix(uf, r1, r2);
            *circuit_count -= 1;
        }
    }

    for i in uf.keys().cloned().collect::<Vec<_>>() {
        union_find(uf, i);
    }

    let mut counts = HashMap::new();
    for v in uf.values() {
        *counts.entry(*v).or_insert(0) += 1;
    }

    counts.values().copied().sorted().rev().take(3).product()
}

fn part_2(
    uf: &mut HashMap<Point3d, Point3d>,
    edges: &mut PriorityQueue<(Point3d, Point3d), Reverse<u64>>,
    circuit_count: &mut usize,
) -> u64 {
    let mut result = 0;
    while *circuit_count > 1 {
        let ((p1, p2), _) = edges.pop().unwrap();
        result = (p1.0 * p2.0) as u64;

        let r1 = union_find(uf, p1);
        let r2 = union_find(uf, p2);

        if r1 != r2 {
            mix(uf, r1, r2);
            *circuit_count -= 1;
        }
    }

    result
}
