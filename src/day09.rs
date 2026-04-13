use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::fs;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: i64,
    y: i64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Rectangle {
    p1: Point,
    p2: Point,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Candidate {
    area: i64,
    rect: Rectangle,
}

impl Ord for Candidate {
    fn cmp(&self, other: &Self) -> Ordering {
        (
            self.area,
            self.rect.p1.x,
            self.rect.p1.y,
            self.rect.p2.x,
            self.rect.p2.y,
        )
            .cmp(&(
                other.area,
                other.rect.p1.x,
                other.rect.p1.y,
                other.rect.p2.x,
                other.rect.p2.y,
            ))
    }
}

impl PartialOrd for Candidate {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Point {
    fn rect(self, other: Point) -> Rectangle {
        Rectangle::new(self, other)
    }
}

impl Rectangle {
    fn new(p1: Point, p2: Point) -> Self {
        let (xmin, xmax) = if p1.x < p2.x {
            (p1.x, p2.x)
        } else {
            (p2.x, p1.x)
        };
        let (ymin, ymax) = if p1.y < p2.y {
            (p1.y, p2.y)
        } else {
            (p2.y, p1.y)
        };

        let p1 = Point { x: xmin, y: ymin };
        let p2 = Point { x: xmax, y: ymax };
        Self { p1, p2 }
    }

    fn area(&self) -> i64 {
        let x = (self.p2.x - self.p1.x).abs() + 1;
        let y = (self.p2.y - self.p1.y).abs() + 1;
        x * y
    }
}

/// Sample
/// ```
/// use advent_of_code_2025::{check_day_output, day09::run};
/// check_day_output!(run, "sample", "50", "24");
/// ```
///
/// Actual
/// ```
/// use advent_of_code_2025::{check_day_output, day09::run};
/// check_day_output!(run, "actual", "4777409595", "1473551379");
/// ```
pub fn run(file_type: &str) -> Option<(String, String)> {
    if let Ok(contents) = fs::read_to_string(format!("inputs/{}/09.txt", file_type)) {
        let (line_segments, mut pq) = parse_input(&contents);

        let p1 = part_1(&mut pq);
        let p2 = part_2(&line_segments, &mut pq);

        Some((p1.to_string(), p2.to_string()))
    } else {
        None
    }
}

fn parse_input(input: &str) -> (Vec<Rectangle>, BinaryHeap<Candidate>) {
    let points = input
        .lines()
        .map(|line| {
            let mut parts = line.split(",");
            let x = parts.next().unwrap().parse::<i64>().unwrap();
            let y = parts.next().unwrap().parse::<i64>().unwrap();
            Point { x, y }
        })
        .collect::<Vec<Point>>();

    let ls = make_line_segments(&points);
    let pq = make_rectangle_pq(&points);

    (ls, pq)
}

fn make_line_segments(points: &[Point]) -> Vec<Rectangle> {
    let mut line_segments = Vec::with_capacity(points.len());
    for pair in points.windows(2) {
        line_segments.push(pair[0].rect(pair[1]));
    }

    if let (Some(&first), Some(&last)) = (points.first(), points.last()) {
        line_segments.push(last.rect(first));
    }

    line_segments
}

fn make_rectangle_pq(points: &[Point]) -> BinaryHeap<Candidate> {
    let n = points.len();
    let pair_count = n.saturating_mul(n.saturating_sub(1)) / 2;
    let mut pq = BinaryHeap::with_capacity(pair_count);

    for (i, p1) in points.iter().enumerate() {
        for p2 in points.iter().skip(i + 1) {
            let rect = p1.rect(*p2);
            pq.push(Candidate {
                area: rect.area(),
                rect,
            });
        }
    }
    pq
}

fn part_1(pq: &mut BinaryHeap<Candidate>) -> i64 {
    pq.pop().unwrap().area
}

fn part_2(line_segments: &[Rectangle], pq: &mut BinaryHeap<Candidate>) -> i64 {
    loop {
        let candidate = pq.pop().unwrap();
        if is_valid_rectangle(&candidate.rect, line_segments) {
            return candidate.area;
        }
    }
}

fn is_valid_rectangle(rect: &Rectangle, line_segments: &[Rectangle]) -> bool {
    for Rectangle { p1, p2 } in line_segments {
        if p2.x > rect.p1.x && rect.p2.x > p1.x && p2.y > rect.p1.y && rect.p2.y > p1.y {
            return false;
        }
    }

    true
}
