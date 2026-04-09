use nom::{
    IResult, Parser,
    bytes::complete::take_while1,
    character::complete::{char, line_ending, space0, space1, u8 as parse_u8, u16 as parse_u16},
    combinator::{map, opt},
    multi::{many1, separated_list1},
    sequence::terminated,
};
use std::fs;

type Present = Vec<String>;
type Presents = Vec<Present>;
#[derive(Debug)]
struct Region {
    size: (u16, u16),
    present_counts: Vec<u16>,
}
type Regions = Vec<Region>;

enum CanFit {
    No,
    Possibly,
    Yes,
}

pub fn run(file_type: &str) -> Option<(String, String)> {
    if let Ok(contents) = fs::read_to_string(format!("inputs/{}/12.txt", file_type)) {
        let input = parse_input(&contents);

        Some((part_1(input).to_string(), "No part 2".to_string()))
    } else {
        None
    }
}

fn parse_input(input: &str) -> (Presents, Regions) {
    fn parse_pattern_line(input: &str) -> IResult<&str, String> {
        map(
            terminated(take_while1(|c| c == '#' || c == '.'), line_ending),
            str::to_string,
        )
        .parse(input)
    }

    fn parse_present(input: &str) -> IResult<&str, Present> {
        let (input, _) = terminated(parse_u8, (char(':'), line_ending)).parse(input)?;
        many1(parse_pattern_line).parse(input)
    }

    fn parse_region(input: &str) -> IResult<&str, Region> {
        map(
            (
                parse_u16,
                char('x'),
                parse_u16,
                char(':'),
                space0,
                separated_list1(space1, parse_u16),
            ),
            |(width, _, height, _, _, present_counts)| Region {
                size: (width, height),
                present_counts,
            },
        )
        .parse(input)
    }

    let mut rest = input;
    let mut presents = Vec::new();
    let mut regions = Vec::new();

    loop {
        rest = rest.trim_start_matches(['\n', '\r']);

        if rest.is_empty() {
            break;
        }

        if let Ok((next, present)) = parse_present(rest) {
            presents.push(present);
            rest = next;
            continue;
        }

        if let Ok((next, region)) = terminated(parse_region, opt(line_ending)).parse(rest) {
            regions.push(region);
            rest = next;
            continue;
        }

        break;
    }

    (presents, regions)
}

fn part_1((presents, regions): (Presents, Regions)) -> u32 {
    regions
        .iter()
        .map(|region| match can_fit(&presents, region) {
            CanFit::Yes => 1,
            _ => 0,
        })
        .sum()
}

fn can_fit(presents: &Presents, region: &Region) -> CanFit {
    let (x, y) = region.size;
    let total_presents: u16 = region.present_counts.iter().sum();
    let densities = presents.iter().map(present_density).collect::<Vec<u16>>();
    let min_space = region
        .present_counts
        .iter()
        .zip(densities.iter())
        .map(|(c, d)| c * d)
        .sum::<u16>();

    if x * y < min_space {
        CanFit::No
    } else if total_presents <= (x / 3) * (y / 3) {
        CanFit::Yes
    } else {
        CanFit::Possibly
    }
}

fn present_density(present: &Present) -> u16 {
    present
        .iter()
        .map(|row| row.chars().filter(|&c| c == '#').count() as u16)
        .sum()
}
