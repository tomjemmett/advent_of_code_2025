use clap::Parser;
use dotenvy::dotenv;
use reqwest::blocking::Client;
use reqwest::header::COOKIE;
use scraper::{Html, Selector};
use std::error::Error;
use std::fs;
use std::io::{self, Write};

const AOC_YEAR: u32 = 2025;

#[derive(Parser, Debug)]
#[command(name = "download_inputs", about = "Download inputs for Advent of Code")]
struct Args {
    day: u8,
}

fn get_possible_sample_input(
    client: &Client,
    day: u8,
    year: u32,
    cookie: &str,
) -> Result<Vec<String>, Box<dyn Error>> {
    let url = format!("https://adventofcode.com/{year}/day/{day}");
    let page = client
        .get(url)
        .header(COOKIE, format!("session={cookie}"))
        .send()?
        .error_for_status()?
        .text()?;

    let document = Html::parse_document(&page);
    let selector = Selector::parse("pre > code")?;

    let mut samples = Vec::new();
    for node in document.select(&selector) {
        let text = node.text().collect::<String>();
        if !samples.contains(&text) {
            samples.push(text);
        }
    }

    Ok(samples)
}

fn get_actual_input(
    client: &Client,
    day: u8,
    year: u32,
    cookie: &str,
) -> Result<String, Box<dyn Error>> {
    let url = format!("https://adventofcode.com/{year}/day/{day}/input");
    let actual = client
        .get(url)
        .header(COOKIE, format!("session={cookie}"))
        .send()?
        .error_for_status()?
        .text()?;

    Ok(actual)
}

fn choose_sample(samples: &[String]) -> Result<&str, Box<dyn Error>> {
    if samples.is_empty() {
        return Err("No sample input found on puzzle page".into());
    }

    if samples.len() == 1 {
        return Ok(&samples[0]);
    }

    println!("Choose a sample to use:");
    for (i, sample) in samples.iter().enumerate() {
        println!("-----");
        println!("{}:", i + 1);
        println!("-----");
        println!();
        println!("{sample}");
    }

    loop {
        print!("Enter the number of the sample to use, or enter to use first: ");
        io::stdout().flush()?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        let trimmed = input.trim();
        let value = if trimmed.is_empty() {
            1
        } else {
            match trimmed.parse::<usize>() {
                Ok(v) => v,
                Err(_) => {
                    println!("Please enter a valid integer.");
                    continue;
                }
            }
        };

        if (1..=samples.len()).contains(&value) {
            return Ok(&samples[value - 1]);
        }

        println!("Please enter a number between 1 and {}.", samples.len());
    }
}

pub fn download_inputs(day: u8) -> Result<(), Box<dyn Error>> {
    dotenv().ok();

    let cookie = std::env::var("AOC_COOKIE")?;

    let client = Client::builder()
        .user_agent("advent_of_code input downloader")
        .build()?;

    let samples = get_possible_sample_input(&client, day, AOC_YEAR, &cookie)?;
    let actual = get_actual_input(&client, day, AOC_YEAR, &cookie)?;
    let sample = choose_sample(&samples)?;

    fs::create_dir_all("inputs/sample")?;
    fs::create_dir_all("inputs/actual")?;

    fs::write(format!("inputs/sample/{day:02}.txt"), sample)?;
    fs::write(format!("inputs/actual/{day:02}.txt"), actual)?;

    Ok(())
}
