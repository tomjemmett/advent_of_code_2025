use std::fs;

#[derive(Clone)]
struct Grid {
    cells: Vec<Vec<bool>>,
    height: isize,
    width: isize,
}

pub fn run(file_type: &str) -> Option<(String, String)> {
    if let Ok(contents) = fs::read_to_string(format!("inputs/{}/04.txt", file_type)) {
        let mut input = parse_input(&contents);

        let p1 = part1(&mut input);
        let p2 = part2(&mut input);

        Some((p1.to_string(), (p1 + p2).to_string()))
    } else {
        None
    }
}

fn parse_input(input: &str) -> Grid {
    let grid: Vec<Vec<bool>> = input
        .lines()
        .map(|line| line.chars().map(|c| c == '@').collect())
        .collect();

    let height = grid.len();
    let width = grid[0].len();

    Grid {
        cells: grid,
        height: height as isize,
        width: width as isize,
    }
}

fn count_neighbours(grid: &Grid, i: usize, j: usize) -> u8 {
    let mut result = 0;
    for di in -1..=1 {
        for dj in -1..=1 {
            if di == 0 && dj == 0 {
                continue;
            }
            let ni = i as isize + di;
            let nj = j as isize + dj;

            if (ni < 0 || ni >= grid.height) || (nj < 0 || nj >= grid.width) {
                continue;
            }

            if grid.cells[ni as usize][nj as usize] {
                result += 1;
            }
        }
    }

    result
}

fn iterate(grid: &mut Grid) -> u32 {
    let mut count = 0;

    let old_grid = grid.clone();
    for i in 0..grid.height {
        for j in 0..grid.width {
            if !grid.cells[i as usize][j as usize] {
                continue;
            }

            let neighbours = count_neighbours(&old_grid, i as usize, j as usize);
            let can_remove = neighbours < 4;

            if can_remove {
                count += 1;
                grid.cells[i as usize][j as usize] = false;
            }
        }
    }

    count
}

fn part1(grid: &mut Grid) -> u32 {
    iterate(grid)
}

fn part2(grid: &mut Grid) -> u32 {
    let iter_count = iterate(grid);
    if iter_count > 0 {
        return iter_count + part2(grid);
    }
    0
}
