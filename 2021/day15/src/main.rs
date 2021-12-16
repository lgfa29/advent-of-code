use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
struct Position {
    x: usize,
    y: usize,
}

fn part1(grid: &Vec<Vec<u32>>) -> i32 {
    let start = Position { x: 0, y: 0 };
    let goal = Position {
        x: grid[0].len() - 1,
        y: grid.len() - 1,
    };

    a_star(&start, &goal, grid)
}

fn part2(grid: &Vec<Vec<u32>>) -> i32 {
    let mut new_grid = Vec::new();
    let y_len = grid.len();
    let x_len = grid[0].len();

    for y in 0..5 * y_len {
        let y_tile = y / y_len;
        new_grid.push(Vec::new());

        for x in 0..5 * x_len {
            let x_tile = x / x_len;
            let mut value = grid[y % y_len][x % x_len] + x_tile as u32 + y_tile as u32;
            if value > 9 {
                value = value - 9
            }
            new_grid[y].push(value);
        }
    }

    part1(&new_grid)
}

fn a_star(start: &Position, goal: &Position, grid: &Vec<Vec<u32>>) -> i32 {
    let mut open_set: Vec<Position> = Vec::new();
    open_set.push(*start);

    let mut came_from: HashMap<Position, Position> = HashMap::new();

    let mut g_score: HashMap<Position, i32> = HashMap::new();
    g_score.insert(*start, 0);

    let mut f_score: HashMap<Position, i32> = HashMap::new();
    f_score.insert(*start, heuristic(start, goal));

    while open_set.len() > 0 {
        open_set.sort_by(|a, b| {
            let a_score = f_score.get(a).unwrap_or(&i32::MAX);
            let b_score = f_score.get(b).unwrap_or(&i32::MAX);
            b_score.cmp(a_score)
        });

        let current = open_set.pop().unwrap();
        if current == *goal {
            return *g_score.get(goal).unwrap();
        }

        for n in neighbours(grid, &current) {
            let tentative_g_score =
                g_score.get(&current).unwrap_or(&i32::MAX) + grid[n.y][n.x] as i32;
            let n_g_score = g_score.get(&n).unwrap_or(&i32::MAX);
            if tentative_g_score < *n_g_score {
                came_from.insert(n, current);
                g_score.insert(n, tentative_g_score);
                f_score.insert(n, tentative_g_score + heuristic(&n, goal));
                if !open_set.contains(&n) {
                    open_set.push(n);
                }
            }
        }
    }

    return -1;
}

fn neighbours(grid: &Vec<Vec<u32>>, p: &Position) -> Vec<Position> {
    let mut result = Vec::new();
    let max_y = grid.len();
    let max_x = grid[0].len();

    if p.x > 0 {
        result.push(Position { x: p.x - 1, y: p.y });
    }
    if p.x + 1 < max_x {
        result.push(Position { x: p.x + 1, y: p.y });
    }
    if p.y > 0 {
        result.push(Position { x: p.x, y: p.y - 1 });
    }
    if p.y + 1 < max_y {
        result.push(Position { x: p.x, y: p.y + 1 });
    }

    result
}

fn heuristic(p: &Position, goal: &Position) -> i32 {
    (goal.x - p.x + goal.y - p.y) as i32
}

fn parse_input(file_name: &str) -> Vec<Vec<u32>> {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);
    let mut grid: Vec<Vec<u32>> = Vec::new();

    for line in reader.lines() {
        let line = line.expect("Failed to read line.");
        grid.push(line.chars().map(|c| c.to_digit(10).unwrap()).collect());
    }
    grid
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut input_file = "input.txt";
    if args.len() == 2 {
        input_file = &args[1];
    }
    let input = parse_input(input_file);

    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}
