use std::collections::HashSet;
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

fn part1(m: &Vec<Vec<u32>>) -> u32 {
    let mut current: Vec<Vec<u32>> = m.to_vec();
    let mut total_flashes = 0;

    for _i in 0..100 {
        let (next, flashes) = step(&current);
        total_flashes += flashes;
        current = next;
    }

    total_flashes
}

fn part2(m: &Vec<Vec<u32>>) -> u32 {
    let total = (m.len() * m[0].len()) as u32;
    let mut current: Vec<Vec<u32>> = m.to_vec();
    let mut round = 1;

    loop {
        let (next, flashes) = step(&current);
        if flashes == total {
            return round;
        }
        current = next;
        round += 1;
    }
}

fn step(m: &Vec<Vec<u32>>) -> (Vec<Vec<u32>>, u32) {
    let mut next_m: Vec<Vec<u32>> = Vec::new();
    let mut flashes = 0;
    let mut flashed: HashSet<(usize, usize)> = HashSet::new();

    for (j, row) in m.iter().enumerate() {
        next_m.push(Vec::new());
        for v in row {
            next_m[j].push(v + 1);
        }
    }

    loop {
        let mut round_flashes = 0;

        for j in 0..next_m.len() {
            for i in 0..next_m[j].len() {
                if next_m[j][i] > 9 && !flashed.contains(&(i, j)) {
                    round_flashes += 1;
                    flashed.insert((i, j));

                    for (adj_i, adj_j) in adjacents(i, j, next_m[j].len(), next_m.len()) {
                        next_m[adj_j][adj_i] += 1;
                    }
                }
            }
        }

        if round_flashes == 0 {
            break;
        }
        flashes += round_flashes;
    }

    for (i, j) in flashed {
        next_m[j][i] = 0;
    }

    (next_m, flashes)
}

fn adjacents(i: usize, j: usize, max_i: usize, max_j: usize) -> Vec<(usize, usize)> {
    let mut result: Vec<(usize, usize)> = Vec::new();

    for di in -1..=1 {
        for dj in -1..=1 {
            if di == 0 && dj == 0 {
                continue;
            }
            let next_i = i as isize + di;
            let next_j = j as isize + dj;
            if next_i >= 0 && (next_i as usize) < max_i && next_j >= 0 && (next_j as usize) < max_j
            {
                result.push((next_i as usize, next_j as usize));
            }
        }
    }

    result
}

fn parse_input(file_name: &str) -> Vec<Vec<u32>> {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);

    let mut map: Vec<Vec<u32>> = Vec::new();

    for (j, line) in reader.lines().enumerate() {
        let line = line.expect("Failed to read line.");
        map.push(Vec::new());

        for c in line.chars() {
            let value: u32 = c.to_digit(10).expect("Failed to read char.");
            map[j].push(value);
        }
    }

    map
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
