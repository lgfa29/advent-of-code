use std::collections::{BinaryHeap, HashSet};
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

fn part1(map: &Vec<Vec<u32>>) -> u32 {
    low_points(map)
        .iter()
        .fold(0, |acc, &(i, j)| acc + 1 + map[j][i])
}

fn part2(map: &Vec<Vec<u32>>) -> u32 {
    let low_point_coords = low_points(map);
    let mut basin_sizes: BinaryHeap<i32> = BinaryHeap::new();

    for coords in low_point_coords {
        let mut basin: Vec<u32> = Vec::new();
        let mut frontier: Vec<(usize, usize)> = Vec::new();
        let mut visited: HashSet<(usize, usize)> = HashSet::new();

        frontier.push(coords);

        while frontier.len() > 0 {
            let (i, j) = frontier.pop().unwrap();
            if visited.contains(&(i, j)) {
                continue;
            }

            visited.insert((i, j));
            basin.push(map[j][i]);

            for (adj_i, adj_j) in adjacents(i, j, map[0].len(), map.len()) {
                let adj_value = map[adj_j][adj_i];
                if adj_value < 9 {
                    frontier.push((adj_i, adj_j));
                }
            }
        }

        if basin.len() > 0 {
            basin_sizes.push(-(basin.len() as i32));
            if basin_sizes.len() > 3 {
                basin_sizes.pop();
            }
        }
    }

    (0..3).fold(1, |acc, _| acc * (-basin_sizes.pop().unwrap())) as u32
}

fn low_points(map: &Vec<Vec<u32>>) -> Vec<(usize, usize)> {
    let mut result: Vec<(usize, usize)> = Vec::new();

    for (j, row) in map.iter().enumerate() {
        for (i, &v) in row.iter().enumerate() {
            let mut low_point = true;

            for (adj_i, adj_j) in adjacents(i, j, row.len(), map.len()) {
                if map[adj_j][adj_i] <= v {
                    low_point = false;
                    break;
                }
            }

            if low_point {
                result.push((i, j));
            }
        }
    }

    result
}

fn adjacents(i: usize, j: usize, max_i: usize, max_j: usize) -> Vec<(usize, usize)> {
    let mut result: Vec<(usize, usize)> = Vec::new();

    if i > 0 {
        result.push((i - 1, j));
    }
    if i + 1 < max_i {
        result.push((i + 1, j));
    }
    if j > 0 {
        result.push((i, j - 1));
    }
    if j + 1 < max_j {
        result.push((i, j + 1));
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
