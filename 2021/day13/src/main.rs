use std::collections::HashSet;
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

#[derive(Debug)]
enum Axis {
    X,
    Y,
}

fn part1((dots, folds): &(HashSet<(u32, u32)>, Vec<(Axis, u32)>)) -> u32 {
    let mut new_dots: HashSet<(u32, u32)> = HashSet::new();

    let fold = &folds[0];
    for dot in dots {
        let new_dot = project(dot, &fold);
        new_dots.insert(new_dot);
    }

    new_dots.len() as u32
}

fn part2((dots, folds): &(HashSet<(u32, u32)>, Vec<(Axis, u32)>)) -> String {
    let mut current_dots: HashSet<(u32, u32)> = dots.clone();

    for fold in folds {
        let mut new_dots = HashSet::new();
        for dot in current_dots {
            let new_dot = project(&dot, &fold);
            new_dots.insert(new_dot);
        }
        current_dots = new_dots;
    }

    print_dots(&current_dots)
}

fn print_dots(dots: &HashSet<(u32, u32)>) -> String {
    let mut result: Vec<char> = Vec::new();
    let mut max_x: u32 = 0;
    let mut max_y: u32 = 0;

    for (x, y) in dots {
        if x > &max_x {
            max_x = *x;
        }
        if y > &max_y {
            max_y = *y;
        }
    }

    for y in 0..=max_y {
        for x in 0..=max_x {
            if dots.contains(&(x, y)) {
                result.push('#');
            } else {
                result.push('.');
            }
        }
        result.push('\n');
    }
    result.into_iter().collect()
}

fn project((x, y): &(u32, u32), (axis, fold): &(Axis, u32)) -> (u32, u32) {
    let value = match axis {
        Axis::X => x,
        Axis::Y => y,
    };

    if value < fold {
        return (*x, *y);
    }

    let diff = value - fold;
    match axis {
        Axis::X => (fold - diff, *y),
        Axis::Y => (*x, fold - diff),
    }
}

fn parse_input(file_name: &str) -> (HashSet<(u32, u32)>, Vec<(Axis, u32)>) {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);

    let mut dots: HashSet<(u32, u32)> = HashSet::new();
    let mut folds: Vec<(Axis, u32)> = Vec::new();

    let mut mode = "coords";
    for line in reader.lines() {
        let line = line.expect("Failed to read line.");
        if line.trim().len() == 0 {
            mode = "folds";
            continue;
        }

        match mode {
            "coords" => {
                let parts: Vec<u32> = line.split(',').map(|s| s.parse().unwrap()).collect();
                dots.insert((parts[0], parts[1]));
            }
            "folds" => {
                let parts: Vec<&str> = line.split(' ').map(|s| s.split('=')).flatten().collect();
                let axis = match parts[2] {
                    "x" => Axis::X,
                    "y" => Axis::Y,
                    _ => Axis::X,
                };
                folds.push((axis, parts[3].parse().unwrap()));
            }
            _ => (),
        }
    }

    (dots, folds)
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut input_file = "input.txt";
    if args.len() == 2 {
        input_file = &args[1];
    }
    let input = parse_input(input_file);

    println!("Part 1: {}", part1(&input));
    println!("Part 2: \n{}", part2(&input));
}
