use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

fn part1(file_name: &str) -> i32 {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);

    let mut horz = 0;
    let mut depth = 0;

    for line in reader.lines() {
        let line = line.expect("Failed to read line.");
        let line: Vec<&str> = line.split(" ").collect();
        let cmd = line[0];
        let value: i32 = line[1].trim().parse().expect("Failed to read number.");

        match cmd {
            "forward" => horz += value,
            "down" => depth += value,
            "up" => depth -= value,
            c => println!("Invalid command {}", c),
        }
    }

    return horz * depth;
}

fn part2(file_name: &str) -> i32 {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);

    let mut horz = 0;
    let mut depth = 0;
    let mut aim = 0;

    for line in reader.lines() {
        let line = line.expect("Failed to read line.");
        let line: Vec<&str> = line.split(" ").collect();
        let cmd = line[0];
        let value: i32 = line[1].trim().parse().expect("Failed to read number.");

        match cmd {
            "forward" => {
                horz += value;
                depth += aim * value;
            }
            "down" => aim += value,
            "up" => aim -= value,
            c => println!("Invalid command {}", c),
        }
    }

    return horz * depth;
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut input_file = "input.txt";
    if args.len() == 2 {
        input_file = &args[1];
    }

    let part1_result = part1(input_file);
    let part2_result = part2(input_file);

    println!("Part 1: {}", part1_result);
    println!("Part 2: {}", part2_result);
}
