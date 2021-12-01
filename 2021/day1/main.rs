use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

fn part1(file_name: &str) -> i32 {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);

    let mut prev = -1;
    let mut larger_than_prev = 0;

    for line in reader.lines() {
        if let Ok(value) = line {
            let value: i32 = value.trim().parse().expect("Failed to read number.");

            if prev == -1 {
                prev = value;
                continue;
            }

            if value > prev {
                larger_than_prev = larger_than_prev + 1;
            }
            prev = value;
        } else {
            println!("Failed to read line.");
            break;
        }
    }

    return larger_than_prev;
}

fn part2(file_name: &str) -> i32 {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);
    let mut larger_than_prev = 0;

    let mut prev_window: Vec<i32> = Vec::new();
    let mut cur_window: Vec<i32>;

    for line in reader.lines() {
        if let Ok(value) = line {
            let value: i32 = value.trim().parse().expect("Failed to read number.");

            if prev_window.len() < 3 {
                prev_window.push(value);
                continue;
            }

            cur_window = prev_window[1..].to_vec();
            cur_window.push(value);

            let cur_window_sum: i32 = cur_window.iter().sum();
            let prev_window_sum: i32 = prev_window.iter().sum();

            if cur_window_sum > prev_window_sum {
                larger_than_prev = larger_than_prev + 1;
            }

            prev_window = cur_window;
        } else {
            println!("Failed to read line.");
            break;
        }
    }

    return larger_than_prev;
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
