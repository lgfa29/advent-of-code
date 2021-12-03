use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

enum GeneratorType {
    O2,
    CO2,
}

fn part1(input: &Vec<u16>, size: usize) -> i32 {
    let counter = count_bits(input);
    let mut gamma = 0;
    let mut epsilon = 0;

    for i in 0..size {
        if counter[i] > 0 {
            gamma |= 1 << i;
        } else {
            epsilon |= 1 << i;
        }
    }

    gamma * epsilon
}

fn part2(input: &Vec<u16>, size: usize) -> i32 {
    let o2_rating = find_rating(input, size, GeneratorType::O2);
    let co2_rating = find_rating(input, size, GeneratorType::CO2);

    o2_rating * co2_rating
}

fn find_rating(input: &Vec<u16>, size: usize, generator: GeneratorType) -> i32 {
    let mut pool: Vec<u16>;
    pool = input.clone();

    for i in (0..size).rev() {
        if pool.len() == 1 {
            break;
        }

        let counter = count_bits(&pool);
        let mut new_pool: Vec<u16> = Vec::new();

        for v in pool {
            let bit_i = v >> i & 1;
            let should_add = match generator {
                GeneratorType::O2 => bit_i == 1 && counter[i] >= 0 || bit_i == 0 && counter[i] < 0,
                GeneratorType::CO2 => bit_i == 0 && counter[i] >= 0 || bit_i == 1 && counter[i] < 0,
            };

            if should_add {
                new_pool.push(v);
            }
        }

        pool = new_pool.clone();
    }

    pool[0] as i32
}

fn count_bits(input: &Vec<u16>) -> [i32; 16] {
    let mut counter: [i32; 16] = [0; 16];

    for v in input {
        for i in (0..16).rev() {
            let bit_i = v >> i & 1;
            if bit_i == 0 {
                counter[i] -= 1;
            } else {
                counter[i] += 1;
            }
        }
    }

    counter
}

fn parse_input(file_name: &str) -> (Vec<u16>, usize) {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);

    let mut values: Vec<u16> = Vec::new();
    let mut size: usize = 0;

    for line in reader.lines() {
        let line = line.expect("Failed to read line.");
        let value = u16::from_str_radix(&line, 2).expect("Failed to parse number");
        values.push(value);

        if line.len() > size {
            size = line.len();
        }
    }

    (values, size)
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut input_file = "input.txt";
    if args.len() == 2 {
        input_file = &args[1];
    }

    let input = parse_input(input_file);

    println!("Part 1: {}", part1(&input.0, input.1));
    println!("Part 2: {}", part2(&input.0, input.1));
}
