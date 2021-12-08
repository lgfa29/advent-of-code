use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

fn part1(input: &Vec<i64>) -> i64 {
    let len = input.len() as i64;
    let max = input.iter().max().unwrap();
    let mut fuel: i64 = input.iter().sum();
    let mut min_fuel = fuel;

    for i in 1..=*max {
        let gte_count = input
            .iter()
            .fold(0, |acc, j| if j >= &i { acc + 1 } else { acc });
        let lt_count = len - gte_count;

        fuel = fuel - gte_count + lt_count;
        if fuel < min_fuel {
            min_fuel = fuel;
        }
    }

    min_fuel
}

fn part2(input: &Vec<i64>) -> i64 {
    let max = input.iter().max().unwrap();
    let mut min_fuel = i64::MAX;

    let mut costs: HashMap<(i64, i64), i64> = HashMap::new();
    for i in input {
        if costs.contains_key(&(*i, 0)) {
            continue;
        }

        costs.insert((*i, 0), 0);
        for j in 1..=*max {
            costs.insert((*i, j), *costs.get(&(*i, j - 1)).unwrap() + j);
        }
    }

    for i in 0..=*max {
        let mut fuel = 0;
        for j in input {
            let diff = i64::abs(i - j);
            let key = (*j, diff);
            let cost = costs.get(&key).unwrap();
            fuel += cost;
        }

        if fuel < min_fuel {
            min_fuel = fuel
        }
    }

    min_fuel
}

fn parse_input(file_name: &str) -> Vec<i64> {
    let file = File::open(file_name).expect("Failed to read file");
    let mut reader = BufReader::new(file);

    let mut line = String::new();
    reader.read_line(&mut line).expect("Unable to read line");
    line.split(',')
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .map(|s| s.parse().unwrap())
        .collect()
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
