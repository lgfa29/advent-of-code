use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

struct FishCounter {
    counter: HashMap<u64, u64>,
}

impl FishCounter {
    fn new(fishes: &Vec<u64>) -> FishCounter {
        let mut counter: HashMap<u64, u64> = HashMap::new();
        for f in fishes {
            let count = counter.entry(*f).or_insert(0);
            *count += 1;
        }

        FishCounter { counter }
    }

    fn step(&mut self) {
        let mut new_counter: HashMap<u64, u64> = HashMap::new();

        for k in self.counter.keys() {
            let count = *self.counter.get(k).unwrap();
            if *k == 0 {
                new_counter.insert(6, count + new_counter.get(&6).unwrap_or(&0));
                new_counter.insert(8, count + new_counter.get(&8).unwrap_or(&0));
                continue;
            }
            new_counter.insert(k - 1, count + new_counter.get(&(k - 1)).unwrap_or(&0));
        }

        self.counter = new_counter;
    }

    fn count(&self) -> u64 {
        let mut count = 0;
        for k in self.counter.keys() {
            count += *self.counter.get(k).unwrap();
        }

        count
    }
}

fn part1(fishes: &Vec<u64>) -> u64 {
    let mut counter = FishCounter::new(fishes);
    for _d in 0..80 {
        counter.step();
    }

    counter.count()
}

fn part2(fishes: &Vec<u64>) -> u64 {
    let mut counter = FishCounter::new(fishes);
    for _d in 0..256 {
        counter.step();
    }

    counter.count()
}

fn parse_input(file_name: &str) -> Vec<u64> {
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
