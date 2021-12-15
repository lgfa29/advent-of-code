use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

#[derive(Debug, Clone)]
struct Polymer {
    template: String,
    rules: HashMap<String, String>,
}

impl Polymer {
    fn grow(&self) -> Polymer {
        let mut result: Vec<char> = vec![self.template.chars().nth(0).unwrap()];

        for i in 1..self.template.len() {
            let pair = &self.template[(i - 1)..=i];
            if let Some(x) = self.rules.get(pair) {
                result.push(x.chars().nth(0).unwrap());
            }
            result.push(pair.chars().nth(1).unwrap());
        }

        Polymer {
            template: result.into_iter().collect(),
            rules: self.rules.clone(),
        }
    }
}

fn part1(polymer: &Polymer) -> u32 {
    let mut next = polymer.clone();
    for _i in 0..10 {
        next = next.grow();
    }

    let mut counter: HashMap<char, u32> = HashMap::new();
    for c in next.template.chars() {
        let count = counter.entry(c).or_insert(0);
        *count += 1;
    }

    let mut max: u32 = 0;
    let mut min: u32 = u32::MAX;
    for v in counter.values() {
        if v > &max {
            max = *v;
        }
        if v < &min {
            min = *v;
        }
    }

    max - min
}

fn part2(polymer: &Polymer) -> u64 {
    let mut pair_counter: HashMap<String, u64> = HashMap::new();
    let mut char_counter: HashMap<char, u64> = HashMap::new();

    for i in 1..polymer.template.len() {
        let pair = &polymer.template[(i - 1)..=i];
        let count = pair_counter.entry(pair.to_string()).or_insert(0);
        *count += 1;

        let count = char_counter
            .entry(pair.chars().nth(0).unwrap())
            .or_insert(0);
        *count += 1;
    }

    // Count last char since it was skipped above.
    let last_char = polymer
        .template
        .chars()
        .nth(polymer.template.len() - 1)
        .unwrap();
    let count = char_counter.entry(last_char).or_insert(0);
    *count += 1;

    for _i in 0..40 {
        let mut new_pair_counter: HashMap<String, u64> = HashMap::new();
        for (pair, pair_count) in &pair_counter {
            if let Some(x) = polymer.rules.get(pair) {
                let k1 = format!("{}{}", pair.chars().nth(0).unwrap(), x);
                let count = new_pair_counter.entry(k1).or_insert(0);
                *count += pair_count;

                let k2 = format!("{}{}", x, pair.chars().nth(1).unwrap());
                let count = new_pair_counter.entry(k2).or_insert(0);
                *count += pair_count;

                let count = char_counter.entry(x.chars().nth(0).unwrap()).or_insert(0);
                *count += pair_count;
            }
        }
        pair_counter = new_pair_counter;
    }

    let mut max: u64 = 0;
    let mut min: u64 = u64::MAX;
    for v in char_counter.values() {
        if v > &max {
            max = *v;
        }
        if v < &min {
            min = *v;
        }
    }

    max - min
}

fn parse_input(file_name: &str) -> Polymer {
    let file = File::open(file_name).expect("Failed to read file");
    let mut reader = BufReader::new(file);

    let mut template = String::new();
    let mut rules: HashMap<String, String> = HashMap::new();

    reader
        .read_line(&mut template)
        .expect("Unable to read line");
    template = template.trim().to_string();

    for line in reader.lines() {
        let line = line.expect("Failed to read line.");
        if line.trim().len() == 0 {
            continue;
        }

        let parts: Vec<&str> = line.split(" -> ").map(|s| s.trim()).collect();
        rules.insert(parts[0].to_string(), parts[1].to_string());
    }

    Polymer { template, rules }
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
