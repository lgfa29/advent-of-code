use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

fn part1(lines: &Vec<String>) -> u64 {
    let mut score = 0;

    for l in lines {
        let (_, corrupt, c) = process_line(l.to_string());
        if !corrupt {
            continue;
        }

        match c {
            ')' => score += 3,
            ']' => score += 57,
            '}' => score += 1197,
            '>' => score += 25137,
            _ => (),
        }
    }

    score
}

fn part2(lines: &Vec<String>) -> u64 {
    let mut scores: Vec<u64> = Vec::new();

    for l in lines {
        let mut score = 0;
        let (stack, corrupt, _) = process_line(l.to_string());
        if corrupt {
            continue;
        }

        for c in stack.iter().rev() {
            score *= 5;
            match pair(c) {
                ')' => score += 1,
                ']' => score += 2,
                '}' => score += 3,
                '>' => score += 4,
                _ => (),
            }
        }
        scores.push(score)
    }

    scores.sort();
    scores[scores.len() / 2]
}

fn process_line(line: String) -> (Vec<char>, bool, char) {
    let mut stack: Vec<char> = Vec::new();
    for c in line.chars() {
        match c {
            '(' | '[' | '{' | '<' => {
                stack.push(c);
                continue;
            }
            ')' | ']' | '}' | '>' => {
                if stack.len() > 0 && stack.last().unwrap() == pair(&c) {
                    stack.pop();
                    continue;
                }
                return (stack, true, c);
            }
            _ => (),
        }
    }
    (stack, false, '_')
}

fn pair(c: &char) -> &char {
    match c {
        '(' => &')',
        ')' => &'(',
        '[' => &']',
        ']' => &'[',
        '{' => &'}',
        '}' => &'{',
        '<' => &'>',
        '>' => &'<',
        _ => &'_',
    }
}

fn parse_input(file_name: &str) -> Vec<String> {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);
    reader
        .lines()
        .map(|l| l.unwrap())
        .map(|l| l.trim().to_string())
        .filter(|l| !l.is_empty())
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
