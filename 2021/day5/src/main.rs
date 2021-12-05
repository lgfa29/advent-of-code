use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

#[derive(Debug, Default)]
struct Line {
    x1: i32,
    y1: i32,
    x2: i32,
    y2: i32,
}

impl Line {
    fn is_horizontal(&self) -> bool {
        self.x1 == self.x2
    }

    fn is_vertical(&self) -> bool {
        self.y1 == self.y2
    }

    fn walk(&self) -> Vec<(i32, i32)> {
        let mut path: Vec<(i32, i32)> = Vec::new();
        let mut next_x1 = self.x1;
        let mut next_y1 = self.y1;

        loop {
            path.push((next_x1, next_y1));

            if next_x1 == self.x2 && next_y1 == self.y2 {
                break;
            }

            if self.x2 > next_x1 {
                next_x1 += 1;
            } else if self.x2 < next_x1 {
                next_x1 -= 1;
            }

            if self.y2 > next_y1 {
                next_y1 += 1;
            } else if self.y2 < next_y1 {
                next_y1 -= 1;
            }
        }

        path
    }
}

fn part1(lines: &Vec<Line>) -> i32 {
    let mut counter: HashMap<(i32, i32), i32> = HashMap::new();
    let mut overlaps = 0;

    for l in lines {
        if !l.is_horizontal() && !l.is_vertical() {
            continue;
        }

        for c in l.walk() {
            let pos = counter.entry(c).or_insert(0);
            *pos += 1;
            if *pos == 2 {
                overlaps += 1;
            }
        }
    }

    overlaps
}

fn part2(lines: &Vec<Line>) -> i32 {
    let mut counter: HashMap<(i32, i32), i32> = HashMap::new();
    let mut overlaps = 0;

    for l in lines {
        for c in l.walk() {
            let pos = counter.entry(c).or_insert(0);
            *pos += 1;
            if *pos == 2 {
                overlaps += 1;
            }
        }
    }

    overlaps
}

fn parse_input(file_name: &str) -> Vec<Line> {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);
    let mut lines: Vec<Line> = Vec::new();

    for line in reader.lines() {
        let line = line.expect("Failed to read line.");

        let coords: Vec<i32> = line
            .split("->")
            .map(|s| s.trim())
            .map(|c| c.split(','))
            .flatten()
            .map(|n| n.parse().unwrap())
            .collect();

        lines.push(Line {
            x1: coords[0],
            y1: coords[1],
            x2: coords[2],
            y2: coords[3],
        })
    }

    lines
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
