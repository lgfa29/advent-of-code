use std::collections::HashSet;
use std::env;
use std::fmt;
use std::fs::File;
use std::io::{prelude::*, BufReader};

#[derive(Debug, Clone)]
struct Image {
    light_pixels: HashSet<Point>,
    unset: char,
    top_left: Point,
    bot_right: Point,
}

impl fmt::Display for Image {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for y in self.top_left.y..=self.bot_right.y {
            for x in self.top_left.x..=self.bot_right.x {
                let p = Point::new(x, y);
                let c = match self.light_pixels.get(&p) {
                    None => '.',
                    Some(_) => '#',
                };
                write!(f, "{}", c)?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl Image {
    fn apply(&self, algo: &HashSet<u64>) -> Image {
        let mut next = HashSet::new();

        for x in (self.top_left.x - 2)..=(self.bot_right.x + 2) {
            for y in (self.top_left.y - 2)..=(self.bot_right.y + 2) {
                let p = Point::new(x, y);
                let mut bits = Vec::new();

                for n in p.neighbours() {
                    let c = match self.light_pixels.get(&n) {
                        None => {
                            if self.includes(&n) || self.unset == '.' {
                                '0'
                            } else {
                                '1'
                            }
                        }
                        Some(_) => '1',
                    };
                    bits.push(c);
                }
                let idx_bin: String = bits.into_iter().collect();
                let idx = u64::from_str_radix(&idx_bin, 2).unwrap();

                if algo.contains(&idx) {
                    next.insert(Point::new(x, y));
                }
            }
        }

        let next_unset_idx = match self.unset {
            '.' => u64::from_str_radix("000000000", 2).unwrap(),
            '#' => u64::from_str_radix("111111111", 2).unwrap(),
            _ => panic!(),
        };
        let next_unset = match algo.contains(&next_unset_idx) {
            true => '#',
            false => '.',
        };

        Image {
            light_pixels: next,
            unset: next_unset,
            top_left: Point::new(self.top_left.x - 1, self.top_left.y - 1),
            bot_right: Point::new(self.bot_right.x + 1, self.bot_right.y + 1),
        }
    }

    fn includes(&self, p: &Point) -> bool {
        p.x >= self.top_left.x
            && p.x <= self.bot_right.x
            && p.y >= self.top_left.y
            && p.y <= self.bot_right.y
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
struct Point {
    x: i64,
    y: i64,
}

impl Point {
    fn new(x: i64, y: i64) -> Point {
        Point { x, y }
    }

    fn neighbours(&self) -> Vec<Point> {
        let mut result = Vec::new();

        for dy in -1..=1 {
            for dx in -1..=1 {
                result.push(Point::new(self.x + dx, self.y + dy));
            }
        }

        result
    }
}

fn part1((algo, img): &(HashSet<u64>, Image)) -> usize {
    let next = img.apply(algo).apply(algo);
    next.light_pixels.len()
}

fn part2((algo, img): &(HashSet<u64>, Image)) -> usize {
    let next = (0..50).fold(img.clone(), |acc, _i| acc.apply(algo));
    next.light_pixels.len()
}

fn parse_input(file_name: &str) -> (HashSet<u64>, Image) {
    let file = File::open(file_name).expect("Failed to read file");
    let mut reader = BufReader::new(file);

    let mut line = String::new();
    reader.read_line(&mut line).expect("Unable to read line");

    let mut algo = HashSet::new();
    for (i, c) in line.chars().enumerate() {
        if c == '#' {
            algo.insert(i as u64);
        }
    }

    let mut image = HashSet::new();
    let mut max_i = 0;

    let mut j = 0;
    for line in reader.lines() {
        let line = line.expect("Failed to read line.");
        if line.trim().len() == 0 {
            continue;
        }

        for (i, c) in line.chars().enumerate() {
            if c == '#' {
                let p = Point::new(i as i64, j as i64);
                image.insert(p);
            }

            if i > max_i {
                max_i = i;
            }
        }

        j += 1;
    }

    (
        algo,
        Image {
            light_pixels: image,
            unset: '.',
            top_left: Point::new(0, 0),
            bot_right: Point::new(j, max_i as i64),
        },
    )
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
