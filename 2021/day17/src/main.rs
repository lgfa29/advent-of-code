use regex::Regex;
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

#[derive(Debug, Default)]
struct Target {
    x1: i64,
    x2: i64,
    y1: i64,
    y2: i64,
}

#[derive(Debug, Default, Copy, Clone)]
struct Probe {
    x: i64,
    y: i64,
    vx: i64,
    vy: i64,
}

impl Probe {
    fn step(&mut self) {
        self.x += self.vx;
        self.y += self.vy;

        if self.vx > 0 {
            self.vx -= 1;
        } else if self.vx < 0 {
            self.vx += 1;
        }

        self.vy -= 1;
    }

    fn shoot(&mut self, t: &Target) -> (Vec<(i64, i64)>, bool) {
        let mut trajectory = Vec::new();

        loop {
            trajectory.push((self.x, self.y));

            if self.in_target(t) || self.past_target(t) {
                return (trajectory, self.in_target(t));
            }

            self.step();
        }
    }

    fn in_target(&self, t: &Target) -> bool {
        self.x >= t.x1 && self.x <= t.x2 && self.y >= t.y1 && self.y <= t.y2
    }

    fn past_target(&self, t: &Target) -> bool {
        if self.vx > 0 && self.x > t.x2
            || self.vx < 0 && self.x < t.x1
            || self.vx == 0 && (self.x < t.x1 || self.x > t.x2)
        {
            return true;
        }

        if self.vy <= 0 && self.y < t.y1 {
            return true;
        }

        false
    }
}

fn part1(t: &Target) -> i64 {
    let mut min_vx = 0;
    loop {
        let mut probe = Probe::default();
        probe.vx = min_vx;

        let (_, hit) = probe.shoot(t);
        if hit {
            break;
        }
        min_vx += 1;
    }

    let mut max_y = 0;
    for vy in 0..=t.y1.abs() {
        let mut probe = Probe::default();
        probe.vx = min_vx;
        probe.vy = vy;

        let (traj, hit) = probe.shoot(t);
        if hit {
            let local_max_y = traj.iter().max_by(|a, b| a.1.cmp(&b.1)).unwrap().1;
            if local_max_y >= max_y {
                max_y = local_max_y;
            }
        }
    }

    max_y
}

fn part2(t: &Target) -> i64 {
    let mut hit_count = 0;

    for vx in 0..=t.x2 {
        for vy in -(t.y1.abs())..=t.y1.abs() {
            let mut probe = Probe::default();
            probe.vx = vx;
            probe.vy = vy;
            let (_, hit) = probe.shoot(t);
            if hit {
                hit_count += 1;
            }
        }
    }

    hit_count
}

fn parse_input(file_name: &str) -> Target {
    let file = File::open(file_name).expect("Failed to read file");
    let mut reader = BufReader::new(file);

    let mut line = String::new();
    reader.read_line(&mut line).expect("Unable to read line");

    let mut target = Target::default();

    let re = Regex::new(r"target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)").unwrap();
    for cap in re.captures_iter(&line) {
        target = Target {
            x1: cap[1].parse().unwrap(),
            x2: cap[2].parse().unwrap(),
            y1: cap[3].parse().unwrap(),
            y2: cap[4].parse().unwrap(),
        };
    }

    target
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
