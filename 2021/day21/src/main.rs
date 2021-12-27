use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

#[derive(Debug)]
struct DeterministicDie {
    rolls_count: u64,
}

impl Iterator for DeterministicDie {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        let next = (self.rolls_count % 100) + 1;
        self.rolls_count += 1;
        Some(next)
    }
}

impl DeterministicDie {
    fn new() -> DeterministicDie {
        DeterministicDie { rolls_count: 0 }
    }
}

#[derive(Debug, Clone)]
struct Game {
    player_spaces: Vec<u64>,
    player_scores: Vec<u64>,
    current_player: usize,
    winning_score: u64,
}

impl Game {
    fn new(starting_spaces: &[u64], winning_score: u64) -> Game {
        Game {
            player_spaces: starting_spaces.to_vec(),
            player_scores: vec![0; starting_spaces.len()],
            current_player: 0,
            winning_score,
        }
    }

    fn play(&self, roll: u64) -> Game {
        let mut next = self.clone();

        let current_space = next.player_spaces[next.current_player];
        let next_space = (((current_space - 1) + roll) % 10) + 1;

        next.player_spaces[next.current_player] = next_space;
        next.player_scores[next.current_player] += next_space;
        next.current_player = (next.current_player + 1) % next.player_spaces.len();

        next
    }

    fn winner(&self) -> Option<usize> {
        for (i, score) in self.player_scores.iter().enumerate() {
            if score >= &self.winning_score {
                return Some(i);
            }
        }

        None
    }
}

fn part1(starting_spaces: &[u64]) -> u64 {
    let mut d = DeterministicDie::new();
    let mut g = Game::new(starting_spaces, 1000);

    while g.winner().is_none() {
        let roll = (0..3).fold(0, |acc, _| acc + d.next().unwrap());
        g = g.play(roll);
    }

    for score in &g.player_scores {
        if score < &1000 {
            return score * d.rolls_count;
        }
    }

    0
}

fn part2(starting_spaces: &[u64]) -> u64 {
    let mut games: Vec<(Game, u64)> = Vec::new();
    let mut winning_count = HashMap::new();

    let g = Game::new(starting_spaces, 21);
    games.push((g, 1));

    while games.len() > 0 {
        let (g, c) = games.pop().unwrap();

        match g.winner() {
            None => (),
            Some(p) => {
                let count = winning_count.entry(p).or_insert(0);
                *count += c;
                continue;
            }
        }

        games.push((g.play(3), c * 1));
        games.push((g.play(4), c * 3));
        games.push((g.play(5), c * 6));
        games.push((g.play(6), c * 7));
        games.push((g.play(7), c * 6));
        games.push((g.play(8), c * 3));
        games.push((g.play(9), c * 1));
    }

    *winning_count.values().max().unwrap()
}

fn parse_input(file_name: &str) -> Vec<u64> {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);
    let mut starting_spaces = Vec::new();

    for line in reader.lines() {
        let line = line.expect("Unable to read line");
        let player_space = line
            .split(":")
            .nth(1)
            .unwrap()
            .trim()
            .parse::<u64>()
            .unwrap();
        starting_spaces.push(player_space);
    }

    starting_spaces
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
