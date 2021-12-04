use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

#[derive(Debug, Clone)]
struct Board {
    size: (i32, i32),
    numbers: HashMap<i32, Vec<(usize, usize)>>,
    marked: Vec<Vec<(bool, i32)>>,
}

impl Board {
    fn new(lines: &Vec<String>) -> Board {
        let mut board = Board {
            size: (lines.len() as i32, 0),
            numbers: HashMap::new(),
            marked: Vec::new(),
        };

        for (i, line) in lines.iter().enumerate() {
            board.marked.push(Vec::new());

            let nums: Vec<i32> = line
                .split(' ')
                .map(|l| l.trim())
                .filter(|l| !l.is_empty())
                .map(|s| s.parse().unwrap())
                .collect();
            board.size.1 = nums.len() as i32;

            for (j, n) in nums.iter().enumerate() {
                let coords = board.numbers.entry(*n).or_insert(Vec::new());
                coords.push((i, j));

                board.marked[i].push((false, *n));
            }
        }

        board
    }

    fn mark(&mut self, n: &i32) {
        if !self.numbers.contains_key(n) {
            return;
        }

        let n = self.numbers.get(n).unwrap();
        for coord in n {
            self.marked[coord.0][coord.1].0 = true;
        }
    }

    fn winner(&self) -> bool {
        for i in 0..self.size.0 {
            let mut winner = true;
            for j in 0..self.size.1 {
                if !self.marked[i as usize][j as usize].0 {
                    winner = false;
                    break;
                }
            }
            if winner {
                return true;
            }
        }

        for j in 0..self.size.1 {
            let mut winner = true;
            for i in 0..self.size.0 {
                if !self.marked[i as usize][j as usize].0 {
                    winner = false;
                    break;
                }
            }
            if winner {
                return true;
            }
        }

        false
    }

    fn sum_unmarked(&self) -> i32 {
        let mut score: i32 = 0;

        for i in 0..self.size.0 {
            for j in 0..self.size.1 {
                let pos = self.marked[i as usize][j as usize];
                if !pos.0 {
                    score += pos.1;
                }
            }
        }

        score
    }
}

fn part1(mut boards: Vec<Board>, numbers: &Vec<i32>) -> i32 {
    for n in numbers {
        for i in 0..boards.len() {
            let mut b = boards[i].clone();
            b.mark(n);

            if b.winner() {
                return b.sum_unmarked() * n;
            }

            boards[i] = b;
        }
    }

    -1
}

fn part2(mut boards: Vec<Board>, numbers: &Vec<i32>) -> i32 {
    let mut winners = HashSet::new();

    for n in numbers {
        for i in 0..boards.len() {
            if winners.contains(&i) {
                continue;
            }

            let mut b = boards[i].clone();
            b.mark(n);

            if b.winner() {
                winners.insert(i);
                if winners.len() == boards.len() {
                    return b.sum_unmarked() * n;
                }
            }

            boards[i] = b;
        }
    }

    -1
}

fn parse_input(file_name: &str) -> (Vec<Board>, Vec<i32>) {
    let file = File::open(file_name).expect("Failed to read file");
    let mut reader = BufReader::new(file);

    // Read numbers.
    let mut first_line = String::new();
    reader
        .read_line(&mut first_line)
        .expect("Unable to read line");
    let numbers = first_line
        .split(',')
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .map(|s| s.parse().unwrap())
        .collect();

    // Read boards.
    let mut boards: Vec<Board> = Vec::new();
    let mut board_values: Vec<String> = Vec::new();

    for line in reader.lines() {
        let line = line.expect("Failed to read line.");
        if line.trim().len() == 0 {
            if board_values.len() != 0 {
                boards.push(Board::new(&board_values));
                board_values = Vec::new();
            }
            continue;
        }
        board_values.push(line);
    }
    if board_values.len() != 0 {
        boards.push(Board::new(&board_values));
    }

    (boards, numbers)
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut input_file = "input.txt";
    if args.len() == 2 {
        input_file = &args[1];
    }
    let input = parse_input(input_file);

    println!("Part 1: {}", part1(input.0.clone(), &input.1));
    println!("Part 2: {}", part2(input.0.clone(), &input.1));
}
