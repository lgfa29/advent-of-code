use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{prelude::*, BufReader};

enum Mode {
    Scanner,
    Coords,
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Scanner {
    index: usize,
    beacons: HashSet<Vector3>,
    last_seen: HashMap<usize, u64>,
    orientation_matrix: HashMap<usize, Matrix>,
    translation_vector: HashMap<usize, Vector3>,
}

impl Scanner {
    fn new(index: usize) -> Scanner {
        Scanner {
            index,
            beacons: HashSet::new(),
            last_seen: HashMap::new(),
            orientation_matrix: HashMap::new(),
            translation_vector: HashMap::new(),
        }
    }

    fn beacons_hash(&self) -> u64 {
        let mut s = DefaultHasher::new();
        for b in &self.beacons {
            b.hash(&mut s);
        }
        s.finish()
    }

    fn find_translate_vector(&self, target: &Scanner, m: &Matrix) -> Option<Vector3> {
        let mut matches = HashMap::new();

        for b1 in &target.beacons {
            let b1_rot = b1.matrix_mult(&m);

            for b0 in &self.beacons {
                // Find source - destination vector.
                let diff = b0.add(&b1_rot.inverse());
                let diff_match = matches.entry(diff).or_insert(0);
                *diff_match += 1
            }
        }

        for (v, count) in matches {
            if count >= 12 {
                return Some(v);
            }
        }

        None
    }

    fn update(&mut self, source: &Scanner, m: &Matrix, v: &Vector3) {
        self.orientation_matrix.insert(source.index, m.clone());
        self.translation_vector.insert(source.index, v.clone());

        for b in &source.beacons {
            self.beacons.insert(v.add(&b.matrix_mult(&m)));
        }
    }
}

type Matrix = Vec<Vec<i32>>;

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy, PartialOrd)]
struct Vector3 {
    x: i32,
    y: i32,
    z: i32,
}

impl Vector3 {
    fn new(x: i32, y: i32, z: i32) -> Vector3 {
        Vector3 { x, y, z }
    }

    fn inverse(&self) -> Vector3 {
        Vector3::new(-self.x, -self.y, -self.z)
    }

    fn matrix_mult(&self, m: &Matrix) -> Vector3 {
        Vector3 {
            x: self.x * m[0][0] + self.y * m[0][1] + self.z * m[0][2],
            y: self.x * m[1][0] + self.y * m[1][1] + self.z * m[1][2],
            z: self.x * m[2][0] + self.y * m[2][1] + self.z * m[2][2],
        }
    }

    fn add(&self, v: &Vector3) -> Vector3 {
        Vector3 {
            x: self.x + v.x,
            y: self.y + v.y,
            z: self.z + v.z,
        }
    }
}

fn orientation_matrices() -> Vec<Matrix> {
    vec![
        // Facing +x, +z is up.
        vec![vec![1, 0, 0], vec![0, 1, 0], vec![0, 0, 1]],
        // Facing +x, -z is up.
        vec![vec![1, 0, 0], vec![0, -1, 0], vec![0, 0, -1]],
        // Facing +x, +y is up.
        vec![vec![1, 0, 0], vec![0, 0, -1], vec![0, 1, 0]],
        // Facing +x, -y is up.
        vec![vec![1, 0, 0], vec![0, 0, 1], vec![0, -1, 0]],
        //
        // Facing -x, +z is up.
        vec![vec![-1, 0, 0], vec![0, -1, 0], vec![0, 0, 1]],
        // Facing -x, -z is up.
        vec![vec![-1, 0, 0], vec![0, 1, 0], vec![0, 0, -1]],
        // Facing -x, +y is up.
        vec![vec![-1, 0, 0], vec![0, 0, 1], vec![0, 1, 0]],
        // Facing -x, -y is up.
        vec![vec![-1, 0, 0], vec![0, 0, -1], vec![0, -1, 0]],
        //
        // Facing +y, +z is up.
        vec![vec![0, 1, 0], vec![-1, 0, 0], vec![0, 0, 1]],
        // Facing +y, -z is up.
        vec![vec![0, 1, 0], vec![-1, 0, 0], vec![0, 0, -1]],
        // Facing +y, +x is up.
        vec![vec![0, 1, 0], vec![0, 0, 1], vec![1, 0, 0]],
        // Facing +y, -x is up.
        vec![vec![0, 1, 0], vec![0, 0, -1], vec![-1, 0, 0]],
        //
        // Facing -y, +z is up.
        vec![vec![0, -1, 0], vec![1, 0, 0], vec![0, 0, 1]],
        // Facing -y, -z is up.
        vec![vec![0, -1, 0], vec![-1, 0, 0], vec![0, 0, -1]],
        // Facing -y, +x is up.
        vec![vec![0, -1, 0], vec![0, 0, -1], vec![1, 0, 0]],
        // Facing -y, -x is up.
        vec![vec![0, -1, 0], vec![0, 0, 1], vec![-1, 0, 0]],
        //
        // facing +z, +y is up.
        vec![vec![0, 0, 1], vec![1, 0, 0], vec![0, 1, 0]],
        // facing +z, -y is up.
        vec![vec![0, 0, 1], vec![-1, 0, 0], vec![0, -1, 0]],
        // facing +z, +x is up.
        vec![vec![0, 0, 1], vec![0, -1, 0], vec![1, 0, 0]],
        // facing +z, -x is up.
        vec![vec![0, 0, 1], vec![0, 1, 0], vec![-1, 0, 0]],
        //
        // facing -z, +y is up.
        vec![vec![0, 0, -1], vec![-1, 0, 0], vec![0, 1, 0]],
        // facing -z, -y is up.
        vec![vec![0, 0, -1], vec![1, 0, 0], vec![0, -1, 0]],
        // facing -z, +x is up.
        vec![vec![0, 0, -1], vec![0, 1, 0], vec![1, 0, 0]],
        // facing -z, -x is up.
        vec![vec![0, 0, -1], vec![0, -1, 0], vec![-1, 0, 0]],
    ]
}

fn part1(scanners: &mut Vec<Scanner>) -> usize {
    let mut done = false;

    while !done {
        done = true;

        for i in 0..scanners.len() {
            for j in 0..scanners.len() {
                if i == j {
                    continue;
                }

                let mut source = scanners[i].clone();
                let target = &scanners[j];
                let target_hash = target.beacons_hash();

                match source.last_seen.get(&target.index) {
                    None => (),
                    Some(h) => {
                        if *h == target_hash {
                            continue;
                        }
                    }
                }

                source.last_seen.insert(target.index, target_hash);

                for m in orientation_matrices() {
                    let t_vec = match source.find_translate_vector(&target, &m) {
                        None => continue,
                        Some(v) => v,
                    };

                    source.update(&target, &m, &t_vec);
                    scanners.remove(i);
                    scanners.insert(i, source);

                    done = false;
                    break;
                }
            }
        }
    }

    scanners[0].beacons.len()
}

fn part2(scanners: &Vec<Scanner>) -> usize {
    let mut max_dist = 0;

    for s1 in scanners {
        for t_vec in s1.translation_vector.values() {
            let dist = t_vec.x.abs() + t_vec.y.abs() + t_vec.z.abs();
            if dist > max_dist {
                max_dist = dist;
            }
        }
    }

    max_dist as usize
}

fn parse_input(file_name: &str) -> Vec<Scanner> {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);
    let mut scanners = Vec::new();
    let mut mode = Mode::Scanner;

    for line in reader.lines() {
        let line = line.expect("Failed to read line.");
        match &mode {
            Mode::Scanner => {
                let idx = line.split_whitespace().nth(2).unwrap().parse().unwrap();
                scanners.push(Scanner::new(idx));
                mode = Mode::Coords;
            }
            Mode::Coords => {
                if line.trim().len() == 0 {
                    mode = Mode::Scanner;
                    continue;
                }

                let coords: Vec<i32> = line.split(",").map(|n| n.parse().unwrap()).collect();
                let point = Vector3::new(coords[0], coords[1], coords[2]);
                let mut current_scanner = scanners.pop().unwrap();
                current_scanner.beacons.insert(point);
                scanners.push(current_scanner);
            }
        }
    }

    let mut first_scanner = scanners.get_mut(0).unwrap();

    first_scanner.orientation_matrix = HashMap::new();
    first_scanner
        .orientation_matrix
        .insert(0, vec![vec![1, 0, 0], vec![0, 1, 0], vec![0, 0, 1]]);

    first_scanner.translation_vector = HashMap::new();
    first_scanner
        .translation_vector
        .insert(0, Vector3::new(0, 0, 0));

    scanners
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut input_file = "input.txt";
    if args.len() == 2 {
        input_file = &args[1];
    }
    let mut input = parse_input(input_file);

    println!("Part 1: {}", part1(&mut input));
    println!("Part 2: {}", part2(&input));
}
