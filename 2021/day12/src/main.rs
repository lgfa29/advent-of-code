use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

#[derive(Debug)]
struct Cave {
    name: String,
    children: HashSet<String>,
}

fn part1(caves: &HashMap<String, Cave>) -> i32 {
    let mut visited: HashSet<String> = HashSet::new();
    let mut frontier: Vec<Vec<String>> = Vec::new();
    let mut current_path: Vec<String> = Vec::new();
    let mut paths = 0;

    frontier.push(vec!["start".to_string()]);

    while frontier.len() > 0 {
        let mut current_level = frontier.pop().unwrap();

        if current_level.len() == 0 {
            if current_path.len() > 0 {
                let c = current_path.pop().unwrap();
                if c.chars().nth(0).unwrap().is_lowercase() {
                    visited.remove(&c);
                }
            }
            continue;
        }

        let current = current_level.pop().unwrap();
        frontier.push(current_level.to_vec());

        if current == "end" {
            paths += 1;
            continue;
        }

        if is_small(current.to_string()) {
            if visited.contains(&current) {
                continue;
            }
            visited.insert(current.to_string());
        }

        current_path.push(current.to_string());

        let cave = caves.get(&current).unwrap();
        let mut children: Vec<String> = Vec::new();
        for c in &cave.children {
            children.push(c.to_string());
        }
        frontier.push(children);
    }

    paths
}

fn part2(caves: &HashMap<String, Cave>) -> i32 {
    let mut paths: Vec<Vec<String>> = Vec::new();
    paths.push(vec!["start".to_string()]);

    loop {
        let mut new_paths: Vec<Vec<String>> = Vec::new();

        for path in &paths {
            let cave = caves.get(path.last().unwrap()).unwrap();
            if cave.name == "end" {
                new_paths.push(path.clone());
                continue;
            }

            for child in &cave.children {
                if child == "start" {
                    continue;
                }

                if !is_small(child.to_string()) || can_add_small(path, child.to_string()) {
                    let mut new_path = path.clone();
                    new_path.push(child.to_string());
                    new_paths.push(new_path);
                }
            }
        }

        if paths == new_paths {
            break;
        }

        paths = new_paths;
    }

    paths.len() as i32
}

fn can_add_small(path: &Vec<String>, cave: String) -> bool {
    let mut counter: HashMap<String, u32> = HashMap::new();
    let mut repeat = false;
    for s in path {
        if !is_small(s.to_string()) {
            continue;
        }
        let c = counter.entry(s.to_string()).or_insert(0);
        *c += 1;
        if *c == 2 {
            repeat = true;
        }
    }

    counter.get(&cave) == None
        || counter.get(&cave) == Some(&0)
        || counter.get(&cave) == Some(&1) && !repeat
}

fn is_small(s: String) -> bool {
    s.chars().nth(0).unwrap().is_lowercase()
}

fn parse_input(file_name: &str) -> HashMap<String, Cave> {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);

    let mut caves: HashMap<String, Cave> = HashMap::new();

    for line in reader.lines() {
        let line = line.expect("Failed to read line.");
        let parts: Vec<&str> = line.split('-').collect();
        let src = parts[0];
        let dest = parts[1];

        let src_cave = caves.entry(src.to_string()).or_insert(Cave {
            name: src.to_string(),
            children: HashSet::new(),
        });
        src_cave.children.insert(dest.to_string());

        let dest_cave = caves.entry(dest.to_string()).or_insert(Cave {
            name: dest.to_string(),
            children: HashSet::new(),
        });
        dest_cave.children.insert(src.to_string());
    }

    caves
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
