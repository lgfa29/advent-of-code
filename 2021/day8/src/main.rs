use std::collections::{BTreeSet, HashMap};
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

struct Entry {
    signals: Vec<String>,
    outputs: Vec<String>,
}

fn part1(entries: &Vec<Entry>) -> i32 {
    let mut count = 0;
    for e in entries {
        for o in &e.outputs {
            match o.len() {
                2 | 3 | 4 | 7 => count += 1,
                _ => (),
            }
        }
    }

    count
}

fn part2(entries: &Vec<Entry>) -> i32 {
    let mut seg_to_digit: HashMap<String, i32> = HashMap::new();
    seg_to_digit.insert("abcefg".to_string(), 0);
    seg_to_digit.insert("cf".to_string(), 1);
    seg_to_digit.insert("acdeg".to_string(), 2);
    seg_to_digit.insert("acdfg".to_string(), 3);
    seg_to_digit.insert("bcdf".to_string(), 4);
    seg_to_digit.insert("abdfg".to_string(), 5);
    seg_to_digit.insert("abdefg".to_string(), 6);
    seg_to_digit.insert("acf".to_string(), 7);
    seg_to_digit.insert("abcdefg".to_string(), 8);
    seg_to_digit.insert("abcdfg".to_string(), 9);

    let mut sum = 0;

    for entry in entries {
        let mut segments: HashMap<char, BTreeSet<char>> = HashMap::new();
        let mut idx_length: HashMap<usize, Vec<String>> = HashMap::new();

        for s in &entry.signals {
            let v = idx_length.entry(s.len()).or_insert(Vec::new());
            v.push(s.to_string());
        }

        // 1 is the only digit with length 2 and uses `cf`.
        let one = &idx_length.get(&2).unwrap()[0];
        let one_segment_set: BTreeSet<_> = one.chars().collect();
        segments.insert('c', one_segment_set.clone());
        segments.insert('f', one_segment_set.clone());

        // 7 is the only digit with length 3 and uses `acf`.
        // 7 and 1 overlap in `cf`, so the extra one in 7 is `a`.
        let seven = &idx_length.get(&3).unwrap()[0];
        let seven_segment_set: BTreeSet<_> = seven.chars().collect();
        let diff: BTreeSet<_> = seven_segment_set
            .difference(&one_segment_set)
            .map(|c| *c)
            .collect();
        segments.insert('a', diff);

        // 4 is the only digit with length 4 and uses `bcdf`.
        // 4 and 1 overlap in `cf`, so the extra ones in 4 are `bd`.
        let four = &idx_length.get(&4).unwrap()[0];
        let four_segment_set: BTreeSet<_> = four.chars().collect();
        let diff: BTreeSet<_> = four_segment_set
            .difference(&one_segment_set)
            .map(|c| *c)
            .collect();
        segments.insert('b', diff.clone());
        segments.insert('d', diff.clone());

        // 8 is the only digit with length 7 and uses `abcdefg`.
        // Removing the segments from 1, 7, and 4 leaves `eg`.
        let eight = &idx_length.get(&7).unwrap()[0];
        let eight_segment_set: BTreeSet<_> = eight.chars().collect();
        let mut one_four_seven_segment_set: BTreeSet<char> = BTreeSet::new();
        one_four_seven_segment_set.extend(&one_segment_set);
        one_four_seven_segment_set.extend(&four_segment_set);
        one_four_seven_segment_set.extend(&seven_segment_set);
        let diff: BTreeSet<_> = eight_segment_set
            .difference(&one_four_seven_segment_set)
            .map(|c| *c)
            .collect();
        segments.insert('e', diff.clone());
        segments.insert('g', diff.clone());

        // 0, 6, and 9 have lenght 6.
        // 0 uses `abcefg`, 6 uses `abdefg`, and 9 uses `abcdfg`.
        // 0 and 9 overlaps with 1 and 6 doesn't.
        // 9 overlaps with 4 and 0 and 6 don't.
        let zero_six_nine = idx_length.get(&6).unwrap();
        let mut zero = "";
        let mut six = "";
        let mut nine = "";
        for d in zero_six_nine {
            let segment_set: BTreeSet<_> = d.chars().collect();
            if !segment_set.is_superset(&one_segment_set) {
                six = d;
            } else if segment_set.is_superset(&four_segment_set) {
                nine = d;
            } else {
                zero = d;
            }
        }
        let zero_segment_set: BTreeSet<_> = zero.chars().collect();
        let six_segment_set: BTreeSet<_> = six.chars().collect();
        let nine_segment_set: BTreeSet<_> = nine.chars().collect();

        // Find c and f from 6.
        let diff: BTreeSet<_> = one_segment_set
            .difference(&six_segment_set)
            .map(|c| *c)
            .collect();
        let intersect: BTreeSet<_> = one_segment_set
            .intersection(&six_segment_set)
            .map(|c| *c)
            .collect();
        segments.insert('c', diff);
        segments.insert('f', intersect);

        // Find g from 9.
        let mut four_seven_segment_set: BTreeSet<char> = BTreeSet::new();
        four_seven_segment_set.extend(&four_segment_set);
        four_seven_segment_set.extend(&seven_segment_set);
        let diff: BTreeSet<_> = nine_segment_set
            .difference(&four_seven_segment_set)
            .map(|c| *c)
            .collect();
        let e = segments.entry('e').or_insert(BTreeSet::new());
        e.remove(&diff.iter().collect::<Vec<&char>>()[0]);
        segments.insert('g', diff);

        // Find d from 0.
        let diff: BTreeSet<_> = nine_segment_set
            .difference(&zero_segment_set)
            .map(|c| *c)
            .collect();
        let b = segments.entry('b').or_insert(BTreeSet::new());
        b.remove(&diff.iter().collect::<Vec<&char>>()[0]);
        segments.insert('d', diff);

        let mut mapping: HashMap<BTreeSet<char>, i32> = HashMap::new();
        for (k, v) in &seg_to_digit {
            let mut key: BTreeSet<char> = BTreeSet::new();
            for c in k.chars() {
                key.insert(*segments.get(&c).unwrap().iter().collect::<Vec<&char>>()[0]);
            }
            mapping.insert(key, *v);
        }

        let mut num = 0;
        for (i, o) in entry.outputs.iter().enumerate() {
            let mut segment_set: BTreeSet<char> = BTreeSet::new();
            for c in o.chars() {
                segment_set.insert(c);
            }
            let digit = mapping.get(&segment_set).unwrap();
            num += digit * 10_i32.pow((entry.outputs.len() - 1 - i) as u32);
        }
        sum += num;
    }

    sum
}

fn parse_input(file_name: &str) -> Vec<Entry> {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);
    let mut entries: Vec<Entry> = Vec::new();

    for line in reader.lines() {
        let line = line.expect("Failed to read line.");

        let parts: Vec<&str> = line.split("|").collect();
        let signals = parts[0].split_whitespace().map(|s| s.to_string()).collect();
        let outputs = parts[1].split_whitespace().map(|s| s.to_string()).collect();

        entries.push(Entry { signals, outputs });
    }

    entries
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
