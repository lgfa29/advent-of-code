use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

enum Mode {
    Version,
    Type,
    LiteralValue,
    Operator,
    Done,
}

#[derive(Debug, Default)]
struct Packet {
    version: u8,
    type_id: u8,
    value: u64,
    subpackets: Vec<Packet>,
}

impl Packet {
    fn version_sum(&self) -> u64 {
        self.subpackets
            .iter()
            .fold(self.version as u64, |acc, p| acc + p.version_sum())
    }

    fn total_value(&self) -> u64 {
        match self.type_id {
            0 => self.sum(),
            1 => self.prod(),
            2 => self.min(),
            3 => self.max(),
            5 => self.gt(),
            6 => self.lt(),
            7 => self.eq(),
            _ => self.value,
        }
    }

    fn sum(&self) -> u64 {
        self.subpackets
            .iter()
            .fold(0, |acc, p| acc + p.total_value())
    }

    fn prod(&self) -> u64 {
        self.subpackets
            .iter()
            .fold(1, |acc, p| acc * p.total_value())
    }

    fn min(&self) -> u64 {
        let mut min = u64::MAX;
        for p in &self.subpackets {
            if p.total_value() < min {
                min = p.total_value();
            }
        }
        min
    }

    fn max(&self) -> u64 {
        let mut max = 0;
        for p in &self.subpackets {
            if p.total_value() > max {
                max = p.total_value();
            }
        }
        max
    }

    fn gt(&self) -> u64 {
        if self.subpackets[0].total_value() > self.subpackets[1].total_value() {
            return 1;
        }
        return 0;
    }

    fn lt(&self) -> u64 {
        if self.subpackets[0].total_value() < self.subpackets[1].total_value() {
            return 1;
        }
        return 0;
    }

    fn eq(&self) -> u64 {
        if self.subpackets[0].total_value() == self.subpackets[1].total_value() {
            return 1;
        }
        return 0;
    }
}

fn parse_packet(input: &str) -> (Packet, usize) {
    let mut current_mode = Mode::Version;
    let mut packet = Packet::default();
    let mut data = input.clone();
    let mut total_read = 0;

    loop {
        let read = match &current_mode {
            Mode::Version => {
                let (version, read) = parse_version(data);
                packet.version = version;
                current_mode = Mode::Type;
                read
            }
            Mode::Type => {
                let (type_id, read) = parse_type_id(data);
                packet.type_id = type_id;
                current_mode = match type_id {
                    4 => Mode::LiteralValue,
                    _ => Mode::Operator,
                };
                read
            }
            Mode::LiteralValue => {
                let (value, read) = parse_literal_value(data);
                packet.value = value;
                current_mode = match packet.type_id {
                    4 => Mode::Done,
                    _ => Mode::Version,
                };
                read
            }
            Mode::Operator => {
                let (subpackets, read) = parse_operator(data);
                packet.subpackets = subpackets;
                current_mode = Mode::Done;
                read
            }
            Mode::Done => {
                return (packet, total_read);
            }
        };

        data = &data[read..];
        total_read += read;
    }
}

fn parse_version(input: &str) -> (u8, usize) {
    let version = u8::from_str_radix(&input[..3], 2).unwrap();
    (version, 3)
}

fn parse_type_id(input: &str) -> (u8, usize) {
    let type_id = u8::from_str_radix(&input[..3], 2).unwrap();
    (type_id, 3)
}

fn parse_literal_value(input: &str) -> (u64, usize) {
    let mut read = 0;
    let mut value = Vec::new();
    let mut data = &input[..];

    loop {
        let last = data.chars().nth(0) == Some('0');
        value.push(&data[1..=4]);
        read += 5;
        data = &data[5..];

        if last {
            break;
        }
    }

    (u64::from_str_radix(&value.join(""), 2).unwrap(), read)
}

fn parse_operator(input: &str) -> (Vec<Packet>, usize) {
    let length_type_id = input.chars().nth(0).unwrap();

    let (packet, read) = match length_type_id {
        '0' => parse_operator_by_bits(&input[1..]),
        '1' => parse_operator_by_count(&input[1..]),
        _ => (Vec::new(), 0),
    };

    (packet, read + 1)
}

fn parse_operator_by_bits(input: &str) -> (Vec<Packet>, usize) {
    let mut packets = Vec::new();
    let mut bits_left = usize::from_str_radix(&input[..15], 2).unwrap();
    let mut data = &input[15..];
    let mut total_read = 15;

    while bits_left > 0 {
        let (packet, read) = parse_packet(data);
        packets.push(packet);
        data = &data[read..];
        total_read += read;
        bits_left -= read;
    }

    (packets, total_read)
}
fn parse_operator_by_count(input: &str) -> (Vec<Packet>, usize) {
    let mut packets = Vec::new();
    let mut packets_left = usize::from_str_radix(&input[..11], 2).unwrap();
    let mut data = &input[11..];
    let mut total_read = 11;

    while packets_left > 0 {
        let (packet, read) = parse_packet(data);
        packets.push(packet);
        data = &data[read..];
        total_read += read;
        packets_left -= 1;
    }

    (packets, total_read)
}

fn hex_to_bin(input: String) -> String {
    let mut bin = Vec::new();

    for c in input.chars() {
        let bin_c = match c {
            '0' => "0000",
            '1' => "0001",
            '2' => "0010",
            '3' => "0011",
            '4' => "0100",
            '5' => "0101",
            '6' => "0110",
            '7' => "0111",
            '8' => "1000",
            '9' => "1001",
            'A' => "1010",
            'B' => "1011",
            'C' => "1100",
            'D' => "1101",
            'E' => "1110",
            'F' => "1111",
            _ => "",
        };
        bin.push(bin_c);
    }

    bin.join("")
}

fn part1(input: &Packet) -> u64 {
    input.version_sum()
}

fn part2(input: &Packet) -> u64 {
    input.total_value()
}

fn parse_input(file_name: &str) -> Vec<Packet> {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);

    let mut packets = Vec::new();

    for line in reader.lines() {
        let line = line.expect("Failed to read line.");
        let bin_line = hex_to_bin(line);
        let (p, _) = parse_packet(&bin_line);
        packets.push(p);
    }

    packets
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut input_file = "input.txt";
    if args.len() == 2 {
        input_file = &args[1];
    }
    let input = parse_input(input_file);

    println!("Part 1: {:?}", part1(&input[0]));
    println!("Part 2: {:?}", part2(&input[0]));
}
