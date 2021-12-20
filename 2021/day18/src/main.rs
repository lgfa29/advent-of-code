use std::cell::RefCell;
use std::env;
use std::fmt;
use std::fs::File;
use std::io::{prelude::*, BufReader};
use std::rc::{Rc, Weak};

enum Direction {
    Left,
    Right,
}

struct SnailNumberIterator {
    stack: Vec<Rc<RefCell<SnailNumber>>>,
}

impl Iterator for SnailNumberIterator {
    type Item = Rc<RefCell<SnailNumber>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.stack.len() == 0 {
            return None;
        }

        let current = match self.stack.pop() {
            None => return None,
            Some(n) => Rc::clone(&n),
        };

        if !current.borrow().right.is_none() {
            self.stack
                .push(Rc::clone(current.borrow().right.as_ref().unwrap()));
        }

        if !current.borrow().left.is_none() {
            self.stack
                .push(Rc::clone(current.borrow().left.as_ref().unwrap()));
        }

        return Some(current);
    }
}

#[derive(Default, Clone)]
struct SnailNumber {
    value: Option<u64>,
    left: Option<Rc<RefCell<Self>>>,
    right: Option<Rc<RefCell<Self>>>,
    parent: Weak<RefCell<Self>>,
}

impl PartialEq for SnailNumber {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
            && self.left == other.left
            && self.right == other.right
            && self.parent.upgrade() == other.parent.upgrade()
    }
}

impl fmt::Debug for SnailNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut result = Vec::new();
        for n in self.iter() {
            if n.borrow().is_leaf() {
                result.push(n.borrow().value.unwrap());
            }
        }
        write!(f, "{:?}", result)
    }
}

impl SnailNumber {
    fn parse(input: &str) -> Option<Rc<RefCell<Self>>> {
        let mut stack: Vec<Rc<RefCell<Self>>> = Vec::new();

        let mut chars = input.chars();
        while let Some(c) = chars.next() {
            match c {
                '[' => {
                    let snail_num = Rc::new(RefCell::new(Self::default()));

                    if stack.len() > 0 {
                        let parent = stack.last().unwrap();
                        snail_num.borrow_mut().parent = Rc::downgrade(parent);

                        if parent.borrow().left.is_none() {
                            parent.borrow_mut().left = Some(Rc::clone(&snail_num));
                        } else {
                            parent.borrow_mut().right = Some(Rc::clone(&snail_num));
                        }
                    }
                    stack.push(snail_num);
                }
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    let num = c.to_digit(10).unwrap();

                    let snail_num = match stack.last() {
                        None => return None,
                        Some(n) => n,
                    };

                    let num_node = Rc::new(RefCell::new(Self {
                        value: Some(num as u64),
                        left: None,
                        right: None,
                        parent: Rc::downgrade(&snail_num),
                    }));

                    if snail_num.borrow().left.is_none() {
                        snail_num.borrow_mut().left = Some(num_node);
                    } else {
                        snail_num.borrow_mut().right = Some(num_node);
                    }
                }
                ']' => {
                    if stack.len() == 1 {
                        break;
                    }
                    let snail_num = stack.pop().unwrap();
                    let parent = stack.last().unwrap();

                    snail_num.borrow_mut().parent = Rc::downgrade(&parent);
                    if parent.borrow().left.is_none() {
                        parent.borrow_mut().left = Some(Rc::clone(&snail_num));
                    } else {
                        parent.borrow_mut().right = Some(Rc::clone(&snail_num));
                    }
                }
                ',' => (),
                _ => return None,
            };
        }

        let root = stack.pop().unwrap();
        match &root.borrow().left {
            None => (),
            Some(l) => l.borrow_mut().parent = Rc::downgrade(&root),
        };
        match &root.borrow().right {
            None => (),
            Some(r) => r.borrow_mut().parent = Rc::downgrade(&root),
        };

        Some(root)
    }

    fn iter(&self) -> SnailNumberIterator {
        let stack = vec![Rc::new(RefCell::new(self.clone()))];
        SnailNumberIterator { stack }
    }

    fn depth(&self) -> u64 {
        let mut depth = 0;
        let mut current = Rc::new(RefCell::new(self.clone()));

        loop {
            let parent = match current.borrow().parent.upgrade() {
                None => break,
                Some(p) => Rc::clone(&p),
            };
            depth += 1;
            current = parent;
        }

        depth
    }

    fn is_zero(&self) -> bool {
        self.value.is_none()
            && self.left.is_none()
            && self.right.is_none()
            && self.parent.upgrade().is_none()
    }

    fn is_leaf(&self) -> bool {
        self.left.is_none() && self.right.is_none()
    }

    fn is_pair(&self) -> bool {
        let left = match &self.left {
            None => return false,
            Some(l) => Rc::clone(&l),
        };

        let right = match &self.right {
            None => return false,
            Some(r) => Rc::clone(&r),
        };

        let left_is_leaf = left.borrow().is_leaf();
        let right_is_leaf = right.borrow().is_leaf();

        left_is_leaf && right_is_leaf
    }

    fn should_explode(&self) -> bool {
        self.depth() == 4 && self.is_pair()
    }
}

fn clone(n: &Rc<RefCell<SnailNumber>>) -> Rc<RefCell<SnailNumber>> {
    let new = Rc::new(RefCell::new(SnailNumber {
        value: n.borrow().value.clone(),
        left: None,
        right: None,
        parent: Weak::new(),
    }));

    if n.borrow().is_leaf() {
        return new;
    }

    let left = match &n.borrow().left {
        None => None,
        Some(left) => {
            let new_left = clone(left);
            new_left.borrow_mut().parent = Rc::downgrade(&new);
            Some(Rc::clone(&new_left))
        }
    };
    new.borrow_mut().left = left;

    let right = match &n.borrow().right {
        None => None,
        Some(right) => {
            let new_right = clone(right);
            new_right.borrow_mut().parent = Rc::downgrade(&new);
            Some(Rc::clone(&new_right))
        }
    };
    new.borrow_mut().right = right;

    new
}

fn add(a: Rc<RefCell<SnailNumber>>, b: Rc<RefCell<SnailNumber>>) -> Rc<RefCell<SnailNumber>> {
    if a.borrow().is_zero() {
        return b;
    }

    if b.borrow().is_zero() {
        return a;
    }

    let sum = Rc::new(RefCell::new(SnailNumber {
        value: None,
        left: Some(Rc::clone(&a)),
        right: Some(Rc::clone(&b)),
        parent: Weak::new(),
    }));

    a.borrow_mut().parent = Rc::downgrade(&sum);
    b.borrow_mut().parent = Rc::downgrade(&sum);

    reduce(Rc::clone(&sum));
    sum
}

fn reduce(n: Rc<RefCell<SnailNumber>>) {
    'main: loop {
        for i in n.borrow().iter() {
            if i.borrow().is_pair() && i.borrow().depth() == 4 {
                explode(Rc::clone(&i));
                continue 'main;
            }
        }

        for i in n.borrow().iter() {
            if i.borrow().is_leaf() && i.borrow().value.unwrap() >= 10 {
                split(Rc::clone(&i));
                continue 'main;
            }
        }

        break;
    }
}

fn explode(n: Rc<RefCell<SnailNumber>>) {
    if !n.borrow().should_explode() {
        return;
    }

    // Handle left value.
    let left_value = match &n.borrow().left {
        None => return,
        Some(l) => l.borrow().value.unwrap(),
    };

    let left_target = find_closest(Rc::clone(&n), Direction::Left);
    match left_target {
        None => (),
        Some(ref t) => {
            let target_value = t.borrow().value.unwrap();
            t.borrow_mut().value = Some(left_value + target_value);
        }
    };

    // Handle right value.
    let right_value = match &n.borrow().right {
        None => return,
        Some(l) => l.borrow().value.unwrap(),
    };

    let right_target = find_closest(Rc::clone(&n), Direction::Right);
    match right_target {
        None => (),
        Some(ref t) => {
            let target_value = t.borrow().value.unwrap();
            t.borrow_mut().value = Some(right_value + target_value);
        }
    };

    // Explode node.
    n.borrow_mut().left = None;
    n.borrow_mut().right = None;
    n.borrow_mut().value = Some(0);
}

fn split(n: Rc<RefCell<SnailNumber>>) {
    let value = match n.borrow().value {
        None => return,
        Some(x) => x,
    };

    n.borrow_mut().left = Some(Rc::new(RefCell::new(SnailNumber {
        value: Some(value / 2),
        left: None,
        right: None,
        parent: Rc::downgrade(&n),
    })));

    n.borrow_mut().right = Some(Rc::new(RefCell::new(SnailNumber {
        value: Some(value / 2 + value % 2),
        left: None,
        right: None,
        parent: Rc::downgrade(&n),
    })));

    n.borrow_mut().value = None;
}

fn find_closest(n: Rc<RefCell<SnailNumber>>, dir: Direction) -> Option<Rc<RefCell<SnailNumber>>> {
    let mut current = Rc::clone(&n);

    // Find common ancestor.
    loop {
        if current.borrow().is_leaf() && Rc::as_ptr(&current) != Rc::as_ptr(&n) {
            return Some(current);
        }

        let parent = match current.borrow().parent.upgrade() {
            None => return None,
            Some(p) => Rc::clone(&p),
        };

        match dir {
            Direction::Left => {
                match &parent.borrow().left {
                    None => (),
                    Some(l) => {
                        if Rc::as_ptr(l) != Rc::as_ptr(&current) {
                            current = Rc::clone(l);
                            break;
                        }
                    }
                };
            }
            Direction::Right => {
                match &parent.borrow().right {
                    None => (),
                    Some(r) => {
                        if Rc::as_ptr(r) != Rc::as_ptr(&current) {
                            current = Rc::clone(r);
                            break;
                        }
                    }
                };
            }
        };

        current = Rc::clone(&parent);
    }

    loop {
        let next = match dir {
            Direction::Left => match &current.borrow().right {
                None => break,
                Some(r) => Rc::clone(&r),
            },
            Direction::Right => match &current.borrow().left {
                None => break,
                Some(l) => Rc::clone(&l),
            },
        };
        current = Rc::clone(&next);
    }

    Some(current)
}

fn magnitude(n: Rc<RefCell<SnailNumber>>) -> u64 {
    let mut sum = 0;
    let value = &n.borrow().value;

    if n.borrow().is_leaf() {
        return value.unwrap();
    }

    sum += 3 * match &n.borrow().left {
        None => value.unwrap(),
        Some(l) => magnitude(Rc::clone(l)),
    };

    sum += 2 * match &n.borrow().right {
        None => value.unwrap(),
        Some(r) => magnitude(Rc::clone(r)),
    };

    sum
}

fn part1(nums: &[Rc<RefCell<SnailNumber>>]) -> u64 {
    let mut sum = clone(&nums[0]);
    for n in nums[1..].iter() {
        sum = add(sum, clone(n));
    }

    magnitude(Rc::clone(&sum))
}

fn part2(nums: &[Rc<RefCell<SnailNumber>>]) -> u64 {
    let mut max = 0;

    for a in nums {
        for b in nums {
            let result = magnitude(add(clone(&a), clone(&b)));
            if result > max {
                max = result;
            }
        }
    }

    max
}

fn parse_input(file_name: &str) -> Vec<Rc<RefCell<SnailNumber>>> {
    let file = File::open(file_name).expect("Failed to read file");
    let reader = BufReader::new(file);
    let mut numbers = Vec::new();

    for line in reader.lines() {
        match SnailNumber::parse(&line.unwrap().trim()) {
            Some(n) => numbers.push(n),
            None => (),
        }
    }

    numbers
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
