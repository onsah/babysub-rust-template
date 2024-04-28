
use std::{fs::File, io::{self, BufReader, Read, BufRead}};

use crate::utils::LOG;


#[derive(Debug, Eq, PartialEq)]
pub enum CNFLine {
    Header { n_vars: usize, n_clauses: usize },
    ClauseLine { literals: Vec<i32>, },
    Comment,
}

pub fn parse(input_path: String) -> Box<dyn Iterator<Item = CNFLine>> {
    let input: Box<dyn Read> = if input_path == "<stdin>" {
        LOG!("reading from '<stdin>'");
        Box::new(io::stdin())
    } else {
        LOG!("reading from '{}'", &input_path);
        Box::new(File::open(&input_path).unwrap())
    };

    let reader = BufReader::new(input);
    let mut line_number: usize = 0;

    Box::new(reader.lines()
        .map(|l| l.unwrap())
        .map(
            move |line| {
                line_number += 1;
                if line.starts_with('c') {
                    CNFLine::Comment
                } else if line.starts_with("p cnf") {
                    let parts: Vec<&str> = line.split_whitespace().collect();
                    if parts.len() < 4 {
                        panic!("Invalid header format. [line number={}]", line_number);
                    }
                    let n_vars: usize = parts[2].parse().unwrap_or_else(|_| {
                        panic!("Could not read number of variables. [line number={}]", line_number);
                    });
                    let n_clauses = parts[3].parse().unwrap_or_else(|_| {
                        panic!("Could not read number of clauses. [line number={}]", line_number);
                    });

                    LOG!(
                        "parsed 'p cnf {} {}' header",
                        n_vars,
                        n_clauses,
                    );

                    CNFLine::Header { n_vars, n_clauses }
                } else {
                    let literals: Vec<i32> = line
                        .split_whitespace()
                        .map(|num| {
                            num.parse::<i32>().unwrap_or_else(|_| {
                                panic!("Invalid literal format. [line number={}]", line_number);
                            })
                        })
                        .filter(|&x| x != 0)
                        .collect();
                    LOG!("parsed clause: {:?}", &literals);
                    CNFLine::ClauseLine { literals }
                }
            }
        )
    )
}

#[cfg(test)]
mod tests {

    use std::path::Path;

    use super::*;

    #[test]
    fn test_parser_binbin1() {
        let input_path = Path::new("tests/test_cases/binbin1.cnf");

        let lines = parse(input_path.to_str().unwrap().into());   

        assert_eq!(lines.collect::<Vec<CNFLine>>(), vec![
            CNFLine::Header { n_vars: 2, n_clauses: 2 },
            CNFLine::ClauseLine { literals: vec![1, 2] },
            CNFLine::ClauseLine { literals: vec![2, 1] },
        ]);
    }

}
