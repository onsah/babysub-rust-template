use clap::{Arg, ArgAction, Command};
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufWriter, Write};
use std::ops::{Deref, DerefMut};
use std::time::Instant;

mod parser;
mod utils;
mod stats;
mod preprocess;

use stats::Stats;

use crate::parser::parse;
use crate::preprocess::preprocess;

macro_rules! raw_message {
    ($ctx:expr, $($arg:tt)*) => {{
        if $ctx.verbosity >= 0 {
            use std::io::Write;
            if let Err(e) = writeln!($ctx.writer, "{}", format_args!($($arg)*)) {
                die!("Failed to write message: {}", e);
            }
            if let Err(e) = $ctx.writer.flush() {
                die!("Failed to flush writer: {}", e);
            }
        }
    }}
}

struct Config {
    sign: bool,
}

fn average(a: usize, b: usize) -> f64 {
    if b != 0 {
        a as f64 / b as f64
    } else {
        0.0
    }
}

fn percent(a: usize, b: usize) -> f64 {
    100.0 * average(a, b)
}

// TODO: The data structures right now are inefficient and need to be optimized. I will work on
// this in the next few days. - Bernhard

#[derive(Clone)]
struct Clause {
    id: usize,
    literals: Vec<i32>,
    garbage: bool,
}

impl Clause {
    fn new(id: usize, literals: Vec<i32>) -> Clause {
        Clause {
            id,
            literals,
            garbage: false,
        }
    }
}

struct LiteralMatrix(HashMap<i32, Vec<usize>>);

impl LiteralMatrix {
    fn new() -> LiteralMatrix { LiteralMatrix(HashMap::new()) }

    fn connect(&mut self, clause: &Clause) {
        LOG!(
            "adding clause id: {} to literal matrix for literals: {:?}",
            clause_id,
            new_clause.literals
        );
        for &literal in &clause.literals {
            self.0
                .entry(literal)
                .or_insert_with(Vec::new)
                .push(clause.id);
        }
    }
}

impl Deref for LiteralMatrix {
    type Target = HashMap<i32, Vec<usize>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for LiteralMatrix {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

struct CNFFormula {
    variables: usize,
    added_clauses: usize,
    clauses: Vec<Clause>,
    literal_matrix: LiteralMatrix,
}

impl CNFFormula {
    fn new() -> CNFFormula {
        CNFFormula {
            variables: 0,
            added_clauses: 0,
            clauses: Vec::new(),
            literal_matrix: LiteralMatrix::new(),
        }
    }

    fn add_clause(&mut self, clause: Vec<i32>) {
        let clause_id = self.added_clauses;
        self.added_clauses += 1;

        let clause = Clause::new(clause_id, clause);
    
        LOG!(
            "adding clause: {:?} with id: {}",
            new_clause.literals,
            new_clause.id
        );
        self.clauses.push(clause);
    }

    fn reset(&mut self) {
        self.clauses.clear();
        self.literal_matrix.clear();
        self.added_clauses = 0;
    }
}

struct PrintContext {
    writer: BufWriter<Box<dyn Write>>,
    verbosity: i32,
}

impl PrintContext {

    fn new(verbosity: i32, output_path: &str) -> PrintContext {
        let output: Box<dyn Write> = match output_path {
            "<stdout>" => Box::new(io::stdout()),
            path => Box::new(File::create(path).expect("Failed to create output file")),
        };
        let writer: BufWriter<Box<dyn Write>> = BufWriter::new(output);
        
        let mut ptx = PrintContext {
            writer,
            verbosity,
        };
        
        verbose!(ptx, 1, "writing to '{}'", output_path);
        ptx
    }

}

struct SATContext {
    config: Config,
    formula: CNFFormula,
    stats: Stats,
}

impl SATContext {
    fn new(config: Config) -> Self {
        SATContext {
            config,
            formula: CNFFormula::new(),
            stats: Stats {
                checked: 0,
                parsed: 0,
                subsumed: 0,
                start_time: Instant::now(),
            },
        }
    }
}

fn report_stats(ctx: &mut SATContext, ptx: &mut PrintContext) {
    let elapsed_time = ctx.stats.start_time.elapsed().as_secs_f64();
    message!(
        ptx,
        "{:<20} {:>10}    clauses {:.2} per subsumed",
        "checked:",
        ctx.stats.checked,
        average(ctx.stats.subsumed, ctx.stats.subsumed)
    );
    message!(
        ptx,
        "{:<20} {:>10}    clauses {:.0}%",
        "subsumed:",
        ctx.stats.subsumed,
        percent(ctx.stats.subsumed, ctx.stats.parsed)
    );
    message!(ptx, "{:<20} {:13.2} seconds", "process-time:", elapsed_time);
}

fn compute_signature(ctx: &mut SATContext, ptx: &mut PrintContext) -> u64 {
    verbose!(ptx, 1, "computing hash-signature");
    let nonces = [
        71876167, 708592741, 1483128881, 907283241, 442951013, 537146759, 1366999021, 1854614941,
        647800535, 53523743, 783815875, 1643643143, 682599717, 291474505, 229233697, 1633529763,
    ];
    let mut hash: u64 = 0;

    for clause in &ctx.formula.clauses {
        let mut d: Vec<u32> = clause.literals.iter().map(|&lit| lit as u32).collect();
        d.sort_unstable();
        let mut tmp = (d.len() as u64 + 1).wrapping_mul(nonces[0]);
        let mut i = 1usize;

        for &ulit in &d {
            tmp = (tmp << 4) | (tmp >> 60);
            tmp = tmp.wrapping_add(ulit as u64);
            tmp = tmp.wrapping_mul(nonces[i]);
            i = (i + 1) % nonces.len();
        }

        hash = hash.wrapping_add(tmp);
    }

    hash
}

fn print(ctx: &mut SATContext, ptx: &mut PrintContext) {
    
    if ctx.config.sign {
        let signature = compute_signature(ctx, ptx);
        message!(ptx, "hash-signature: {}", signature);
    }

    raw_message!(
        ptx,
        "p cnf {} {}",
        ctx.formula.variables,
        ctx.formula.clauses.len()
    );
    for clause in &ctx.formula.clauses {
        let clause_string = clause
            .literals
            .iter()
            .map(|&lit| lit.to_string())
            .collect::<Vec<String>>()
            .join(" ")
            + " 0";
        raw_message!(ptx, "{}", clause_string);
    }
}
struct Args {
    verbosity: i32,
    sign: bool,
    output_path: String,
    input_path: String,
}

fn get_args() -> Args {
    let app = Command::new("BabySub")
        .version("1.0")
        .author("Bernhard Gstrein")
        .about("Processes and simplifies logical formulae in DIMACS CNF format.")
        .arg(
            Arg::new("input")
                .help("Sets the input file to use")
                .index(1),
        )
        .arg(
            Arg::new("output")
                .help("Sets the output file to use")
                .index(2),
        )
        .arg(
            Arg::new("verbosity")
                .short('v')
                .action(ArgAction::Count)
                .help("Increases verbosity level"),
        )
        .arg(Arg::new("quiet").short('q').help("Suppresses all output"))
        .arg(
            Arg::new("sign")
                .short('s')
                .help("Computes and adds a hash signature to the output"),
        );

    #[cfg(feature = "logging")]
    let app = app.arg(
        Arg::new("logging")
            .short('l')
            .help("Enables detailed logging for debugging")
            .action(ArgAction::SetTrue),
    );

    let matches = app.get_matches();

    #[cfg(not(feature = "logging"))]
    let verbosity = if matches.is_present("quiet") {
        -1
    } else {
        *matches.get_one::<u8>("verbosity").unwrap_or(&0) as i32
    };

    #[cfg(feature = "logging")]
    let verbosity = if matches.is_present("quiet") {
        -1
    } else if matches.get_flag("logging") {
        999
    } else {
        *matches.get_one::<u8>("verbosity").unwrap_or(&0) as i32
    };

    Args {
        input_path: matches.value_of("input").unwrap_or("<stdin>").to_string(),
        output_path: matches.value_of("output").unwrap_or("<stdout>").to_string(),
        sign: matches.is_present("sign"),
        verbosity,
    }
}

fn main() {
    let Args {
        input_path,
        output_path,
        sign,
        verbosity,
    } = get_args();

    let config = Config {
        sign,
    };

    let mut ptx = PrintContext::new(verbosity, &output_path);
    let mut ctx = SATContext::new(config);

    let lines = parse(&input_path);
    preprocess(lines, &mut ctx, &mut ptx);
    print(&mut ctx, &mut ptx);
    report_stats(&mut ctx, &mut ptx);
}
