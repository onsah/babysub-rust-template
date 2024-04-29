use std::{collections::{HashMap, HashSet}, hash::RandomState, ops::{Deref, DerefMut}};

use crate::{parser::CNFLine, PrintContext, SATContext};

// TODO: The data structures right now are inefficient and need to be optimized. I will work on
// this in the next few days. - Bernhard

#[derive(Clone)]
pub struct Clause {
    pub literals: Vec<i32>,
}

impl Clause {
    fn new(literals: Vec<i32>) -> Clause {
        Clause {
            literals,
        }
    }

    fn empty() -> Clause {
        Clause::new(Vec::new())
    }
}

pub struct LiteralMatrix(HashMap<i32, Vec<usize>>);

impl LiteralMatrix {
    fn new() -> LiteralMatrix { LiteralMatrix(HashMap::new()) }

    fn connect(&mut self, clause: &Clause, clause_index: usize) {
        LOG!(
            "adding clause id: {} to literal matrix for literals: {:?}",
            clause.id,
            clause.literals
        );
        for &literal in &clause.literals {
            self.0
                .entry(literal)
                .or_insert_with(Vec::new)
                .push(clause_index);
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

pub struct CNFFormula {
    pub variables: usize,
    pub added_clauses: usize,
    pub clauses: Vec<Option<Clause>>,
    pub literal_matrix: LiteralMatrix,
}

impl CNFFormula {
    pub fn new() -> CNFFormula {
        CNFFormula {
            variables: 0,
            added_clauses: 0,
            clauses: Vec::new(),
            literal_matrix: LiteralMatrix::new(),
        }
    }

    fn add_clause(&mut self, literals: Vec<i32>) {
        self.added_clauses += 1;

        let clause = Clause::new(literals);
    
        LOG!(
            "adding clause: {:?} with id: {}",
            clause.literals,
            clause.id
        );
        self.clauses.push(Some(clause));
    }

    fn reset(&mut self) {
        self.clauses.clear();
        self.literal_matrix.clear();
        self.added_clauses = 0;
    }
}

pub fn preprocess(lines: Box<dyn Iterator<Item = CNFLine>>, ctx: &mut SATContext, ptx: &mut PrintContext) {
    message!(ptx, "BabySub Subsumption Preprocessor");

    // Some only if header is parsed
    let mut clauses_count: Option<usize> = None;
    for (line_number, line) in lines.enumerate() {
        match line {
            CNFLine::Comment => (),
            CNFLine::Header { n_vars, n_clauses } => {
                ctx.formula.variables = n_vars;
                clauses_count = Some(n_clauses);
                LOG!("parsed 'p cnf {} {}' header", n_vars, n_clauses);
            }
            CNFLine::ClauseLine { literals } => {
                match clauses_count.is_some() {
                    true => {
                        if let Some(literals) = trivial_clause(literals) {
                            if literals.is_empty() {
                                ctx.formula.reset();
                                ctx.formula.clauses.push(Some(Clause::empty()));
                                break;
                            } else {
                                ctx.formula.add_clause(literals);
                            }
                        }
                    }
                    false => parse_error!(ptx, "CNF header not found.", line_number)
                }
            }
        }
    }
    
    simplify(ctx, ptx);
}

fn backward_subsume(clause_index: usize, ctx: &mut SATContext) {
    if let Some(clause) = &ctx.formula.clauses[clause_index] {
        let clause = clause.clone();
        // Literal that is contained in least number of clauses.
        let min_lit = clause
            .literals.iter()
            .map(|l| ctx.formula.literal_matrix.get(l).map(|cls| (l, cls.len())))
            .min_by(|lit1, lit2|
                match (lit1, lit2) {
                    (Some((_, len1)), Some((_, len2))) => len1.cmp(&len2),
                    (Some(_), None) => std::cmp::Ordering::Greater,
                    (None, Some(_)) => std::cmp::Ordering::Less,
                    (None, None) => std::cmp::Ordering::Equal,
                }
            )
            .flatten()
            .map(|l| l.0);
    
        let lit_set: HashSet<i32, RandomState> = HashSet::from_iter(
            clause.literals.iter().map(|l| *l)
        );
    
        if let Some(min_lit,) = min_lit {
            for clause_mb_subsumed_id in ctx.formula.literal_matrix[min_lit].iter() {
                ctx.stats.checked += 1;
        
                let clause_mb_subsumed_opt = &mut ctx.formula.clauses[*clause_mb_subsumed_id];
                if let Some(clause_mb_subsumed) = clause_mb_subsumed_opt {
                    // TODO: Use linear check to improve
                    let subsumed = clause_mb_subsumed.literals
                        .iter()
                        .all(|lit| lit_set.contains(lit));
                    
                    if subsumed {
                        *clause_mb_subsumed_opt = None;
                        ctx.stats.subsumed += 1;
                    }
                }        
            }
        }
    }
}

fn backward_subsumption(ctx: &mut SATContext, ptx: &mut PrintContext) {
    verbose!(ptx, 1, "backward_subsumption start");

    ctx.formula.clauses.sort_by(|clause1, clause2| {
        // Since we didn't remove any clause yet, we can safely unwrap.
        clause1.as_ref().unwrap().literals.cmp(&clause2.as_ref().unwrap().literals)
    });
    for clause_index in 0..ctx.formula.clauses.len() {
        verbose!(ptx, 1, "processing clause {}", clause_index);
        backward_subsume(clause_index, ctx);
        // TODO: move literal matrix to the beginning of the for loop. 
        ctx.formula.literal_matrix.connect(ctx.formula.clauses[clause_index].as_mut().unwrap(), clause_index);
    }

    // let clauses = take(&mut ctx.formula.clauses);

    verbose!(ptx, 1, "backward_subsumption complete");
}

fn simplify(ctx: &mut SATContext, ptx: &mut PrintContext) {
    verbose!(ptx, 1, "starting to simplify formula");

    backward_subsumption(ctx, ptx);

    verbose!(ptx, 1, "simplification complete");
}



/** If clause is trivial, returns None
 * Otherwise returns the clause that duplicates are removed
 */
fn trivial_clause(literals: Vec<i32>) -> Option<Vec<i32>> {
    let mut occured = HashSet::<i32>::with_capacity(literals.len());

    let filtered: Vec<i32> = literals.iter().map(|lit| *lit)
        .filter(|literal| {
            if occured.contains(literal) {
                false
            } else {
                occured.insert(*literal);
                true
            }
        })
        .collect();

    if filtered.iter().any(|literal| {
        occured.contains(literal) && occured.contains(& -literal)
    }) {
        None
    } else {
        Some(filtered)
    }
}