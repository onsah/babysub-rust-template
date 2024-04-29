use std::{collections::HashSet, hash::RandomState, mem::take};

use crate::{parser::CNFLine, Clause, PrintContext, SATContext};

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
                                ctx.formula.clauses.push(Clause::new(0, literals));
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
    let min_lit = ctx.formula.clauses[clause_index]
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
        ctx.formula.clauses[clause_index].literals.iter().map(|l| *l)
    );

    if let Some(min_lit,) = min_lit {
        for clause_mb_subsumed_id in ctx.formula.literal_matrix[min_lit].iter() {
            ctx.stats.checked += 1;
    
            let clause_mb_subsumed = &mut ctx.formula.clauses[*clause_mb_subsumed_id];
    
            let subsumed = clause_mb_subsumed.literals
                .iter()
                .all(|lit| lit_set.contains(lit));
            
            if subsumed {
                clause_mb_subsumed.garbage = true;
                ctx.stats.subsumed += 1;
            }
        }
    }
}

fn backward_subsumption(ctx: &mut SATContext, ptx: &mut PrintContext) {
    verbose!(ptx, 1, "backward_subsumption start");

    ctx.formula.clauses.sort_by(|clause1, clause2| {
        clause1.literals.cmp(&clause2.literals)
    });
    for clause_index in 0..ctx.formula.clauses.len() {
        backward_subsume(clause_index, ctx);
        ctx.formula.literal_matrix.connect(&mut ctx.formula.clauses[clause_index]);
    }

    let clauses = take(&mut ctx.formula.clauses);

    // Idea: Can just mark and not really remove from vec.
    // Delete marked clauses
    ctx.formula.clauses = clauses.into_iter()
        .filter(|clause| !clause.garbage)
        .collect();

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