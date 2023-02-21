use std::collections::HashMap;

use crate::parser_pc::{Term, Decl};

mod call_by_value;
mod call_by_name;

/**
 * Computes the least fix point of the functional induced by the declarations
 *  given as argument; a least fix point for computing the main function.
 * 
 * This functiion uses a call-by-value strategy.
 */
pub fn run_rec_program_va(decls: Vec<Decl>) -> i32 {
    let program = rec_program_from_decls(decls);
    call_by_value::fix_point_iteration_va(&program, "main".to_owned(), vec![])
}

/**
 * Computes the least fix point of the functional induced by the declarations
 *  given as argument; a least fix point for computing the main function.
 * 
 * This functiion uses a call-by-name strategy.
 */
pub fn run_rec_program_na(decls: Vec<Decl>) -> i32 {
    let program = rec_program_from_decls(decls);
    call_by_name::fix_point_iteration_na(&program, "main".to_owned(), vec![])
}

type Program = HashMap<String, Decl>;

fn rec_program_from_decls(decls: Vec<Decl>) -> Program {
    let mut ret = HashMap::new();
    for decl in decls {
        ret.insert(decl.fn_name.clone(), decl.clone());
    }
    ret
}

