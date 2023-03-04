use std::{collections::HashMap};

use super::Program;

use crate::interpreter::Term;

/// 
/// Finds the fix point of the functional induced by the program p, 
///  the least one that has the function main sufficiently defined
///  to provide a result.
/// 
pub fn fix_point_iteration_va(p: &Program, fn_name: String, args: Vec<i32>) -> i32 {
    if let Some(d_i) = p.get(&fn_name) {

        let decl_args = d_i.args.clone();
        let decl_expr = d_i.expr.clone();
        
        let mut rho = VarEnv::new();
        for (val, var) in args.into_iter().zip(decl_args.clone()) {
            rho.update(var, val);
        }

        let mut i = 0;

        loop {
            let result = eval_va(i, p, rho.clone(), decl_expr.clone());

            if let Some(n) = result {
                return n;
            }

            i += 1;
        }
    } else {
        panic!("The function {} is not defined.", &fn_name);
    }
}


struct PrintOnDrop(&'static str);

impl Drop for PrintOnDrop {
    fn drop(&mut self) {
        println!("{}", self.0);
    }
}

// VarEnv_va
#[derive(Debug, Clone)]
struct VarEnv {
    memory: HashMap<String, i32>
}

impl VarEnv {
    fn new() -> VarEnv {
        VarEnv { memory: HashMap::new() }
    }
    
    fn update(&mut self, arg_name: String, arg_val: i32) {
        self.memory.insert(arg_name, arg_val);
    }

    fn lookup(&self, var: &String) -> i32 {
        match self.memory.get(var) {
            Some(n) => *n,
            None => panic!("The variable {} is not defined.", var)
        }
    }
}

fn eval_va(kleene_index: u64, p: &Program, var_env: VarEnv, term: Term) -> Option<i32> {
    // println!("Entering eval_va for term {:?} kleene max iterations is {}", &term, kleene_index);
    // let _overwritten = PrintOnDrop("Closed eval_va stack");

    let mut var_env = var_env;
    let mut term = term;
    let mut result = None;

    let mut recursions_done = 1; // parte da 1 perché 0 è sempre \bot

    while recursions_done < kleene_index && result == None { 
        let term_to_move = term.clone();
        let var_env_to_move = var_env.clone();
        result = match term_to_move {
            Term::Num(n) => Some(n),
            Term::Var(x) => Some(var_env_to_move.lookup(&x)),
            Term::Add(t1, t2) => {
                let n1 = eval_va(kleene_index, p, var_env_to_move.clone(), *t1)?;
                let n2 = eval_va(kleene_index, p, var_env_to_move, *t2)?;
                Some(n1 + n2)
            },
            Term::Sub(t1, t2) => {
                let n1 = eval_va(kleene_index, p, var_env_to_move.clone(), *t1)?;
                let n2 = eval_va(kleene_index, p, var_env_to_move, *t2)?;
                Some(n1 - n2)
            },
            Term::Mul(t1, t2) => {
                let n1 = eval_va(kleene_index, p, var_env_to_move.clone(), *t1)?;
                let n2 = eval_va(kleene_index, p, var_env_to_move, *t2)?;
                Some(n1 * n2)
            },
            Term::Brn(t_pred, t_then, t_else) => {
                let n_pred = eval_va(kleene_index, p, var_env_to_move.clone(), *t_pred)?;
                if n_pred == 0 {
                    let n_then = eval_va(kleene_index, p, var_env_to_move, *t_then)?;
                    Some(n_then)
                } else {
                    let n_else = eval_va(kleene_index, p, var_env_to_move, *t_else)?;
                    Some(n_else)
                }
            },
            Term::App(fn_name, args) => {

                if let Some(phi_i) = p.get(&fn_name) {

                    let mut rho = VarEnv::new();
                    for (arg, var) in args.into_iter().zip(phi_i.args.clone()) {
                        let val = eval_va(kleene_index, p, var_env_to_move.clone(), arg.clone())?;
                        rho.update(var, val);
                    }

                    // nella prossima iterazione si usa \varphi_{i-1}
                    var_env = rho;
                    term = phi_i.expr.clone();
                    continue;

                } else {
                    panic!("The function {} is not defined.", &fn_name);
                }
            }
        };
        recursions_done += 1;
    }
    result
}