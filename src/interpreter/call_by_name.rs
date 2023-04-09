use std::{collections::HashMap, rc::Rc};

use super::Program;

use crate::interpreter::Term;
use crate::utils::{LazyI32, Env};

/// 
/// Finds the fix point of the functional induced by the program p, 
///  the least one that has the function main defined.
/// 
/// In other words, it unleashes man made horrors beyond comprehension.
/// 
pub fn fix_point_iteration_na(p: &Program, fn_name: String, args: Vec<i32>) -> i32 {
    let mut delta: FnEnv = delta_0_na(p);
    loop {
        if let Some(phi_i) = delta.get(&fn_name) {
            let mut lazy_vals = vec![];
            for arg in args.clone() {
                let lazy_val = LazyI32::new(Rc::new(move || {
                    Some(arg)
                }));
                lazy_vals.push(lazy_val);
            }
            if let Some(n) = phi_i(lazy_vals) {
                return n;
            } else {
                delta = functional_na(p, delta);
            }
        } else {
            panic!("The function {} is not defined.", &fn_name);
        }
    }
}

// FnEnv_na
type FnEnv = HashMap<String, Rc<dyn Fn(Vec<LazyI32>) -> Option<i32>>>;

// VarEnv_na
type VarEnv = Env<LazyI32>;


fn delta_0_na(p: &Program) -> FnEnv {
    let mut phi: FnEnv = HashMap::new();
    for (fn_name, _) in p {
        let bottom = Rc::new(|_| None);
        phi.insert(fn_name.clone(), bottom);
    }
    phi
}


fn functional_na(p: &Program, phi: FnEnv) -> FnEnv {
    let mut ret: FnEnv = HashMap::new();
    for (fn_name, decl) in p {
        let phi_to_move = phi.clone();
        let args_to_move = decl.args.clone();
        let expr_to_move = decl.expr.clone();

        let lambda = Rc::new(move |vs: Vec<LazyI32>| {
            let mut rho = VarEnv::new();
            for (val, var) in vs.into_iter().zip(args_to_move.clone()) {
                rho.update(var, val);
            }
            eval_na(&phi_to_move, &rho, expr_to_move.clone())
        });
        ret.insert(fn_name.clone(), lambda);
    }
    ret
}


fn eval_na(fn_env: &FnEnv, var_env: &VarEnv, term: Term) -> Option<i32> {
    match term {
        Term::Num(n) => Some(n),
        Term::Var(x) => var_env.lookup(&x).deref(),
        Term::Add(t1, t2) => {
            let n1 = eval_na(fn_env, var_env, *t1)?;
            let n2 = eval_na(fn_env, var_env, *t2)?;
            Some(n1 + n2)
        },
        Term::Sub(t1, t2) => {
            let n1 = eval_na(fn_env, var_env, *t1)?;
            let n2 = eval_na(fn_env, var_env, *t2)?;
            Some(n1 - n2)
        },
        Term::Mul(t1, t2) => {
            let n1 = eval_na(fn_env, var_env, *t1)?;
            let n2 = eval_na(fn_env, var_env, *t2)?;
            Some(n1 * n2)
        },
        Term::Brn(t_pred, t_then, t_else) => {
            let n_pred = eval_na(fn_env, var_env, *t_pred)?;
            if n_pred == 0 {
                let n_then = eval_na(fn_env, var_env, *t_then)?;
                Some(n_then)
            } else {
                let n_else = eval_na(fn_env, var_env, *t_else)?;
                Some(n_else)
            }
        },
        Term::App(fn_name, args) => {

            let phi_i_opt = fn_env.get(&fn_name);
            let mut lazy_vals = vec![];
            for arg in args {
                
                let var_env_to_move = var_env.clone();
                let fn_env_to_move = fn_env.clone();
                let lazy_val = LazyI32::new(Rc::new(move || {
                    eval_na(&fn_env_to_move.clone(), &var_env_to_move.clone(), arg.clone())
                }));
                lazy_vals.push(lazy_val);
            }
            match phi_i_opt {
                Some(phi_i) => phi_i(lazy_vals),
                None => panic!("The function {} is not defined.", &fn_name)
            }
        }
    }
}