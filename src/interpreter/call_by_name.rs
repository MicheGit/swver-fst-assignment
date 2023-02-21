use std::fmt::Debug;
use std::{collections::HashMap, rc::Rc};

use super::Program;

use crate::interpreter::Term;

#[derive(Clone)]
struct LazyI32 {
    function: Rc<dyn Fn() -> Option<i32>>,
    result: Option<Option<i32>>
}

impl LazyI32 {

    fn new(function: Rc<dyn Fn() -> Option<i32>>) -> LazyI32 {
        LazyI32 { function, result: None }
    }

    // se è none allora equivale a \bot, ma il calcolo è avvenuto
    fn deref(&mut self) -> Option<i32> {
        match self.result {
            None => {
                let val = (self.function)();
                self.result = Some(val);
                val
            },
            Some(val) => val
        }
    }
}

impl Debug for LazyI32 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LazyI32").field("result", &self.result).finish()
    }
}

type FnEnv_na = HashMap<String, Rc<dyn Fn(Vec<LazyI32>) -> Option<i32>>>;

#[derive(Debug, Clone)]
struct VarEnv_na {
    memory: HashMap<String, LazyI32>
}

impl VarEnv_na {
    pub fn new() -> VarEnv_na {
        VarEnv_na { memory: HashMap::new() }
    }
    fn update(&mut self, arg_name: String, arg_val: LazyI32) -> () {
        self.memory.insert(arg_name, arg_val);
    }
    fn lookup(&self, var: &String) -> LazyI32 {
        match self.memory.get(var) {
            Some(term) => term.clone(),
            None => panic!("The variable {} is not defined.", var)
        }
    }
}

/**
 * 
 * TODO optimize
 * 
 * Finds the fix point of the functional induced by the program p, 
 *  the least one that has the function main defined.
 * 
 * In other words, it unleashes man made horrors beyond comprehension.
 * 
 */
pub fn fix_point_iteration_na(p: &Program, fn_name: String, args: Vec<i32>) -> i32 {
    let mut delta: FnEnv_na = delta_0_na(p);
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

fn delta_0_na(p: &Program) -> FnEnv_na {
    let mut phi: FnEnv_na = HashMap::new();
    for (fn_name, _) in p {
        let bottom = Rc::new(|_| None);
        phi.insert(fn_name.clone(), bottom);
    }
    phi
}


fn functional_na(p: &Program, phi: FnEnv_na) -> FnEnv_na {
    let mut ret: FnEnv_na = HashMap::new();
    for (fn_name, decl) in p {
        let phi_to_move = phi.clone();
        let args_to_move = decl.args.clone();
        let expr_to_move = decl.expr.clone();

        let lambda = Rc::new(move |vs: Vec<LazyI32>| {
            let mut rho = VarEnv_na::new();
            for (val, var) in vs.into_iter().zip(args_to_move.clone()) {
                rho.update(var, val);
            }
            eval_na(&phi_to_move, &rho, expr_to_move.clone())
        });
        ret.insert(fn_name.clone(), lambda);
    }
    ret
}


fn eval_na(fn_env: &FnEnv_na, var_env: &VarEnv_na, term: Term) -> Option<i32> {
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