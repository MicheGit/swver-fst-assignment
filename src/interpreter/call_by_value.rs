use std::{collections::HashMap, rc::Rc};

use super::Program;

use crate::{interpreter::Term, utils::VarEnv};

/// 
/// Finds the fix point of the functional induced by the program p, 
///  the least one that has the function main defined.
/// 
/// In other words, it unleashes man made horrors beyond comprehension.
/// 
pub fn fix_point_iteration_va(p: &Program, fn_name: String, args: Vec<i32>) -> i32 {
    let mut delta: FnEnv = delta_0_va(p);
    loop {
        let phi_i = delta.lookup(&fn_name);
        if let Some(n) = phi_i(args.clone()) {
            return n;
        } else {
            delta = functional_va(p, delta);
        }
    }
}

// FnEnv_va
#[derive(Clone)]
struct FnEnv {
    memory: HashMap<String, Rc<dyn Fn(Vec<i32>) -> Option<i32>>>
}

impl FnEnv {
    pub fn new() -> FnEnv {
        FnEnv { memory: HashMap::new() }
    }
    
    pub fn update(&mut self, arg_name: String, phi: Rc<dyn Fn(Vec<i32>) -> Option<i32>>) -> () {
        self.memory.insert(arg_name, phi);
    }

    pub fn lookup(&self, var: &String) -> Rc<dyn Fn(Vec<i32>) -> Option<i32>> {
        match self.memory.get(var) {
            Some(n) => n.clone(),
            None => panic!("The function {} is not defined.", var)
        }
    }
}

fn delta_0_va(p: &Program) -> FnEnv {
    let mut phi = FnEnv::new();
    for (fn_name, _) in p {
        let bottom = Rc::new(|_| None);
        phi.update(fn_name.clone(), bottom);
    }
    phi
}

fn functional_va(p: &Program, phi: FnEnv) -> FnEnv {
    let mut ret = FnEnv::new();
    for (fn_name, decl) in p {
        let phi_to_move = phi.clone();
        let args_to_move = decl.args.clone();
        let expr_to_move = decl.expr.clone();

        let phi_i = Rc::new(move |vs: Vec<i32>| {
            let mut rho = VarEnv::new();
            for (val, var) in vs.into_iter().zip(args_to_move.clone()) {
                rho.update(var, val);
            }
            eval_va(phi_to_move.clone(), rho.clone(), expr_to_move.clone())
        });
        ret.update(fn_name.clone(), phi_i);
    }
    ret
}

fn eval_va(fn_env: FnEnv, var_env: VarEnv, term: Term) -> Option<i32> {
    match term {
        Term::Num(n) => Some(n),
        Term::Var(x) => Some(var_env.lookup(&x)),
        Term::Add(t1, t2) => {
            let n1 = eval_va(fn_env.clone(), var_env.clone(), *t1)?;
            let n2 = eval_va(fn_env, var_env, *t2)?;
            Some(n1 + n2)
        },
        Term::Sub(t1, t2) => {
            let n1 = eval_va(fn_env.clone(), var_env.clone(), *t1)?;
            let n2 = eval_va(fn_env, var_env, *t2)?;
            Some(n1 - n2)
        },
        Term::Mul(t1, t2) => {
            let n1 = eval_va(fn_env.clone(), var_env.clone(), *t1)?;
            let n2 = eval_va(fn_env, var_env, *t2)?;
            Some(n1 * n2)
        },
        Term::Brn(t_pred, t_then, t_else) => {
            let n_pred = eval_va(fn_env.clone(), var_env.clone(), *t_pred)?;
            if n_pred == 0 {
                let n_then = eval_va(fn_env, var_env, *t_then)?;
                Some(n_then)
            } else {
                let n_else = eval_va(fn_env, var_env, *t_else)?;
                Some(n_else)
            }
        },
        Term::App(fn_name, args) => {

            let mut vs = vec![];
            for arg in args {
                let v_i = eval_va(fn_env.clone(), var_env.clone(), arg.clone())?;
                vs.push(v_i);
            }
            let phi_i = fn_env.lookup(&fn_name);
            phi_i(vs)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{expr, parse_program};

    use super::*;

    #[test]
    fn eval_va_num() {
        let var_env = VarEnv::new();
        let fn_env = FnEnv::new();
        assert_eq!(eval_va(fn_env, var_env, Term::Num(5)), Some(5));
    }

    #[test]
    fn eval_va_var() {
        let mut var_env = VarEnv::new();
        var_env.update("x".to_owned(), 5);
        var_env.update("y".to_owned(), 7);
        
        let fn_env = FnEnv::new();
        assert_eq!(eval_va(fn_env.clone(), var_env.clone(), Term::Var("x".to_owned())), Some(5));
        assert_eq!(eval_va(fn_env, var_env, Term::Var("y".to_owned())), Some(7));
    }

    #[test]
    fn eval_va_add() {
        let mut var_env = VarEnv::new();
        var_env.update("x".to_owned(), 5);
        var_env.update("y".to_owned(), 7);
        
        let fn_env = FnEnv::new();
        if let Ok((_, add)) = expr("x + y +1") {
            assert_eq!(eval_va(fn_env.clone(), var_env.clone(), add), Some(13));   
        } else {
            assert!(false);
        }
    }

    #[test]
    fn eval_va_sub() {
        
        let mut var_env = VarEnv::new();
        var_env.update("x".to_owned(), 5);
        var_env.update("y".to_owned(), 7);
        
        let fn_env = FnEnv::new();
        if let Ok((_, add)) = expr("x - y + 2") {
            assert_eq!(eval_va(fn_env.clone(), var_env.clone(), add), Some(0));   
        } else {
            assert!(false);
        }
    }

    #[test]
    fn eval_va_mul() {
        
        let mut var_env = VarEnv::new();
        var_env.update("x".to_owned(), 5);
        var_env.update("y".to_owned(), 7);
        
        let fn_env = FnEnv::new();
        if let Ok((_, add)) = expr("x + y * 5") {
            assert_eq!(eval_va(fn_env.clone(), var_env.clone(), add), Some(40));   
        } else {
            assert!(false);
        }
    }

    #[test]
    fn eval_va_brn() {
        
        let mut var_env = VarEnv::new();
        var_env.update("x".to_owned(), 5);
        var_env.update("y".to_owned(), 7);
        var_env.update("z".to_owned(), 5);
        
        let fn_env = FnEnv::new();
        if let Ok((_, brn)) = expr("if x - z then 21 else 0") {
            assert_eq!(eval_va(fn_env.clone(), var_env.clone(), brn), Some(21));   
        } else {
            assert!(false);
        }
        
        if let Ok((_, brn)) = expr("if x + z then 21 else 0") {
            assert_eq!(eval_va(fn_env.clone(), var_env.clone(), brn), Some(0));   
        } else {
            assert!(false);
        }
    }

    #[test]
    fn eval_va_app() {
        let p = parse_program("fact(x) = if x then 1 else fact(x-1) * x");
        let program = super::super::rec_program_from_decls(p);
        assert_eq!(fix_point_iteration_va(&program, "fact".to_owned(), vec![5]), 120);
    }
}