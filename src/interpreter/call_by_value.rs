use std::{collections::HashMap, rc::Rc};

use super::Program;

use crate::interpreter::Term;

type FnEnv_va = HashMap<String, Rc<dyn Fn(Vec<i32>) -> Option<i32>>>;

#[derive(Debug, Clone)]
struct VarEnv_va {
    memory: HashMap<String, i32>
}

impl VarEnv_va {
    fn new() -> VarEnv_va {
        VarEnv_va { memory: HashMap::new() }
    }
    fn update(&mut self, arg_name: String, arg_val: i32) -> () {
        self.memory.insert(arg_name, arg_val);
    }
    fn lookup(&self, var: &String) -> i32 {
        match self.memory.get(var) {
            Some(n) => *n,
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
pub fn fix_point_iteration_va(p: &Program, fn_name: String, args: Vec<i32>) -> i32 {
    let mut delta: FnEnv_va = delta_0_va(p);
    loop {
        if let Some(phi_i) = delta.get(&fn_name) {
            if let Some(n) = phi_i(args.clone()) {
                return n;
            } else {
                delta = functional_va(p, delta);
            }
        } else {
            panic!("The function {} is not defined.", &fn_name);
        }
    }
}

fn delta_0_va(p: &Program) -> FnEnv_va {
    let mut phi: FnEnv_va = HashMap::new();
    for (fn_name, _) in p {
        let bottom = Rc::new(|_| None);
        phi.insert(fn_name.clone(), bottom);
    }
    phi
}

fn functional_va(p: &Program, phi: FnEnv_va) -> FnEnv_va {
    let mut ret: FnEnv_va = HashMap::new();
    for (fn_name, decl) in p {
        let phi_to_move = phi.clone();
        let args_to_move = decl.args.clone();
        let expr_to_move = decl.expr.clone();

        let lambda = Rc::new(move |vs: Vec<i32>| {
            let mut rho = VarEnv_va::new();
            for (val, var) in vs.into_iter().zip(args_to_move.clone()) {
                rho.update(var, val);
            }
            eval_va(&phi_to_move, &rho, expr_to_move.clone())
        });
        ret.insert(fn_name.clone(), lambda);
    }
    ret
}

fn eval_va(fn_env: &FnEnv_va, var_env: &VarEnv_va, term: Term) -> Option<i32> {
    match term {
        Term::Num(n) => Some(n),
        Term::Var(x) => Some(var_env.lookup(&x)),
        Term::Add(t1, t2) => {
            let n1 = eval_va(fn_env, var_env, *t1)?;
            let n2 = eval_va(fn_env, var_env, *t2)?;
            Some(n1 + n2)
        },
        Term::Sub(t1, t2) => {
            let n1 = eval_va(fn_env, var_env, *t1)?;
            let n2 = eval_va(fn_env, var_env, *t2)?;
            Some(n1 - n2)
        },
        Term::Mul(t1, t2) => {
            let n1 = eval_va(fn_env, var_env, *t1)?;
            let n2 = eval_va(fn_env, var_env, *t2)?;
            Some(n1 * n2)
        },
        Term::Brn(t_pred, t_then, t_else) => {
            let n_pred = eval_va(fn_env, var_env, *t_pred)?;
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
                let v_i = eval_va(fn_env, var_env, arg.clone())?;
                vs.push(v_i);
            }

            let phi_i_opt = fn_env.get(&fn_name);
            match phi_i_opt {
                Some(phi_i) => phi_i(vs),
                None => panic!("The function {} is not defined.", &fn_name)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser_pc::{expr, parse_program};

    use super::*;

    #[test]
    fn eval_va_num() {
        let var_env = VarEnv_va::new();
        let fn_env = HashMap::new();
        assert_eq!(eval_va(&fn_env, &var_env, Term::Num(5)), Some(5));
    }

    #[test]
    fn eval_va_var() {
        let mut var_env = VarEnv_va::new();
        var_env.update("x".to_owned(), 5);
        var_env.update("y".to_owned(), 7);
        
        let fn_env = HashMap::new();
        assert_eq!(eval_va(&fn_env, &var_env, Term::Var("x".to_owned())), Some(5));
        assert_eq!(eval_va(&fn_env, &var_env, Term::Var("y".to_owned())), Some(7));
    }

    #[test]
    fn eval_va_add() {
        let mut var_env = VarEnv_va::new();
        var_env.update("x".to_owned(), 5);
        var_env.update("y".to_owned(), 7);
        
        let fn_env = HashMap::new();
        if let Ok((_, add)) = expr("x + y +1") {
            assert_eq!(eval_va(&fn_env, &var_env, add), Some(13));   
        } else {
            assert!(false);
        }
    }

    #[test]
    fn eval_va_sub() {
        
        let mut var_env = VarEnv_va::new();
        var_env.update("x".to_owned(), 5);
        var_env.update("y".to_owned(), 7);
        
        let fn_env = HashMap::new();
        if let Ok((_, add)) = expr("x - y + 2") {
            assert_eq!(eval_va(&fn_env, &var_env, add), Some(0));   
        } else {
            assert!(false);
        }
    }

    #[test]
    fn eval_va_mul() {
        
        let mut var_env = VarEnv_va::new();
        var_env.update("x".to_owned(), 5);
        var_env.update("y".to_owned(), 7);
        
        let fn_env = HashMap::new();
        if let Ok((_, add)) = expr("x + y * 5") {
            assert_eq!(eval_va(&fn_env, &var_env, add), Some(40));   
        } else {
            assert!(false);
        }
    }

    #[test]
    fn eval_va_brn() {
        
        let mut var_env = VarEnv_va::new();
        var_env.update("x".to_owned(), 5);
        var_env.update("y".to_owned(), 7);
        var_env.update("z".to_owned(), 5);
        
        let fn_env = HashMap::new();
        if let Ok((_, brn)) = expr("if x - z then 21 else 0") {
            assert_eq!(eval_va(&fn_env, &var_env, brn), Some(21));   
        } else {
            assert!(false);
        }
        
        if let Ok((_, brn)) = expr("if x + z then 21 else 0") {
            assert_eq!(eval_va(&fn_env, &var_env, brn), Some(0));   
        } else {
            assert!(false);
        }
    }

    #[test]
    fn eval_va_app() {
        if let Ok((_, p)) = parse_program("fact(x) = if x then 1 else fact(x-1) * x") {
            let program = super::super::rec_program_from_decls(p);
            assert_eq!(fix_point_iteration_va(&program, "fact".to_owned(), vec![5]), 120);
        } else {
            assert!(false);
        }
    }
}