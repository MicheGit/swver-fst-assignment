use std::{collections::{HashMap, LinkedList}, rc::Rc};

use crate::parser_pc::{Term, Decl, expr, parse_program};

// we define N_\bot as Option<i32>

type StateFn = Rc<dyn Fn(String) -> Option<i32>>;

type Program = HashMap<String, Decl>;

fn fn_env_from_decls(decls: Vec<Decl>) -> Program {
    let mut ret = HashMap::new();
    for decl in decls {
        ret.insert(decl.fn_name.clone(), decl.clone());
    }
    ret
}

type FnEnv = HashMap<String, Rc<dyn Fn(&VarEnv) -> Option<i32>>>;

fn functional(p: Program, phi: FnEnv) -> FnEnv {
    let mut ret: FnEnv = HashMap::new();
    for (fn_name, decl) in p {
        let fn_env = phi.clone();
        let expr = decl.expr.clone();
        ret.insert(fn_name, Rc::new(move |params| {

            eval_va(&fn_env, params, expr.clone())

        }));
    }
    ret
}

fn f_bottom(p: &Program) -> FnEnv {
    let mut ret: FnEnv = HashMap::new();
    for (fn_name, _) in p {
        ret.insert(fn_name.clone(), Rc::new(|_| None));
    }
    ret
}

fn call_fn(p: Program, fn_name: String, args: HashMap<String, i32>) -> i32 {
    let var_env: VarEnv = VarEnv { memory: args };
    let mut point: FnEnv = f_bottom(&p);
    let mut result = point.get(&fn_name).unwrap()(&var_env);
    while result == None {
        point = functional(p, point);
        result = point.get(&fn_name).unwrap()(&var_env);
    }
    result.unwrap()
}

#[derive(Debug, Clone)]
pub struct VarEnv {
    memory: HashMap<String, i32>
}

impl VarEnv {
    pub fn new() -> VarEnv {
        VarEnv { memory: HashMap::new() }
    }
    pub fn update(&mut self, arg_name: String, arg_val: i32) -> () {
        self.memory.insert(arg_name, arg_val);
    }
    pub fn lookup(&self, var: &String) -> i32 {
        *self.memory.get(var).unwrap()
    }
}

pub fn eval_va(fn_env: &FnEnv, var_env: &VarEnv, term: Term) -> Option<i32> {
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

            // if function.args.len() != args.len() {
            //     panic!("Function and argument are of different length");
            // }
            // let mut acc_var_env: VarEnv = var_env.clone();
            // for (arg_term, arg_name) in args.iter().zip(function.args.iter()) {
            //     let arg_val = eval_va(fn_env, var_env, arg_term.clone())?;
            //     acc_var_env.update(arg_name.clone(), arg_val);
            // }
            // eval_va(fn_env, &acc_var_env, function.expr.clone())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval_va_num() {
        let var_env = VarEnv::new();
        let fn_env = HashMap::new();
        assert_eq!(eval_va(&fn_env, &var_env, Term::Num(5)), Some(5));
    }

    #[test]
    fn eval_va_var() {
        let mut var_env = VarEnv::new();
        var_env.update("x".to_owned(), 5);
        var_env.update("y".to_owned(), 7);
        
        let fn_env = HashMap::new();
        assert_eq!(eval_va(&fn_env, &var_env, Term::Var("x".to_owned())), Some(5));
        assert_eq!(eval_va(&fn_env, &var_env, Term::Var("y".to_owned())), Some(7));
    }

    #[test]
    fn eval_va_add() {
        let mut var_env = VarEnv::new();
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
        
        let mut var_env = VarEnv::new();
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
        
        let mut var_env = VarEnv::new();
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
        
        let mut var_env = VarEnv::new();
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
        // if let Ok((_, program)) = parse_program("fact(x) = if x then 1 else fact(x-1) * x") {
        //     let fn_env = fn_env_from_decls(program);
        //     if let Ok((_, term)) = expr("fact(5)") {
        //         let var_env = VarEnv::new();
        //         assert_eq!(eval_va(&fn_env, &var_env, term), Some(120))
        //     } else {
        //         assert!(false);
        //     }
        // } else {
            
        //     assert!(false);
        // }

    }
}