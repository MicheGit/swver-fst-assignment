use std::{collections::HashMap, rc::Rc};

use crate::parser::{Term, Decl};

type RecVarEnv = HashMap<String, Term>;

type RecFEnv = HashMap<String, RecFunction>;

#[derive(Clone)]
enum RecFunction {
    RecDecl(Decl),
    Function(Rc<dyn Fn(&Vec<Term>) -> Option<i32>>)
}

fn eval_va(term: &Term, fenv: &Rc<RecFEnv>, var_env: &RecVarEnv) -> Option<i32> {
    match term {
        Term::Num(n) => Some(*n),
        Term::Var(x) => {
            if let Term::Num(n) = var_env.get(x).unwrap() {
                Some(*n)
            } else {
                None
            }
        }, // siccome siamo in va dovrebbero essere tutti Term::Num
        Term::Add(t1, t2) => {
            let n1 = eval_va(t1, fenv, var_env)?;
            let n2 = eval_va(t2, fenv, var_env)?;
            Some(n1 + n2)
        },
        Term::Sub(t1, t2) => {
            let n1 = eval_va(t1, fenv, var_env)?;
            let n2 = eval_va(t2, fenv, var_env)?;
            Some(n1 - n2)
        },
        Term::Mul(t1, t2) => {
            let n1 = eval_va(t1, fenv, var_env)?;
            let n2 = eval_va(t2, fenv, var_env)?;
            Some(n1 * n2)
        },
        Term::Brn(pred, t1, t2) => {
            let cond = eval_va(pred, fenv, var_env)?;
            if cond == 0 {
                eval_va(t1, fenv, var_env)
            } else {
                eval_va(t2, fenv, var_env)
            }
        },
        Term::App(fn_name, args) => {
            
            let rec_fn = fenv.get(fn_name).unwrap();
            match rec_fn {
                RecFunction::Function(boxed_fn) => boxed_fn(args),
                RecFunction::RecDecl(decl) => {
                    let term = &decl.term;

                    let functional = |g: Rc<dyn Fn(&Vec<Term>) -> Option<i32>>| {
                        let mut fenv_ = fenv.clone();
                        fenv_.insert(fn_name.clone(), RecFunction::Function(g));
                        Rc::new(|args: &Vec<Term>| {
                            let var_env_: RecVarEnv = HashMap::from_iter(decl.args.clone().into_iter().zip(args.clone()));
                            eval_va(term, &fenv_, &var_env_)
                        })
                    };
                    
                    // fixpoint iteration goes brrr
                    // fn_name(args)    = (FIX F)(args)
                    //                  = (\sqcup {F^k(\bot) | k >= 0}) (args)
                    //                  = \sqcup {F^k(\bot)(args) | k >= 0}
                    // quindi bisogna iterare F^k(\bot)(args) fintanto che il risultato è None

                    let bottom = Rc::new(|_:&Vec<Term>| Option::None);
                    let mut f_n = functional(bottom);
                    let mut res = f_n(args);
                    while res == None {
                        f_n = functional(f_n);
                        res = f_n(args);
                    }
                    res
                }
            }
        }
    }
}


fn big_f(fenv_: RecFEnv) -> RecFEnv {
    let mut fenv = HashMap::new();
    for (fn_name, rec_fn) in fenv_ {
        match rec_fn {
            RecFunction::Function(fun) => {fenv.insert(fn_name, fun);},
            RecFunction::RecDecl(decl) => {
                fenv.insert(fn_name, Rc::new(|args: &Vec<Term>| {
                    eval_va(&decl.term, fenv_, args)
                }));
            }
        }
    }
    fenv
}