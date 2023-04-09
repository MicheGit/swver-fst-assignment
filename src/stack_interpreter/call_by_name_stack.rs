use std::{rc::Rc, collections::HashMap};

use crate::utils::Program;

use super::{term_tree_to_stack, TermOrOpStack, TermOrOp};

struct LazyEval {
    term: <TermOrOp<LazyEval>>
}

impl LazyEval {
    pub fn new()
}

pub fn fix_point_iteration_na(p: &Program, fn_name: String, args: Vec<i32>) -> i32 {
    if let Some(d_i) = p.get(&fn_name) {

        let decl_args = d_i.args.clone();
        
        let mut rho = VarEnv::new();
        for (val, var) in args.into_iter().zip(decl_args.clone()) {
            rho.update(var, val);
        }

        let var_env = Rc::new(rho);

        
        let mut kleene_index = 0;
        let mut result = None;
        while None == result {
            kleene_index += 1;
            let stack = term_tree_to_stack(d_i.expr.clone(), var_env.clone(), kleene_index);
            result = eval_na_stack(&p, stack.clone());
        }
        result.unwrap()
    } else {
        panic!("The function {} is not defined.", &fn_name);
    }
}

fn eval_na_stack(program: &Program, mut stack: TermOrOpStack) -> Option<i32> {

    let mut delayed: Vec<TermOrOp> = vec![].into();

    while let Some(term_or_op) = stack.pop_front() {
        match term_or_op {
            TermOrOp::Add => {
                let v2 = force_eval(delayed.pop().unwrap());
                let v1 = force_eval(delayed.pop().unwrap());
                delayed.push(TermOrOp::Num(v1 + v2));
            },
            TermOrOp::Sub => {
                let v2 = force_eval(delayed.pop().unwrap());
                let v1 = force_eval(delayed.pop().unwrap());
                delayed.push(TermOrOp::Num(v1 - v2));
            },
            TermOrOp::Mul => {
                let v2 = force_eval(delayed.pop().unwrap());
                let v1 = force_eval(delayed.pop().unwrap());
                delayed.push(TermOrOp::Num(v1 * v2));
            },
            TermOrOp::Brn => {
                let v0 = force_eval(delayed.pop().unwrap());
                let then_branch = stack.pop_front().unwrap();
                let else_branch = stack.pop_front().unwrap();
                if v0 == 0 {
                    stack.push_front(then_branch);
                } else {
                    stack.push_front(else_branch);
                }
            },
            TermOrOp::App(fn_name, var_env, depth_allowed) => {
                if depth_allowed == 0 {
                    // \varphi = \varphi_0
                    return None;
                }
                if let Some(decl) = program.get(&fn_name) {
                    let mut rho = (*var_env).clone();
                    for arg_name in decl.args.iter().rev() {
                        let arg_val = delayed.pop().unwrap();
                        rho.update(arg_name.clone(), LazyEval::new(arg_val, var_env.clone(), depth_allowed));
                    }
                    
                    let mut expanded_function = term_tree_to_stack(decl.expr.clone(), Rc::new(rho), depth_allowed - 1);
                    expanded_function.append(&mut stack);
                    stack = expanded_function;
                } else {
                    panic!("Function named {} is not defined", fn_name);
                }
            },
            _ => {
                delayed.push(term_or_op.clone());
            }
        }
    }
    if let [head] = &delayed[..] {
        return Some(force_eval(*head));
    } else {
        panic!("More computed values when the stack is empty: {:?}.", delayed);
    }
}

fn force_eval(t: TermOrOp) -> i32 {
    match t {
        TermOrOp::Num(n) => n,
        TermOrOp::Var(x, rho) => rho.lookup(&x).deref(),
        _ => panic!("force_eval used with {:?}", t)
    }
}