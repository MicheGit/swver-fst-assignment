use std::collections::{VecDeque, HashMap};

use super::Program;

use crate::interpreter::Term;

#[derive(Debug, Clone)]
struct VarEnv {
    memory: HashMap<String, i32>
}

impl VarEnv {
    fn new() -> VarEnv {
        VarEnv { memory: HashMap::new() }
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

#[derive(PartialEq, Eq)]
enum StackOp {
    Add,
    Sub,
    Mul,
    Brn,
    App(String)
}

#[derive(PartialEq, Eq)]
struct StackNode {
    to_do: VecDeque<Term>,
    done: VecDeque<i32>,
    op: StackOp
}

impl StackNode {

    fn new(to_do: Vec<Term>, op: StackOp) -> StackNode {
        StackNode {
            to_do: to_do.into(),
            done: vec![].into(),
            op
        }
    }

    fn add(t2: Term) -> StackNode {
        StackNode::new(vec![t2], StackOp::Add)
    }

    fn sub(t2: Term) -> StackNode {
        StackNode::new(vec![t2], StackOp::Sub)
    }

    fn mul(t2: Term) -> StackNode {
        StackNode::new(vec![t2], StackOp::Mul)
    }

    fn brn(t_then: Term, t_else: Term) -> StackNode {
        StackNode::new(vec![t_then, t_else], StackOp::Brn)
    }

    fn app(fn_name: String, args: Vec<Term>) -> StackNode {
        StackNode::new(args, StackOp::App(fn_name))
    }

    // instance methods

    fn ready(&self) -> bool {
        self.to_do.len() == 0
    }

}

fn eval_va(program: Program, var_env: VarEnv, term: Term) -> i32 {
    eval_va_aux(program, var_env, term, vec![].into())
}

/// 
/// evaluates a term and appends the result in the head node of the stack
fn eval_va_aux(program: Program, var_env: VarEnv, term: Term, mut stack: VecDeque<StackNode>) -> i32 {

    let mut last_computed_val: i32;

    match term {
        Term::Num(n) => {
            last_computed_val = n;
        },
        Term::Var(x) => {
            last_computed_val = var_env.lookup(&x);
        },
        Term::Add(t1, t2) => {
            let node = StackNode::add(*t2);
            stack.push_front(node);
            return eval_va_aux(program, var_env, *t1, stack);
        },
        Term::Sub(t1, t2) => {
            let node = StackNode::sub(*t2);
            stack.push_front(node);
            return eval_va_aux(program, var_env, *t1, stack);
        },
        Term::Mul(t1, t2) => {
            let node = StackNode::mul(*t2);
            stack.push_front(node);
            return eval_va_aux(program, var_env, *t1, stack);
        },
        Term::Brn(t_pred, t_then, t_else) => {
            let node = StackNode::brn(*t_then, *t_else);
            stack.push_front(node);
            return eval_va_aux(program, var_env, *t_pred, stack);
        },
        Term::App(fn_name, args) => {
            if let [head, tail @ ..] = args.as_slice() {
                let node = StackNode::app(fn_name, Vec::from(tail));
                stack.push_front(node);
                return eval_va_aux(program, var_env, head.clone(), stack);
            } else {
                if let Some(decl) = program.get(&fn_name) {
                    let expr = decl.expr.clone();
                    let rho = VarEnv::new();
                    last_computed_val = eval_va_aux(program.clone(), rho, expr, vec![].into());
                } else {
                    panic!("The function {} is not defined.", fn_name);
                }
            }
        }
    };

    loop { // this loop climbs back the right side of the tree (il versante destro dell'albero)
    // this loop converges when stack is not infinite
        if let Some(mut last_node) = stack.pop_front() {

            // each iteration takes the last pending operation (the parent in the tree)
            last_node.done.push_front(last_computed_val);
            // ..and update the child's computation result

            // if it's a branch, then its result is the evaluation of one of the two children
            if last_node.op == StackOp::Brn {
                // we can cut the branch node and substitute it with the child
                // stack.pop_front();
                if *last_node.done.get(0).unwrap() == 0 {
                    return eval_va_aux(program, var_env, last_node.to_do.get(0).unwrap().clone(), stack);
                } else {
                    return eval_va_aux(program, var_env, last_node.to_do.get(1).unwrap().clone(), stack);
                } 
            }
            // .. otherwise we need to compute each of the children ...

            if let Some(next_term) = last_node.to_do.pop_front() {
                // if there are some terms, we need to compute them
                // the parent stays 
                stack.push_front(last_node);
                return eval_va_aux(program, var_env, next_term, stack);

            } else {
                
                let v: i32;
                match last_node.op {
                    StackOp::Add => {
                        v = last_node.done.get(1).unwrap() + last_node.done.get(0).unwrap();
                    },
                    StackOp::Sub => {
                        v = last_node.done.get(1).unwrap() - last_node.done.get(0).unwrap();
                    },
                    StackOp::Mul => {
                        v = last_node.done.get(1).unwrap() * last_node.done.get(0).unwrap();
                    },
                    StackOp::Brn => panic!("There should not be any Brn in evaluation"),
                    StackOp::App(fn_name) => {
                        if let Some(decl) = program.get(&fn_name) {
                            let args = decl.args.clone();
                            let expr = decl.expr.clone();
                            let mut rho = VarEnv::new();
                            for (val, var) in last_node.done.into_iter().zip(args.clone()) {
                                rho.update(var, val);
                            }
                            v = eval_va_aux(program.clone(), rho, expr, vec![].into());
                        } else {
                            panic!("The function {} is not defined.", fn_name);
                        }
                    }
                    
                };
                // stack.pop_front();
                // since we computed the node, we can remove it and..

                // if it was child of another node (i.e. the one that's now the head of the stack)
                // we append the value calculated to it and iterate this loop again
                // "keep climbing"
                last_computed_val = v;
                
            }
        } else {
            // if there is no more tree to climb we just return the value
            return last_computed_val;
        }
    }
}


#[cfg(test)]
mod tests {
    use crate::parser::{expr, parse_program};

    use super::*;

    #[test]
    fn eval_va_no_app_no_var() {
        let program = HashMap::new();
        if let Ok((_, brn)) = expr("if 1 + 10 - 2 * 5 then 21 else 0") {
            assert_eq!(eval_va(program, VarEnv::new(), brn), 0);   
        } else {
            assert!(false);
        }
    }

    #[test]
    fn eval_va_no_app() {

        let program = HashMap::new();
        let mut var_env = VarEnv::new();
        var_env.update("x".to_owned(), 5);
        var_env.update("y".to_owned(), 7);
        if let Ok((_, brn)) = expr("if x - y then 21 else 5") {
            assert_eq!(eval_va(program, var_env, brn), 5);   
        } else {
            assert!(false);
        }
    }

    #[test]
    fn eval_va_mul_brn() {

        let program = HashMap::new();
        let mut var_env = VarEnv::new();
        var_env.update("x".to_owned(), 10);
        var_env.update("y".to_owned(), 1);
        if let Ok((_, brn)) = expr("if x - 10 then 1 + if y then 23 else 20 else 5") {
            assert_eq!(eval_va(program, var_env, brn), 21);   
        } else {
            assert!(false);
        }
    }

    #[test]
    fn eval_va_app() {
        let p = parse_program("fact(x) = if x then 1 else fact(x-1) * x");
        let program = super::super::rec_program_from_decls(p);
        if let Ok((_, app)) = expr("fact(5)") {
            assert_eq!(eval_va(program, VarEnv::new(), app), 120);   
        } else {
            assert!(false);
        }
    }
}