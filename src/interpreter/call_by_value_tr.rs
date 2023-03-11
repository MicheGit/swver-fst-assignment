use std::collections::{VecDeque, HashMap};

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
    App
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

    // fn app(to_do: Vec<Term>) -> StackNode {
    //     // TODO 
    //     StackNode.new(to_do, StackOp::App(to_do.len()))
    // }

    // instance methods

    fn ready(&self) -> bool {
        self.to_do.len() == 0
    }

}

fn eval_va(var_env: VarEnv, term: Term) -> i32 {
    eval_va_aux(var_env, term, vec![].into())
}

/// 
/// evaluates a term and appends the result in the head node of the stack
fn eval_va_aux(var_env: VarEnv, term: Term, mut stack: VecDeque<StackNode>) -> i32 {

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
            return eval_va_aux(var_env, *t1, stack);
        },
        Term::Sub(t1, t2) => {
            let node = StackNode::sub(*t2);
            stack.push_front(node);
            return eval_va_aux(var_env, *t1, stack);
        },
        Term::Mul(t1, t2) => {
            let node = StackNode::mul(*t2);
            stack.push_front(node);
            return eval_va_aux(var_env, *t1, stack);
        },
        Term::Brn(t_pred, t_then, t_else) => {
            let node = StackNode::brn(*t_then, *t_else);
            stack.push_front(node);
            return eval_va_aux(var_env, *t_pred, stack);
        },
        Term::App(fn_name, args) => {
            return 0;
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
                    return eval_va_aux(var_env, last_node.to_do.get(0).unwrap().clone(), stack);
                } else {
                    return eval_va_aux(var_env, last_node.to_do.get(1).unwrap().clone(), stack);
                } 
            }
            // .. otherwise we need to compute each of the children ...

            if let Some(next_term) = last_node.to_do.pop_front() {
                // if there are some terms, we need to compute them
                // the parent stays 
                stack.push_front(last_node);
                return eval_va_aux(var_env, next_term, stack);

            } else {
                
                let v = match last_node.op {
                    StackOp::Add => last_node.done.get(1).unwrap() + last_node.done.get(0).unwrap(),
                    StackOp::Sub => last_node.done.get(1).unwrap() - last_node.done.get(0).unwrap(),
                    StackOp::Mul => last_node.done.get(1).unwrap() * last_node.done.get(0).unwrap(),
                    StackOp::Brn => panic!("There should not be any Brn in evaluation"),
                    StackOp::App => panic!("Not implemented")
                    
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
        if let Ok((_, brn)) = expr("if 1 + 10 - 2 * 5 then 21 else 0") {
            assert_eq!(eval_va(VarEnv::new(), brn), 0);   
        } else {
            assert!(false);
        }
    }

    #[test]
    fn eval_va_no_app() {

        let mut var_env = VarEnv::new();
        var_env.update("x".to_owned(), 5);
        var_env.update("y".to_owned(), 7);
        if let Ok((_, brn)) = expr("if x - y then 21 else 5") {
            assert_eq!(eval_va(var_env, brn), 5);   
        } else {
            assert!(false);
        }
    }
}