mod call_by_value_stack;
mod call_by_name_stack;

use std::collections::{VecDeque, HashMap};
use std::rc::Rc;

use crate::parser::{Decl, Term};
use crate::utils::{rec_program_from_decls, Env};


pub fn run_rec_program_va_opt(decls: Vec<Decl>) -> i32 {
    let program = rec_program_from_decls(decls);
    call_by_value_stack::fix_point_iteration_va(&program, "main".to_owned(), vec![])
}

pub fn run_rec_program_na_opt(decls: Vec<Decl>) -> i32 {
    let program = rec_program_from_decls(decls);
    call_by_name_stack::fix_point_iteration_na(&program, "main".to_owned(), vec![])
}


#[derive(Debug, Clone)]
enum TermOrOp<T> {
    Num(i32),
    Var(String, Rc<Env<T>>),
    Add,
    Sub,
    Mul,
    Brn,
    Block(TermOrOpStack<T>),
    App(String, Rc<Env<T>>, i32),
}

type TermOrOpStack<T> = VecDeque<TermOrOp<T>>;

///
/// Takes a term and returns a stack of terms, and operations.
/// 
/// Terms include only literals or variables, in couple with the variables environment. Operations include sum, difference, product, branching and function applications. 
/// 
/// A branch operation is always followed by two blocks, which are used to group the operations in a same branch, so that's much easier to discard a branch.
/// 
/// Function applications are not expanded: this function converges even if there are infinite recursive calls in the program execution.
fn term_tree_to_stack<T>(term: Term, var_env: Rc<Env<T>>, depth_allowed: i32) -> TermOrOpStack<T> {
    let mut stack: TermOrOpStack = vec![].into();

    match term {
        Term::Num(n) => stack.push_back(TermOrOp::Num(n)),
        Term::Var(x) => stack.push_back(TermOrOp::Var(x, var_env)),
        Term::Add(t1, t2) => {
            let mut expanded = term_tree_to_stack(*t1, var_env.clone(), depth_allowed);
            stack.append(&mut expanded);
            let mut expanded = term_tree_to_stack(*t2, var_env.clone(), depth_allowed);
            stack.append(&mut expanded);
            stack.push_back(TermOrOp::Add);
        },
        Term::Sub(t1, t2) => {
            let mut expanded = term_tree_to_stack(*t1, var_env.clone(), depth_allowed);
            stack.append(&mut expanded);
            let mut expanded = term_tree_to_stack(*t2, var_env.clone(), depth_allowed);
            stack.append(&mut expanded);
            stack.push_back(TermOrOp::Sub);
        },
        Term::Mul(t1, t2) => {
            let mut expanded = term_tree_to_stack(*t1, var_env.clone(), depth_allowed);
            stack.append(&mut expanded);
            let mut expanded = term_tree_to_stack(*t2, var_env.clone(), depth_allowed);
            stack.append(&mut expanded);
            stack.push_back(TermOrOp::Mul);
        },
        Term::Brn(t0, t1, t2) => {
            let mut expanded = term_tree_to_stack(*t0, var_env.clone(), depth_allowed);
            stack.append(&mut expanded);
            stack.push_back(TermOrOp::Brn);
            let expanded = term_tree_to_stack(*t1, var_env.clone(), depth_allowed);
            stack.push_back(TermOrOp::Block(expanded));
            let expanded = term_tree_to_stack(*t2, var_env.clone(), depth_allowed);
            stack.push_back(TermOrOp::Block(expanded));
        },
        Term::App(fn_name, args) => {
            for arg in args {
                let expanded = term_tree_to_stack(arg, var_env.clone(), depth_allowed);
                stack.push_back(TermOrOp::Block(expanded));
            }
            stack.push_back(TermOrOp::App(fn_name, var_env, depth_allowed));
        }
    }

    return stack;
}
