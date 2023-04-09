use std::rc::Rc;

use crate::utils::{Program, Env};

use super::{term_tree_to_stack, TermOrOp, TermOrOpStack};

type VarEnv = Env<i32>;

pub fn fix_point_iteration_va(p: &Program, fn_name: String, args: Vec<i32>) -> i32 {
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
            result = eval_va_stack(&p, stack.clone());
        }
        result.unwrap()
    } else {
        panic!("The function {} is not defined.", &fn_name);
    }
    
}

///
/// Evaluates the stack given as input with:
/// - the function environment \varphi is represented by the program and the kleene iteration index;
/// - the variable environment is there only when required, i.e. in the evaluation of variable and in function application.
/// 
/// \varphi_i is defined as:
/// - \varphi_0 = {f(args) = \bot, \forall f \in program}
/// - \varphi_i = {f(args) = [[f.declaration]] \varphi_{i-1} \rho[variables/args], \forall f \in program}
/// 
/// It returns:
/// - None if the function environment is not defined enough to get a result;
/// - Some(n) where n is the result of the evaluation.
fn eval_va_stack(program: &Program, mut stack: TermOrOpStack<i32>) -> Option<i32> {

    let mut computed = vec![];
    
    while let Some(term_or_op) = stack.pop_front() {
        match term_or_op {
            TermOrOp::Num(n) => {
                computed.push(n);
            },
            TermOrOp::Var(x, rho) => {
                computed.push(rho.lookup(&x));
            },
            TermOrOp::Add => {
                let v2 = computed.pop().unwrap();
                let v1 = computed.pop().unwrap();
                computed.push(v1 + v2);
            },
            TermOrOp::Sub => {
                let v2 = computed.pop().unwrap();
                let v1 = computed.pop().unwrap();
                computed.push(v1 - v2);
            },
            TermOrOp::Mul => {
                let v2 = computed.pop().unwrap();
                let v1 = computed.pop().unwrap();
                computed.push(v1 * v2);
            },
            TermOrOp::Brn => {
                let v0 = computed.pop().unwrap();
                let then_branch = stack.pop_front().unwrap();
                let else_branch = stack.pop_front().unwrap();
                
                if v0 == 0 {
                    stack.push_front(then_branch);
                } else {
                    stack.push_front(else_branch);
                }
            },
            TermOrOp::Block(mut other) => {
                other.append(&mut stack);
                stack = other;
            },
            TermOrOp::App(fn_name, rho, depth_allowed) => {
                // simulating the kleene index as a limit on the number of function calls
                if depth_allowed == 0 {
                    // \varphi = \varphi_0
                    return None;
                }
                if let Some(decl) = program.get(&fn_name) {
                    let mut rho = (*rho).clone();
                    for arg_name in decl.args.iter().rev() {
                        let arg_val = computed.pop().unwrap();
                        rho.update(arg_name.clone(), arg_val);
                    }
                    
                    let mut expanded_function = term_tree_to_stack(decl.expr.clone(), Rc::new(rho), depth_allowed - 1);
                    expanded_function.append(&mut stack);
                    stack = expanded_function;
                } else {
                    panic!("Function named {} is not defined", fn_name);
                }
            }
        }
    }
    if let [head] = &computed[..] {
        return Some(*head);
    } else {
        panic!("More computed values when the stack is empty: {:?}.", computed);
    }
}


#[cfg(test)]
mod tests {
    use crate::parser::expr;

    use super::*;

    // #[test]
    // fn stack_semantics_test() {
    //     let rho = Rc::new(VarEnv::new());
    //     let expected: TermOrOpStack = [
    //         TermOrOp::Var("x".to_string(), rho.clone()),
    //         TermOrOp::Num(10), 
    //         TermOrOp::Sub, 
    //         TermOrOp::Brn, 
    //         TermOrOp::Block([
    //             TermOrOp::Num(1),
    //             TermOrOp::Var("y".to_string(), rho.clone()), 
    //             TermOrOp::Brn, 
    //             TermOrOp::Block([
    //                 TermOrOp::Num(23)
    //             ].into()), 
    //             TermOrOp::Block([
    //                 TermOrOp::Num(20)
    //             ].into()), 
    //             TermOrOp::Add
    //         ].into()), 
    //         TermOrOp::Block([
    //             TermOrOp::Num(5)
    //         ].into())
    //     ].into();
    //     if let Ok((_, expr)) = expr("if x - 10 then 1 + if y then 23 else 20 else 5") {
    //         assert_eq!(term_tree_to_stack(expr, rho.clone(), 1), expected);
    //     } else {
    //         assert!(false);
    //     }
    // }
}