use std::rc::Rc;

use crate::utils::Program;

use super::{VarEnv, term_tree_to_stack, TermOrOpStack, TermOrOp};

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
fn eval_na_stack(program: &Program, mut stack: TermOrOpStack) -> Option<i32> {

    panic!("TODO");
}