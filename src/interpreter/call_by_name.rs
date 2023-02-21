use super::*;

type FnEnv_na = HashMap<String, Rc<dyn Fn(Vec<Term>) -> Option<i32>>>;

#[derive(Debug, Clone)]
pub struct VarEnv_na {
    memory: HashMap<String, Term>
}

impl VarEnv_na {
    pub fn new() -> VarEnv_na {
        VarEnv_na { memory: HashMap::new() }
    }
    pub fn update(&mut self, arg_name: String, arg_val: Term) -> () {
        self.memory.insert(arg_name, arg_val);
    }
    pub fn lookup(&self, var: &String) -> Term {
        match self.memory.get(var) {
            Some(term) => term.clone(),
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
pub fn fix_point_iteration_na(p: &Program, fn_name: String, args: Vec<i32>) -> i32 {
    let mut delta: FnEnv_na = delta_0_na(p);
    loop {
        if let Some(phi_i) = delta.get(&fn_name) {
            let lazy_args = args.iter().map(|var| Term::Num(*var)).collect::<Vec<Term>>();
            println!("fix point {:?}", lazy_args);
            if let Some(n) = phi_i(lazy_args) {
                return n;
            } else {
                delta = functional_na(p, delta);
            }
        } else {
            panic!("The function {} is not defined.", &fn_name);
        }
    }
}

fn delta_0_na(p: &Program) -> FnEnv_na {
    let mut phi: FnEnv_na = HashMap::new();
    for (fn_name, _) in p {
        let bottom = Rc::new(|_| None);
        phi.insert(fn_name.clone(), bottom);
    }
    phi
}


fn functional_na(p: &Program, phi: FnEnv_na) -> FnEnv_na {
    let mut ret: FnEnv_na = HashMap::new();
    for (fn_name, decl) in p {
        let phi_to_move = phi.clone();
        let args_to_move = decl.args.clone();
        let expr_to_move = decl.expr.clone();

        let lambda = Rc::new(move |vs: Vec<Term>| {
            let mut rho = VarEnv_na::new();
            for (val, var) in vs.into_iter().zip(args_to_move.clone()) {
                rho.update(var, val);
            }
            eval_na(&phi_to_move, &rho, expr_to_move.clone())
        });
        ret.insert(fn_name.clone(), lambda);
    }
    ret
}


fn eval_na(fn_env: &FnEnv_na, var_env: &VarEnv_na, term: Term) -> Option<i32> {
    println!("Eval na {:?}", term);
    match term {
        Term::Num(n) => Some(n),
        Term::Var(x) => eval_na(fn_env, var_env, var_env.lookup(&x)),
        Term::Add(t1, t2) => {
            let n1 = eval_na(fn_env, var_env, *t1)?;
            let n2 = eval_na(fn_env, var_env, *t2)?;
            Some(n1 + n2)
        },
        Term::Sub(t1, t2) => {
            let n1 = eval_na(fn_env, var_env, *t1)?;
            let n2 = eval_na(fn_env, var_env, *t2)?;
            Some(n1 - n2)
        },
        Term::Mul(t1, t2) => {
            let n1 = eval_na(fn_env, var_env, *t1)?;
            let n2 = eval_na(fn_env, var_env, *t2)?;
            Some(n1 * n2)
        },
        Term::Brn(t_pred, t_then, t_else) => {
            let n_pred = eval_na(fn_env, var_env, *t_pred)?;
            if n_pred == 0 {
                let n_then = eval_na(fn_env, var_env, *t_then)?;
                Some(n_then)
            } else {
                let n_else = eval_na(fn_env, var_env, *t_else)?;
                Some(n_else)
            }
        },
        Term::App(fn_name, args) => {

            let phi_i_opt = fn_env.get(&fn_name);
            match phi_i_opt {
                Some(phi_i) => phi_i(args),
                None => panic!("The function {} is not defined.", &fn_name)
            }
        }
    }
}