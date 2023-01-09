pub fn nat(input: &str) -> IResult<&str, i32> {
    let (input, digits) = digit1::<&str, Error<&str>>(input)?;
    Ok((input, digits.parse::<i32>().unwrap()))
}

pub fn int(input: &str) -> IResult<&str, i32> {
    nom::character::complete::i32(input)
}

pub fn int_(input: &str) -> IResult<&str, i32> {
    let try_neg = |s: &str| {
        let (input, _) = char('-')(input)?;
        let (input, n) = nat(input)?;
        Ok((input, -n))
    };
    alt((try_neg, nat))(input)
}

mod tests {
    
    #[test]
    fn test_nat() {
        assert_eq!(nat("3"), Ok(("", 3)));
        assert_eq!(nat("3k"), Ok(("k", 3)));
    }

    #[test]
    fn test_int() {
        assert_eq!(int("3  "), Ok(("  ", 3)));
        assert_eq!(int("-3-4"), Ok(("-4", -3)));
        assert!(int("dd3").is_err());
    }
}

// call by value
fn eval_va(term: &Term, fenv: &Vec<Decl>, env: &HashMap<String, Term>) -> i32 {
    match term {
        Term::Num(n) => *n,
        Term::Var(x) => eval_va(env.get(x).unwrap(), fenv, env), // siccome siamo in va dovrebbero essere tutti Term::Num
        Term::Add(t1, t2) => {
            let n1 = eval_va(t1, fenv, env);
            let n2 = eval_va(t2, fenv, env);
            n1 + n2
        },
        Term::Sub(t1, t2) => {
            let n1 = eval_va(t1, fenv, env);
            let n2 = eval_va(t2, fenv, env);
            n1 - n2
        },
        Term::Mul(t1, t2) => {
            let n1 = eval_va(t1, fenv, env);
            let n2 = eval_va(t2, fenv, env);
            n1 * n2
        },
        Term::Brn(pred, t1, t2) => {
            let cond = eval_va(pred, fenv, env);
            if cond == 0 {
                eval_va(t1, fenv, env)
            } else {
                eval_va(t2, fenv, env)
            }
        },
        Term::Fnc(fn_name, args) => {
            let decl = fenv.iter()
                .find(|d| &d.fn_name == fn_name).unwrap();
            let params = decl.args.iter().zip(args);

            let mut state = HashMap::new();
            for (var, arg) in params {
                let v = eval_va(arg, fenv, env);
                state.insert(*var, Term::Num(v));
            }
            eval_va(&decl.term, fenv, &state)
        }
    }
}

/*
Implementazione di [[term]] fenv env che ammette la funzione speciale "bottom" per gestire le funzioni parzialmente definite
    - term      è un termine del linguaggio
    - fenv      è l'ambiente delle funzioni 
    - var_env   è l'insieme dei termini associati alle variabili che appaiono nel termine (in caso di call by value sono tutte Term::Num(n))
*/
fn eval_va_bot(term: &Term, fenv: &RecFEnv, var_env: &RecVarEnv) -> Option<i32> {
    match term {
        Term::Bot => None,
        Term::Num(n) => Some(*n),
        Term::Var(x) => {
            if let Term::Num(n) = var_env.get(x).unwrap() {
                Some(*n)
            } else {
                None
            }
        }, // siccome siamo in va dovrebbero essere tutti Term::Num
        Term::Add(t1, t2) => {
            let n1 = eval_va_bot(t1, fenv, var_env)?;
            let n2 = eval_va_bot(t2, fenv, var_env)?;
            Some(n1 + n2)
        },
        Term::Sub(t1, t2) => {
            let n1 = eval_va_bot(t1, fenv, var_env)?;
            let n2 = eval_va_bot(t2, fenv, var_env)?;
            Some(n1 - n2)
        },
        Term::Mul(t1, t2) => {
            let n1 = eval_va_bot(t1, fenv, var_env)?;
            let n2 = eval_va_bot(t2, fenv, var_env)?;
            Some(n1 * n2)
        },
        Term::Brn(pred, t1, t2) => {
            let cond = eval_va_bot(pred, fenv, var_env)?;
            if cond == 0 {
                eval_va_bot(t1, fenv, var_env)
            } else {
                eval_va_bot(t2, fenv, var_env)
            }
        },
        Term::App(fn_name, args) => {
            
            let mut fenv_: RecFEnv = fenv.clone();
            let decl = fenv.get(fn_name).unwrap();
            let var_env_: RecVarEnv = HashMap::from_iter(decl.args.clone().into_iter().zip(args.clone()));

            // fixpoint iteration goes brrr
            // fn_name(args)    = (FIX F)(args)
            //                  = (\sqcup {F^k(\bot) | k >= 0}) (args)
            //                  = \sqcup {F^k(\bot)(args) | k >= 0}
            // quindi bisogna iterare F^k(\bot)(args) fintanto che il risultato è None

            let fix = functional(Term::Bot, decl);
            fenv_.insert(fn_name.clone(), Decl {
                fn_name: fn_name.clone(),
                args: decl.args,
                term: fix
            });
            let mut res = eval_va_bot(&decl.term, &fenv_, &var_env_);

            while res == None {
                
                let fix = functional(fix, decl);
                fenv_.insert(fn_name.clone(), Decl {
                    fn_name: fn_name.clone(),
                    args: decl.args,
                    term: fix
                });
                let mut res = eval_va_bot(&decl.term, &fenv_, &var_env_);
            }
            res
        }
    }
}