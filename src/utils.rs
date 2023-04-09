use std::{collections::HashMap, rc::Rc, fmt::Debug};

use crate::parser::{Decl, Term};

pub type Program = HashMap<String, Decl>;

pub fn rec_program_from_decls(decls: Vec<Decl>) -> Program {
    let mut ret = HashMap::new();
    for decl in decls {
        ret.insert(decl.fn_name.clone(), decl.clone());
    }
    ret
}

#[derive(Debug, Clone)]
pub struct Env<T: Sized + Clone + Debug> {
    memory: HashMap<String, T>
}

impl<T: Clone + Debug> Env<T> {
    pub fn new() -> Env<T> {
        Env { memory: HashMap::new() }
    }
    pub fn update(&mut self, arg_name: String, arg_val: T) -> () {
        self.memory.insert(arg_name, arg_val);
    }
    pub fn lookup(&self, var: &String) -> T {
        match self.memory.get(var) {
            Some(term) => term.clone(),
            None => panic!("The variable {} is not defined.", var)
        }
    }
} 


#[derive(Clone)]
pub struct LazyI32 {
    function: Rc<dyn Fn() -> Option<i32>>,
    result: Option<Option<i32>>
}

impl LazyI32 {

    pub fn new(function: Rc<dyn Fn() -> Option<i32>>) -> LazyI32 {
        LazyI32 { function, result: None }
    }

    // se è none allora equivale a \bot, ma il calcolo è avvenuto
    pub fn deref(&mut self) -> Option<i32> {
        match self.result {
            None => {
                let val = (self.function)();
                self.result = Some(val);
                val
            },
            Some(val) => val
        }
    }
}

impl Debug for LazyI32 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LazyI32").field("result", &self.result).finish()
    }
}