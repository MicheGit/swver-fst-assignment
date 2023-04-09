use std::collections::HashMap;

use crate::parser::Decl;

pub type Program = HashMap<String, Decl>;

pub fn rec_program_from_decls(decls: Vec<Decl>) -> Program {
    let mut ret = HashMap::new();
    for decl in decls {
        ret.insert(decl.fn_name.clone(), decl.clone());
    }
    ret
}


// VarEnv_va
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarEnv {
    memory: HashMap<String, i32>
}

impl VarEnv {
    pub fn new() -> VarEnv {
        VarEnv { memory: HashMap::new() }
    }
    
    pub fn update(&mut self, arg_name: String, arg_val: i32) -> () {
        self.memory.insert(arg_name, arg_val);
    }

    pub fn lookup(&self, var: &String) -> i32 {
        match self.memory.get(var) {
            Some(n) => *n,
            None => panic!("The variable {} is not defined.", var)
        }
    }
}