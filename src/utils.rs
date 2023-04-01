use std::{collections::HashMap, rc::Rc};

use crate::parser::{Decl, Term};

pub type Program = HashMap<String, Decl>;

pub fn rec_program_from_decls(decls: Vec<Decl>) -> Program {
    let mut ret = HashMap::new();
    for decl in decls {
        ret.insert(decl.fn_name.clone(), decl.clone());
    }
    ret
}