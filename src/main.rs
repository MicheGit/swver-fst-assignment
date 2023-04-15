// mod parser;
mod parser;
mod interpreter;
mod stack_interpreter;
mod utils;

use std::env;

///
/// Gets the file and takes care of windows' \r
///
fn get_input_text() -> String {
    let filename = "input.txt";
    let path = format!("assets/{}", filename);
    if let Ok(content) = std::fs::read_to_string(path) {
        content.replace("\r\n", "\n")
    } else {
        panic!("File {} not found", filename);
    }
}

/// Run this program with
///         cargo run [-- {options}]
///     where the options might be:
/**
 * cbv => call by value
 * cbvo => an optimized version of cbv
 * cbn => call by name
 */
/// If no option is given, the program runs only one time in
///  cbvo mode. 
/// 
/// Example: run in call by name mode:
///     cargo run -- cbn
fn main() {
    let content = get_input_text();
    let mut args: Vec<String> = env::args().skip(1).collect();
    if args.len() == 0 {
        args.push("cbvo".to_owned());
    }
    for arg in args { 
        let decls = parser::parse_program(&content);
        match arg.as_str() {
            "cbn" => {
                let result = interpreter::run_rec_program_na(decls);
                println!("Call by name result => {}", result);
            },
            "cbv" => {
                let result = interpreter::run_rec_program_va(decls);
                println!("Call by value result => {}", result);
            },
            "cbvo" => {
                let result = stack_interpreter::run_rec_program_va(decls);
                println!("Call by value (optimized) result => {}", result);
            },
            option => {
                println!("Option {} not recognized", option);
            }
        }
    }
    
}