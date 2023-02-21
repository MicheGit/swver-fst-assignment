// mod parser;
mod parser_pc;
mod interpreter;

fn main() {
    // TODO args
    let filename = "input.txt";
    let path = format!("assets/{}", filename);
    if let Ok(content) = std::fs::read_to_string(path) {
        let content = content.replace("\r\n", "\n");
        let (_, decls) = parser_pc::parse_program(&content).unwrap();
        println!("{}", interpreter::run_rec_program_na(decls.clone()));
        println!("{}", interpreter::run_rec_program_va(decls));
    } else {
        panic!("File {} not found", filename);
    };
}