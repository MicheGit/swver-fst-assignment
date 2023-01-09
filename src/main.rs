mod parser;
mod interpreter;

fn main() {
    // TODO args
    let filename = "input.txt";
    let path = format!("assets/{}", filename);
    if let Ok(content) = std::fs::read_to_string(path) {
        let content = content.replace("\r\n", "\n");
        println!("{:?}", parser::parse_program(&content));
    } else {
        panic!("File {} not found", filename);
    };
}