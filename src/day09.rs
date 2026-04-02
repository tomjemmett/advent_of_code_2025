use std::fs;

pub fn run(file_type: &str) -> Option<(String, String)> {
    if let Ok(_contents) = fs::read_to_string(format!("inputs/{}/09.txt", file_type)) {
        // let input = &parse_input(&contents);
        panic!("Not implemented");
    } else {
        None
    }
}
