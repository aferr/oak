// try blocks are used in antlr4-rust
#![feature(try_blocks)]

mod parsing;
mod ast;
mod datalog_ir;
mod souffle_emitter;
mod lowering_ast_datalog;

use crate::parsing::astconstructionvisitor;
use std::env;
use std::fs;
use crate::souffle_emitter::*;
use crate::lowering_ast_datalog::*;

fn test_cons_ast(filename: &String) {
    let contents = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");
    let prog = astconstructionvisitor::parse_program(&contents[..]);
    println!("ast from {}, \n {:#?}", &filename, prog);
}

fn test_emit_souffle(filename: &String) {
    let source = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");
    let prog = astconstructionvisitor::parse_program(&source[..]);
    let dlir_prog = LoweringToDatalogPass::lower(&prog);
    let souffle_code = SouffleEmitter::emit_program(&dlir_prog);
    println!("souffle code from {}, \n{}", &filename, souffle_code);
}

fn test_input_to_souffle(filename: &String) {
    let source = fs::read_to_string("test_inputs/".to_owned() + filename)
        .expect("Something went wrong reading the file");
    let prog = astconstructionvisitor::parse_program(&source[..]);
    let dlir_prog = LoweringToDatalogPass::lower(&prog);
    let souffle_code = SouffleEmitter::emit_program(&dlir_prog);
    fs::write("test_outputs/".to_owned() + filename + ".dl", souffle_code)
        .expect("failed to write output to file");
}

fn main() {
    for test in ["test1", "test2", "test3"] {
    // for test in ["test2"] {
        println!("compiling: {}", test);
        test_input_to_souffle(&String::from(test));
    }
    // test_input_to_souffle(&String::from("test2"));
    // test_cons_ast(&String::from("test_inputs/input_file"));
    // test_emit_souffle(&String::from("test_inputs/input_file"));
    // test_cons_ast(&String::from("test_inputs/test2"));
    // test_emit_souffle(&String::from("test_inputs/test2"));
    // test_cons_ast(&String::from("test_inputs/test3"));
    // test_emit_souffle(&String::from("test_inputs/test3"));
}
