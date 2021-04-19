// try blocks are used in antlr4-rust
#![feature(try_blocks)]
mod parsing;
use parsing::printexprvisitor::*;
use parsing::exprlexer::*;
use parsing::exprparser::*;
use antlr_rust::InputStream;
use antlr_rust::common_token_stream::CommonTokenStream;
use crate::parsing::exprvisitor::ExprVisitor;

fn test_print_expr() {
    let mut lexer = ExprLexer::new(
        InputStream::new("3\r\n"));
    let token_source = CommonTokenStream::new(lexer);
    let mut parser = ExprParser::new(token_source);
    let parse_result = parser.prog().expect("failed to parse!");
    let mut pvisitor = PrintExprVisitor::new();
    pvisitor.visit_prog(&parse_result);
}

fn main() {
    test_print_expr();
}
