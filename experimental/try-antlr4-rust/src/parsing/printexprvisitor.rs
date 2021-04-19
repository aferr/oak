use crate::parsing::exprvisitor::*;
use crate::parsing::exprparser::*;
use antlr_rust::tree::{ParseTree,ParseTreeVisitor};

pub struct PrintExprVisitor {
    ret: String
}

impl PrintExprVisitor {
    pub fn new() -> PrintExprVisitor {
        PrintExprVisitor{ret: String::from("")}
    }
}

impl<'a> ParseTreeVisitor<'a, ExprParserContextType> for PrintExprVisitor {}

impl<'a> ExprVisitor<'a> for PrintExprVisitor {
	fn visit_prog(&mut self, ctx: &ProgContext<'a>) {
        self.visit_children(ctx)
    }

	fn visit_op(&mut self, ctx: &OpContext<'a>) {
        self.visit_children(ctx)
    }

	fn visit_parens(&mut self, ctx: &ParensContext<'a>) {
        self.visit_children(ctx)
    }

	fn visit_literal(&mut self, ctx: &LiteralContext<'a>) {
        println!("{}", ctx.INT().unwrap().get_text());
        self.visit_children(ctx)
    }
}
