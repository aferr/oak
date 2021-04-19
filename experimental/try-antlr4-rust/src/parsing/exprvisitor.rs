#![allow(nonstandard_style)]
// Generated from Expr.g4 by ANTLR 4.8
use antlr_rust::tree::{ParseTreeVisitor};
use super::exprparser::*;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link ExprParser}.
 */
pub trait ExprVisitor<'input>: ParseTreeVisitor<'input,ExprParserContextType>{
	/**
	 * Visit a parse tree produced by {@link ExprParser#prog}.
	 * @param ctx the parse tree
	 */
	fn visit_prog(&mut self, ctx: &ProgContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code op}
	 * labeled alternative in {@link ExprParser#expr}.
	 * @param ctx the parse tree
	 */
	fn visit_op(&mut self, ctx: &OpContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code parens}
	 * labeled alternative in {@link ExprParser#expr}.
	 * @param ctx the parse tree
	 */
	fn visit_parens(&mut self, ctx: &ParensContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code literal}
	 * labeled alternative in {@link ExprParser#expr}.
	 * @param ctx the parse tree
	 */
	fn visit_literal(&mut self, ctx: &LiteralContext<'input>) { self.visit_children(ctx) }


}