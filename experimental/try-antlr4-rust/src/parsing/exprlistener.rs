#![allow(nonstandard_style)]
// Generated from Expr.g4 by ANTLR 4.8
use antlr_rust::tree::ParseTreeListener;
use super::exprparser::*;

pub trait ExprListener<'input> : ParseTreeListener<'input,ExprParserContextType>{

/**
 * Enter a parse tree produced by {@link ExprParser#prog}.
 * @param ctx the parse tree
 */
fn enter_prog(&mut self, _ctx: &ProgContext<'input>) { }
/**
 * Exit a parse tree produced by {@link ExprParser#prog}.
 * @param ctx the parse tree
 */
fn exit_prog(&mut self, _ctx: &ProgContext<'input>) { }

/**
 * Enter a parse tree produced by the {@code op}
 * labeled alternative in {@link ExprParser#expr}.
 * @param ctx the parse tree
 */
fn enter_op(&mut self, _ctx: &OpContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code op}
 * labeled alternative in {@link ExprParser#expr}.
 * @param ctx the parse tree
 */
fn exit_op(&mut self, _ctx: &OpContext<'input>) { }

/**
 * Enter a parse tree produced by the {@code parens}
 * labeled alternative in {@link ExprParser#expr}.
 * @param ctx the parse tree
 */
fn enter_parens(&mut self, _ctx: &ParensContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code parens}
 * labeled alternative in {@link ExprParser#expr}.
 * @param ctx the parse tree
 */
fn exit_parens(&mut self, _ctx: &ParensContext<'input>) { }

/**
 * Enter a parse tree produced by the {@code literal}
 * labeled alternative in {@link ExprParser#expr}.
 * @param ctx the parse tree
 */
fn enter_literal(&mut self, _ctx: &LiteralContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code literal}
 * labeled alternative in {@link ExprParser#expr}.
 * @param ctx the parse tree
 */
fn exit_literal(&mut self, _ctx: &LiteralContext<'input>) { }

}
