package visitors

import parser.{SimpleCBaseVisitor, SimpleCParser}

/**
  * Created by ms6413 on 25/10/16.
  */
class SubstitutionVisitor(val toReplace: String, val replaceWith: String) extends SimpleCCodeVisitor{

  // Expressions
  override def visitTernExpr(ctx: SimpleCParser.TernExprContext): String = visitChildren(ctx)

  override def visitLorExpr(ctx: SimpleCParser.LorExprContext): String =
    if(ctx.getChildCount == 1) super.visitLorExpr(ctx)
    else bracket(visit(ctx.landExpr(0)) + ctx.ops.get(0).getText + visit(ctx.landExpr(1)))

  override def visitLandExpr(ctx: SimpleCParser.LandExprContext):String =
    if(ctx.getChildCount == 1) super.visitLandExpr(ctx)
    else bracket(visit(ctx.borExpr(0)) + ctx.ops.get(0).getText + visit(ctx.borExpr(1)))

  override def visitBorExpr(ctx: SimpleCParser.BorExprContext):String =
    if(ctx.getChildCount == 1) super.visitBorExpr(ctx)
    else bracket(visit(ctx.bxorExpr(0)) + ctx.ops.get(0).getText + visit(ctx.bxorExpr(1)))

  override def visitBxorExpr(ctx: SimpleCParser.BxorExprContext):String =
    if(ctx.getChildCount == 1) super.visitBxorExpr(ctx)
    else bracket(visit(ctx.bandExpr(0)) + ctx.ops.get(0).getText + visit(ctx.bandExpr(1)))

  override def visitBandExpr(ctx: SimpleCParser.BandExprContext):String =
    if(ctx.getChildCount == 1) super.visitBandExpr(ctx)
    else bracket(visit(ctx.equalityExpr(0)) + ctx.ops.get(0).getText + visit(ctx.equalityExpr(1)))

  override def visitEqualityExpr(ctx: SimpleCParser.EqualityExprContext):String =
    if(ctx.getChildCount == 1) super.visitEqualityExpr(ctx)
    else bracket(visit(ctx.relExpr(0)) + ctx.ops.get(0).getText + visit(ctx.relExpr(1)))

  override def visitRelExpr(ctx: SimpleCParser.RelExprContext):String =
    if(ctx.getChildCount == 1) super.visitRelExpr(ctx)
    else bracket(visit(ctx.shiftExpr(0))  + ctx.ops.get(0).getText + visit(ctx.shiftExpr(1)))

  override def visitShiftExpr(ctx: SimpleCParser.ShiftExprContext):String =
    if(ctx.getChildCount == 1) super.visitShiftExpr(ctx)
    else bracket(visit(ctx.addExpr(0)) + ctx.ops.get(0).getText + visit(ctx.addExpr(1)))

  override def visitAddExpr(ctx: SimpleCParser.AddExprContext):String =
    if(ctx.getChildCount == 1) super.visitAddExpr(ctx)
    else bracket(visit(ctx.mulExpr(0)) + ctx.ops.get(0).getText + visit(ctx.mulExpr(1)))

  override def visitMulExpr(ctx: SimpleCParser.MulExprContext):String =
    if(ctx.getChildCount == 1) super.visitMulExpr(ctx)
    else bracket(visit(ctx.unaryExpr(0)) + ctx.ops.get(0).getText + visit(ctx.unaryExpr(1)))

  override def visitUnaryExpr(ctx: SimpleCParser.UnaryExprContext):String =
    if(ctx.getChildCount == 1) super.visitUnaryExpr(ctx)
    else bracket(ctx.ops.get(0) + visit(ctx.atomExpr()))


  override def visitNumberExpr(ctx: SimpleCParser.NumberExprContext):String =
    ctx.NUMBER().toString

  override def visitVarrefExpr(ctx: SimpleCParser.VarrefExprContext):String =
    visitIdentifierAndSubstitute(ctx.ID().getSymbol)

  override def visitParenExpr(ctx: SimpleCParser.ParenExprContext):String =
    bracket(visitExpr(ctx.expr()))


  def bracket(s: String): String = " (" + s + ") "

  // perform the actual substitution
  def visitIdentifierAndSubstitute(identifier: org.antlr.v4.runtime.Token): String = {
    val ident = identifier.getText()
    return (if (ident == toReplace) replaceWith else ident)
  }
}


