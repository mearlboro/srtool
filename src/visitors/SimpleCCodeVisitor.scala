package visitors

/**
  * Created by sam_coope on 22/10/2016.
  */

import parser.SimpleCParser.EnsuresContext
import parser.{SimpleCBaseVisitor, SimpleCParser}
import scala.collection.JavaConversions._


class SimpleCCodeVisitor extends SimpleCBaseVisitor[String]{
  override def visitProgram(ctx: SimpleCParser.ProgramContext): String =
    ctx.globals.map(v => visitVarDecl(v)).mkString("") + "\n" +
    visitProcedureDecl(ctx.procedureDecl)


  override def visitVarDecl(ctx: SimpleCParser.VarDeclContext): String =
    if (ctx == null) "" else "int " + ctx.name.getText() + ";"

  override def visitProcedureDecl(ctx: SimpleCParser.ProcedureDeclContext): String =
   if (ctx == null) "" else
     "int " + ctx.name.getText() + "(" + visitFormalParams(ctx.formals) + ")\n" +
     visitPreposts(ctx.contract) +
     "{\n" +  visitStatements(ctx.stmts) + "\n" +
     "return " + visitExpr(ctx.returnExpr) + ";" +
     "\n}\n"

  override def visitFormalParam(ctx: SimpleCParser.FormalParamContext):String=
  if (ctx == null) "" else "int " + ctx.name.getText()

  override def visitPrepost(ctx: SimpleCParser.PrepostContext):String=
    if (ctx == null) "" else visitChildren(ctx)

  override def visitRequires(ctx: SimpleCParser.RequiresContext):String=
    if (ctx == null) "" else "requires " + visitExpr(ctx.condition)

  override def visitEnsures(ctx: SimpleCParser.EnsuresContext):String=
    if (ctx == null) "" else "ensures " + visitExpr(ctx.condition)

  override def visitStmt(ctx: SimpleCParser.StmtContext):String= visitChildren(ctx)

  override def visitAssignStmt(ctx: SimpleCParser.AssignStmtContext):String=
    if (ctx == null) "" else ctx.lhs.getText() + " = "  + visitExpr(ctx.rhs) + ";"

  override def visitAssertStmt(ctx: SimpleCParser.AssertStmtContext):String=
    if (ctx == null) "" else "assert " + visitExpr(ctx.expr) + ";"

  override def visitAssumeStmt(ctx: SimpleCParser.AssumeStmtContext):String=
    if (ctx == null) "" else "assume " + visitExpr(ctx.condition) + ";"


  override def visitHavocStmt(ctx: SimpleCParser.HavocStmtContext):String=
    if (ctx == null) "" else "havoc " + ctx.`var`.getText() + ";"

  override def visitCallStmt(ctx: SimpleCParser.CallStmtContext):String= visitChildren(ctx)

  override def visitIfStmt(ctx: SimpleCParser.IfStmtContext):String=
    if (ctx == null) "" else
      "if (" + visitExpr(ctx.condition) + ") " +
        visitBlockStmt(ctx.thenBlock) + (
          if (ctx.elseBlock == null) "" else
            "\nelse " + visitBlockStmt(ctx.elseBlock)
        )

  override def visitWhileStmt(ctx: SimpleCParser.WhileStmtContext):String= visitChildren(ctx)

  override def visitBlockStmt(ctx: SimpleCParser.BlockStmtContext):String=
    if (ctx == null) "" else "{\n" + visitStatements(ctx.stmts) + "\n}"

  override def visitLoopInvariant(ctx: SimpleCParser.LoopInvariantContext):String= visitChildren(ctx)

  override def visitInvariant(ctx: SimpleCParser.InvariantContext):String= visitChildren(ctx)

  override def visitCandidateInvariant(ctx: SimpleCParser.CandidateInvariantContext):String= visitChildren(ctx)

  override def visitExpr(ctx: SimpleCParser.ExprContext):String= visitChildren(ctx)

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
    else bracket(ctx.ops.get(0).getText() + visit(ctx.atomExpr()))


  override def visitNumberExpr(ctx: SimpleCParser.NumberExprContext):String =
    ctx.NUMBER().getText()

  override def visitVarrefExpr(ctx: SimpleCParser.VarrefExprContext):String =
    ctx.ID().toString()

  override def visitParenExpr(ctx: SimpleCParser.ParenExprContext):String =
    bracket(visitExpr(ctx.expr()))

  override def visitResultExpr(ctx: SimpleCParser.ResultExprContext): String =
    ctx.resultTok.getText()

  override def visitOldExpr(ctx: SimpleCParser.OldExprContext): String =
    ctx.oldTok.getText()

  def bracket(s: String): String = " (" + s + ") "

  def visitStatements(stmts: java.util.List[SimpleCParser.StmtContext]) =
    stmts.toList.map(visitStmt).mkString("\n")

  def visitPreposts(preposts: java.util.List[SimpleCParser.PrepostContext]) =
    preposts.toList.map(visitPrepost).mkString(", \n")

  def visitFormalParams(params: java.util.List[SimpleCParser.FormalParamContext]) =
    params.toList.map(visitFormalParam).mkString(", ")


}
