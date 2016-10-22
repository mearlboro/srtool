package visitors

/**
  * Created by sam_coope on 22/10/2016.
  */

import parser.SimpleCParser.EnsuresContext
import parser.{SimpleCBaseVisitor, SimpleCParser}
import scala.collection.JavaConversions._


class SimpleCCodeVisitor extends SimpleCBaseVisitor[String]{
//  override def visitProgram(ctx: SimpleCParser.ProgramContext): String = visitChildren(ctx)
override def visitProgram(ctx: SimpleCParser.ProgramContext): String = {
  return "// VAR DECL:\n" + visitVarDecl(ctx.varDecl) +
    "\n// PROCEDURES:\n" + visitProcedureDecl(ctx.procedureDecl)

}

  override def visitVarDecl(ctx: SimpleCParser.VarDeclContext): String = {
    if (ctx == null) "" else "int " + ctx.name.getText() + ";"

  }

  override def visitProcedureDecl(ctx: SimpleCParser.ProcedureDeclContext): String =
   if (ctx == null) "NO PROCEDURE\n" else
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
      "if(" + visitExpr(ctx.condition) + ")\n" +
        visitBlockStmt(ctx.thenBlock) + (
          if (ctx.elseBlock == null) "" else
            "\nelse" + visitBlockStmt(ctx.elseBlock)
        )

  override def visitWhileStmt(ctx: SimpleCParser.WhileStmtContext):String= visitChildren(ctx)


  override def visitBlockStmt(ctx: SimpleCParser.BlockStmtContext):String=
    if (ctx == null) "" else "{\n" + visitStatements(ctx.stmts) + "\n}"


  override def visitLoopInvariant(ctx: SimpleCParser.LoopInvariantContext):String= visitChildren(ctx)


  override def visitInvariant(ctx: SimpleCParser.InvariantContext):String= visitChildren(ctx)


  override def visitCandidateInvariant(ctx: SimpleCParser.CandidateInvariantContext):String= visitChildren(ctx)



  // TODO: check that squished things are ok.
  override def visitExpr(ctx: SimpleCParser.ExprContext):String= ctx.getText


  override def visitTernExpr(ctx: SimpleCParser.TernExprContext):String= visitChildren(ctx)

  override def visitLorExpr(ctx: SimpleCParser.LorExprContext):String= visitChildren(ctx)


  override def visitLandExpr(ctx: SimpleCParser.LandExprContext):String= visitChildren(ctx)


  override def visitBorExpr(ctx: SimpleCParser.BorExprContext):String= visitChildren(ctx)


  override def visitBxorExpr(ctx: SimpleCParser.BxorExprContext):String= visitChildren(ctx)


  override def visitBandExpr(ctx: SimpleCParser.BandExprContext):String= visitChildren(ctx)


  override def visitEqualityExpr(ctx: SimpleCParser.EqualityExprContext):String= visitChildren(ctx)


  override def visitRelExpr(ctx: SimpleCParser.RelExprContext):String= visitChildren(ctx)


  override def visitShiftExpr(ctx: SimpleCParser.ShiftExprContext):String= visitChildren(ctx)


  override def visitAddExpr(ctx: SimpleCParser.AddExprContext):String= visitChildren(ctx)

  override def visitMulExpr(ctx: SimpleCParser.MulExprContext):String= visitChildren(ctx)


  override def visitUnaryExpr(ctx: SimpleCParser.UnaryExprContext):String= visitChildren(ctx)


  override def visitAtomExpr(ctx: SimpleCParser.AtomExprContext):String= visitChildren(ctx)


  override def visitNumberExpr(ctx: SimpleCParser.NumberExprContext):String= visitChildren(ctx)


  override def visitVarrefExpr(ctx: SimpleCParser.VarrefExprContext):String= visitChildren(ctx)


  override def visitParenExpr(ctx: SimpleCParser.ParenExprContext):String= {
    return visitChildren(ctx)
  }


  override def visitResultExpr(ctx: SimpleCParser.ResultExprContext):String= {
    return visitChildren(ctx)
  }


  override def visitOldExpr(ctx: SimpleCParser.OldExprContext):String= {
    return visitChildren(ctx)
  }

  def visitStatements(stmts: java.util.List[SimpleCParser.StmtContext]) = stmts.toList.map(visitStmt).mkString("\n")

  def visitPreposts(preposts: java.util.List[SimpleCParser.PrepostContext]) = preposts.toList.map(visitPrepost).mkString(", \n")

  def visitFormalParams(params: java.util.List[SimpleCParser.FormalParamContext]) = params.toList.map(visitFormalParam).mkString(", ")


}
