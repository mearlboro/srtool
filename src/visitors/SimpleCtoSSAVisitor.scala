package visitors

import parser.{SimpleCBaseVisitor, SimpleCParser}
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap


class FreshGenerator {
  var variableToIndex = HashMap.empty[String, Int]

  def fresh(variable: String): Int = {
    if (variableToIndex.contains(variable)) {
      variableToIndex(variable) += 1
      return variableToIndex(variable)
    }
    else {
      variableToIndex.put(variable, 0)
      return 0
    }
  }
}


/*
 * State ::= ( Pred, M )
 * Pred := a conjunction of boolean predicates
 * M    := a mapping of variables and their SSA ID at the current node
 */
class ProgramState {
  var M    = HashMap.empty[String, Int]
  var Pred = "true"
}


/*
 * visits the parse tree while building up the program state
 */
class SimpleCtoSSAVisitor extends SimpleCBaseVisitor[String] {

  // generate new SSA IDs for variables
  val freshGen = new FreshGenerator
  // return the variables changed by a statement
  val modsetVisitor = new ModsetVisitor
  // store the visitor's program state
  var state = new ProgramState


  // visitors
  override def visitProgram(ctx: SimpleCParser.ProgramContext): String =
    visitVarDecl(ctx.varDecl) +
    visitProcedureDecl(ctx.procedureDecl)

  override def visitVarDecl(ctx: SimpleCParser.VarDeclContext): String = {
    if (ctx != null) {
      val name = ctx.name.getText
      this.state.M += (name -> 0)
      return "int " + name + "0;"
    }
    return ""
  }

  override def visitProcedureDecl(ctx: SimpleCParser.ProcedureDeclContext): String =
   if (ctx == null) ""
   else
     "int " + ctx.name.getText() + "(" + visitFormalParams(ctx.formals) + ")\n" +
     visitPreposts(ctx.contract) +
     "{\n" +  visitStatements(ctx.stmts) + "\n" +
     "return " + visitExpr(ctx.returnExpr) + ";" +
     "\n}\n"

  override def visitFormalParam(ctx: SimpleCParser.FormalParamContext): String =
    if (ctx == null) return ""
    else {
      this.state.M += (ctx.name.getText() -> 0)
      return "int " + ctx.name.getText() + this.state.M(ctx.name.getText())
    }

  override def visitPrepost(ctx: SimpleCParser.PrepostContext): String=
    if (ctx == null) "" else visitChildren(ctx)

  override def visitRequires(ctx: SimpleCParser.RequiresContext): String=
    if (ctx == null) "" else "requires " + visitExpr(ctx.condition)

  override def visitEnsures(ctx: SimpleCParser.EnsuresContext): String=
    if (ctx == null) "" else "ensures " + visitExpr(ctx.condition)


  // Statements
  override def visitStmt(ctx: SimpleCParser.StmtContext): String = visitChildren(ctx)

  override def visitAssignStmt(ctx: SimpleCParser.AssignStmtContext): String =
    if (ctx == null) return ""
    else {
      // process right hand side before left in case it uses the same var
      val rhs = visitExpr(ctx.rhs)
      // create new SSA variable for left hand side
      val lhs = ctx.lhs.getText()
      val newID = freshGen.fresh(lhs)
      val newLhs = lhs + newID
      // update M
      this.state.M += (lhs -> newID)
      // emit
      return newLhs + " = " + rhs + ";"
    }

  override def visitAssertStmt(ctx: SimpleCParser.AssertStmtContext): String =
    if (ctx == null) ""
    else "assert " +
         "!(" + this.state.Pred + ") || " +
         visitExpr(ctx.expr) + ";"

  override def visitAssumeStmt(ctx: SimpleCParser.AssumeStmtContext): String =
    if (ctx == null) "" else "assume " + visitExpr(ctx.condition) + ";"

  override def visitHavocStmt(ctx: SimpleCParser.HavocStmtContext): String =
    if (ctx == null) "" else "havoc " + ctx.`var`.getText() + ";"

  override def visitCallStmt(ctx: SimpleCParser.CallStmtContext): String = visitChildren(ctx)

  override def visitIfStmt(ctx: SimpleCParser.IfStmtContext): String =
    if (ctx == null) return ""
    else
    {
      val newPred = visitExpr(ctx.condition)
      val M = state.M.clone()
      val Pred = state.Pred

      state.Pred = Pred + " && " + newPred
      val thenCode = visit(ctx.thenBlock)
      val M1 = state.M.clone() // resulting M' after computing then

      var M2 = M.clone()
      var elseCode = ""
      if(ctx.elseBlock != null) {
        state.M = M.clone()
        state.Pred = Pred + " && !(" + newPred + ")"
        elseCode = visit(ctx.elseBlock)
        M2 = state.M.clone() // resulting M'' after computing else
      }

      state.M = M.clone() // return M to initial state, to be updated below

      // get modified vars
      val modset = modsetVisitor.visit(ctx)
      val decisionCode = modset.map(variable => {
          state.M(variable) = freshGen.fresh(variable)
          variable + state.M(variable) + " = " +
            newPred + " ? " +
            variable + M1(variable) + " : " +
            variable + M2(variable) + ";"
        }).mkString("\n")

      // emit
      return thenCode + elseCode + decisionCode
    }

  override def visitWhileStmt(ctx: SimpleCParser.WhileStmtContext): String= visitChildren(ctx)

  override def visitBlockStmt(ctx: SimpleCParser.BlockStmtContext): String=
    if (ctx == null) "" else "{\n" + visitStatements(ctx.stmts) + "\n}\n"

  override def visitLoopInvariant(ctx: SimpleCParser.LoopInvariantContext): String= visitChildren(ctx)

  override def visitInvariant(ctx: SimpleCParser.InvariantContext): String= visitChildren(ctx)

  override def visitCandidateInvariant(ctx: SimpleCParser.CandidateInvariantContext): String= visitChildren(ctx)


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
    visitIdentifierAndApply(ctx.ID().getSymbol)

  override def visitParenExpr(ctx: SimpleCParser.ParenExprContext):String =
    bracket(visitExpr(ctx.expr()))


  def bracket(s: String): String = " (" + s + ") "

  // mapping helpers
  def visitStatements(stmts: java.util.List[SimpleCParser.StmtContext]) = stmts.toList.map(visitStmt).mkString("\n")

  def visitPreposts(preposts: java.util.List[SimpleCParser.PrepostContext]) = preposts.toList.map(visitPrepost).mkString(", \n")

  def visitFormalParams(params: java.util.List[SimpleCParser.FormalParamContext]) = params.toList.map(visitFormalParam).mkString(", ")


  // actual implementation of apply, whenever an identifier is visited it is replaced with its SSA identifier correspondent
  def visitIdentifierAndApply(identifier: org.antlr.v4.runtime.Token): String =
    identifier.getText() + state.M(identifier.getText()).toString()

}
