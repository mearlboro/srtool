package visitors

import parser.{SimpleCBaseVisitor, SimpleCParser}
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap


class FreshGenerator {
  var variableToIndex = HashMap.empty[String, Int]

  def fresh(variable: String): Int = {
    if (variableToIndex.contains(variable)) {
      return variableToIndex(variable) + 1
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
  val modesetVisitor = new ModsetVisitor
  // store the visitor's program state
  var state = new ProgramState


  // visitors
  override def visitProgram(ctx: SimpleCParser.ProgramContext): String =
    visitVarDecl(ctx.varDecl) +
    visitProcedureDecl(ctx.procedureDecl)

  override def visitVarDecl(ctx: SimpleCParser.VarDeclContext): String = {
    if (ctx != null) {
      this.state.M += (ctx.name.getText -> 0)
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
      // create new SSA variable for left hand side
      val lhs = ctx.lhs.getText()
      val newID = freshGen.fresh(lhs)
      val newLhs = lhs + newID
      // update M
      this.state.M += (lhs -> newID)
      // emit
      return newLhs + " = " + visitExpr(ctx.rhs) + ";"
    }

  override def visitAssertStmt(ctx: SimpleCParser.AssertStmtContext): String =
    if (ctx == null) ""
    else "assert " +
         "!(" + this.state.Pred + ") && " +
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
      visitBlockStmt(ctx.thenBlock)
      val M1 = state.M // resulting M' after computing then

      state.M = M.clone()
      state.Pred = Pred + " && !(" + newPred + ")"
      visitBlockStmt(ctx.elseBlock)
      val M2 = state.M // resulting M'' after computing else

      state.M = M // return M to initial state, to be updated below
      val modset = modesetVisitor.visit(ctx)
      println(modset)
      return modset.map(variable => {
        state.M(variable) = freshGen.fresh(variable)
        return variable + state.M(variable) + " = " +
               newPred + " ? " +
               variable + M1(variable) + " : " +
               variable + M2(variable)
      }).mkString("\n")
    }

  override def visitWhileStmt(ctx: SimpleCParser.WhileStmtContext): String= visitChildren(ctx)

  override def visitBlockStmt(ctx: SimpleCParser.BlockStmtContext): String=
    if (ctx == null) "" else "{\n" + visitStatements(ctx.stmts) + "\n}"

  override def visitLoopInvariant(ctx: SimpleCParser.LoopInvariantContext): String= visitChildren(ctx)

  override def visitInvariant(ctx: SimpleCParser.InvariantContext): String= visitChildren(ctx)

  override def visitCandidateInvariant(ctx: SimpleCParser.CandidateInvariantContext): String= visitChildren(ctx)


  // Expressions
  override def visitExpr(ctx: SimpleCParser.ExprContext): String = visitChildren(ctx)

  override def visitTernExpr(ctx: SimpleCParser.TernExprContext): String= visitChildren(ctx)

  override def visitLorExpr(ctx: SimpleCParser.LorExprContext): String= visitChildren(ctx)

  override def visitLandExpr(ctx: SimpleCParser.LandExprContext):String= visitChildren(ctx)

  override def visitBorExpr(ctx: SimpleCParser.BorExprContext):String= visitChildren(ctx)

  override def visitBxorExpr(ctx: SimpleCParser.BxorExprContext):String= visitChildren(ctx)

  override def visitBandExpr(ctx: SimpleCParser.BandExprContext):String= visitChildren(ctx)

  override def visitEqualityExpr(ctx: SimpleCParser.EqualityExprContext):String = visitChildren(ctx)

  override def visitRelExpr(ctx: SimpleCParser.RelExprContext):String= visitChildren(ctx)

  override def visitShiftExpr(ctx: SimpleCParser.ShiftExprContext):String= visitChildren(ctx)

  override def visitAddExpr(ctx: SimpleCParser.AddExprContext):String= visitChildren(ctx)

  override def visitMulExpr(ctx: SimpleCParser.MulExprContext):String= visitChildren(ctx)

  override def visitUnaryExpr(ctx: SimpleCParser.UnaryExprContext):String= visitChildren(ctx)

  override def visitAtomExpr(ctx: SimpleCParser.AtomExprContext):String= visitChildren(ctx)

  override def visitNumberExpr(ctx: SimpleCParser.NumberExprContext):String= visitChildren(ctx)

  override def visitVarrefExpr(ctx: SimpleCParser.VarrefExprContext):String=
    visitIdentifierAndApply(ctx.ID().getSymbol)

  override def visitParenExpr(ctx: SimpleCParser.ParenExprContext):String= visitChildren(ctx)


  // mapping helpers
  def visitStatements(stmts: java.util.List[SimpleCParser.StmtContext]) = stmts.toList.map(visitStmt).mkString("\n")

  def visitPreposts(preposts: java.util.List[SimpleCParser.PrepostContext]) = preposts.toList.map(visitPrepost).mkString(", \n")

  def visitFormalParams(params: java.util.List[SimpleCParser.FormalParamContext]) = params.toList.map(visitFormalParam).mkString(", ")


  // actual implementation of apply, whenever an identifier is visited it is replaced with its SSA identifier correspondent
  def visitIdentifierAndApply(identifier: org.antlr.v4.runtime.Token): String =
    identifier.getText() + state.M(identifier.getText()).toString()

}
