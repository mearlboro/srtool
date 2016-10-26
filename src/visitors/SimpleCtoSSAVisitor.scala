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
  var Pred = "(1 == 1)"
}

/*
 * visits the parse tree while building up the program state
 */
class SimpleCtoSSAVisitor extends SimpleCCodeVisitor {

  // generate new SSA IDs for variables
  val freshGen = new FreshGenerator
  // return the variables changed by a statement
  val modsetVisitor = new ModsetVisitor
  // store the visitor's program state
  var state = new ProgramState
  // making an ass out of u and me
  var assumptions = List[String]()

  // visitors

  override def visitVarDecl(ctx: SimpleCParser.VarDeclContext): String = {
    if (ctx != null) {
      val name = ctx.name.getText
      val nameID = freshGen.fresh(name)
      this.state.M += (name -> nameID)
      return s"int $name$nameID;\n"
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
      return "int "  + newLhs + ";\n" +
        newLhs + " = " + rhs + ";\n"
    }

  override def visitAssertStmt(ctx: SimpleCParser.AssertStmtContext): String =
    if (ctx == null) ""
    else {

      val fancyName = (state.Pred :: assumptions).mkString(" && ")
      val expr = visitExpr(ctx.condition)

      return s"assert !($fancyName) || $expr;"
    }

  override def visitAssumeStmt(ctx: SimpleCParser.AssumeStmtContext): String = {
    if (ctx == null) return ""

    val conditionText = visitExpr(ctx.condition)
    val preds = state.Pred

    val newAssumption =  s"(!($preds) || $conditionText)"

    this.assumptions = newAssumption :: this.assumptions
    return ""
  }

  override def visitHavocStmt(ctx: SimpleCParser.HavocStmtContext): String =
    if (ctx == null) ""
    else {
      val id = ctx.`var`.getText()
      val newID = freshGen.fresh(id)
      state.M.put(id, newID)
      return s"int $id$newID;"
    }

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
          "int " + variable + state.M(variable) + ";\n" +
          variable + state.M(variable) + " = " +
            newPred + " ? " +
            variable + M1.getOrElse(variable, 0) + " : " +
            variable + M2.getOrElse(variable, 0) + ";"
        }).mkString("\n")

      // emit
      return thenCode + elseCode + decisionCode
    }

  override def visitWhileStmt(ctx: SimpleCParser.WhileStmtContext): String= visitChildren(ctx)

  override def visitBlockStmt(ctx: SimpleCParser.BlockStmtContext): String=
    if (ctx == null) "" else visitStatements(ctx.stmts)

  override def visitLoopInvariant(ctx: SimpleCParser.LoopInvariantContext): String= visitChildren(ctx)

  override def visitInvariant(ctx: SimpleCParser.InvariantContext): String= visitChildren(ctx)

  override def visitCandidateInvariant(ctx: SimpleCParser.CandidateInvariantContext): String= visitChildren(ctx)

  override def visitVarrefExpr(ctx: SimpleCParser.VarrefExprContext):String =
    visitIdentifierAndApply(ctx.ID().getSymbol)


  // actual implementation of apply, whenever an identifier is visited it is replaced with its SSA identifier correspondent
  def visitIdentifierAndApply(identifier: org.antlr.v4.runtime.Token): String =
    identifier.getText() + state.M(identifier.getText()).toString()

}
