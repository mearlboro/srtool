package visitors

import parser.SimpleCBaseVisitor
import parser.SimpleCParser._

/**
  * Created by sam_coope on 25/10/2016.
  */
class SimpleCBoolCheckerVisitor extends SimpleCBaseVisitor[Boolean]{


  override def visitExpr(ctx: ExprContext): Boolean =
      super.visitExpr(ctx)

  override def visitTernExpr(ctx: TernExprContext): Boolean =
      if(ctx.getChildCount() == 1) return super.visitTernExpr(ctx)
      else false

  override def visitLorExpr(ctx: LorExprContext): Boolean =
      if(ctx.getChildCount() == 1) return super.visitLorExpr(ctx)
      else true

  override def visitLandExpr(ctx: LandExprContext): Boolean =
      if(ctx.getChildCount() == 1) return super.visitLandExpr(ctx)
      else true

  override def visitBorExpr(ctx: BorExprContext): Boolean =
      if(ctx.getChildCount() == 1) return super.visitBorExpr(ctx)
      else false

  override def visitBxorExpr(ctx: BxorExprContext): Boolean =
      if(ctx.getChildCount() == 1) return super.visitBxorExpr(ctx)
      else false

  override def visitBandExpr(ctx: BandExprContext): Boolean =
      if(ctx.getChildCount() == 1) return super.visitBandExpr(ctx)
      else false

  override def visitEqualityExpr(ctx: EqualityExprContext): Boolean =
      if(ctx.getChildCount() == 1) return super.visitEqualityExpr(ctx)
      else true

  override def visitRelExpr(ctx: RelExprContext): Boolean =
      if(ctx.getChildCount() == 1) return super.visitRelExpr(ctx)
      else true

  override def visitShiftExpr(ctx: ShiftExprContext): Boolean =
      if(ctx.getChildCount() == 1) return super.visitShiftExpr(ctx)
      else false

  override def visitAddExpr(ctx: AddExprContext): Boolean =
      if(ctx.getChildCount() == 1) return super.visitAddExpr(ctx)
      else false

  override def visitMulExpr(ctx: MulExprContext): Boolean =
      if(ctx.getChildCount() == 1) return super.visitMulExpr(ctx)
      else false

  override def visitUnaryExpr(ctx: UnaryExprContext): Boolean =
    if(ctx.getChildCount() == 1) return super.visitUnaryExpr(ctx)
    else ctx.ops.get(0).getText() match {
      case "+" => false
      case "-" => false
      case "!" => true
      case "~" => false
    }

  // unknown
//  override def visitAtomExpr(ctx: AtomExprContext): Boolean =
//if(ctx.getChildCount() == 1) return super.visitAtomExpr(ctx)
//else super.visitAtomExpr(ctx)

  override def visitNumberExpr(ctx: NumberExprContext): Boolean = false

  override def visitVarrefExpr(ctx: VarrefExprContext): Boolean = false

  override def visitParenExpr(ctx: ParenExprContext): Boolean =
      if(ctx.getChildCount() == 1) return super.visitParenExpr(ctx)
      else super.visitExpr(ctx.expr())


  override def visitResultExpr(ctx: ResultExprContext): Boolean =
    throw new RuntimeException("result should not be in ssa form")

  override def visitOldExpr(ctx: OldExprContext): Boolean =
    throw new RuntimeException("old should not be in ssa form")


// NON EXPRESSIONS

  override def visitProgram(ctx: ProgramContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitVarDecl(ctx: VarDeclContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitProcedureDecl(ctx: ProcedureDeclContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitFormalParam(ctx: FormalParamContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitPrepost(ctx: PrepostContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitRequires(ctx: RequiresContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitEnsures(ctx: EnsuresContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitStmt(ctx: StmtContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitAssignStmt(ctx: AssignStmtContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitAssertStmt(ctx: AssertStmtContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitAssumeStmt(ctx: AssumeStmtContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitHavocStmt(ctx: HavocStmtContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitCallStmt(ctx: CallStmtContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitIfStmt(ctx: IfStmtContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitWhileStmt(ctx: WhileStmtContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitBlockStmt(ctx: BlockStmtContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitLoopInvariant(ctx: LoopInvariantContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitInvariant(ctx: InvariantContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")

  override def visitCandidateInvariant(ctx: CandidateInvariantContext): Boolean =
    throw new RuntimeException("visiting a non expression in boolean check visitor.")
}
