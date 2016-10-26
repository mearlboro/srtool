package visitors

import parser.SimpleCParser.VarDeclContext
import parser.{SimpleCBaseVisitor, SimpleCParser}

import scala.collection.JavaConversions._

/*
 * a visitor for statements to calculate the set of modified variables Modset
 */
class ModsetVisitor extends SimpleCBaseVisitor[Set[String]] {

  def visitStatements(ctx: java.util.List[SimpleCParser.StmtContext]): Set[String] =
     ctx.foldLeft(Set.empty[String])((s, t) => s union visitStmt(t))

  override def visitAssignStmt(ctx: SimpleCParser.AssignStmtContext): Set[String] =
    if (ctx == null) Set.empty[String]
    else Set(ctx.ID().toString)

  override def visitAssertStmt(ctx: SimpleCParser.AssertStmtContext): Set[String] = Set.empty[String]

  override def visitAssumeStmt(ctx: SimpleCParser.AssumeStmtContext): Set[String] = Set.empty[String]

  override def visitHavocStmt(ctx: SimpleCParser.HavocStmtContext): Set[String] = Set.empty[String]

  override def visitCallStmt(ctx: SimpleCParser.CallStmtContext): Set[String] = Set.empty[String]

  override def visitVarDecl(ctx: VarDeclContext): Set[String] = if (ctx.getChildCount == 1) super.visitVarDecl(ctx) else Set.empty[String]

  override def visitIfStmt(ctx: SimpleCParser.IfStmtContext): Set[String] =
    if (ctx == null) Set.empty[String]
    else visitBlockStmt(ctx.thenBlock) union (
        if (ctx.elseBlock == null) visitBlockStmt(ctx.elseBlock) else  Set.empty[String]
      )

  override def visitWhileStmt(ctx: SimpleCParser.WhileStmtContext): Set[String] = Set.empty[String]

  override def visitBlockStmt(ctx: SimpleCParser.BlockStmtContext): Set[String] =
    if (ctx == null) Set.empty[String]
    else visitStatements(ctx.stmt())
}

