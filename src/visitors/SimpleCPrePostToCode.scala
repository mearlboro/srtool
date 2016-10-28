package visitors

import java.util

import scala.collection.JavaConversions._
import parser.SimpleCParser.{OldExprContext, PrepostContext}
import parser.SimpleCParser

/**
  * Created by sam_coope on 24/10/2016.
  */
class SimpleCPrePostToCode extends SimpleCCodeVisitor {

  var requireExprs = List[String]()
  var ensureExprs = List[String]()

  var varsAreGlobals = false
  var oldVars = Set[String]()

  var inRequire = false

  var globalVars = Set.empty[String] // TODO: different visitors for more than one procedure - this needs to be refreshed!!!!
  var localVars = Set.empty[String]

  var returnExprString = ""

  override def visitProgram(ctx: SimpleCParser.ProgramContext): String = {

    varsAreGlobals = true
    val globals = ctx.globals.map(v => visitVarDecl(v)).mkString("\n")
    varsAreGlobals = false

    return globals + "\n" + visitProcedureDecl(ctx.procedureDecl)
  }

  override def visitProcedureDecl(ctx: SimpleCParser.ProcedureDeclContext): String = {
    if (ctx == null) return ""

    val returnExpr = visitExpr(ctx.returnExpr)
    returnExprString = returnExpr
    visitPreposts(ctx.contract)

    val name = ctx.name.getText()

    val formals = visitFormalParams(ctx.formals)
    var statements =
      oldVars.map(v => s"int old_$v;\nold_$v=$v;\n").mkString("\n") + "\n" + visitStatements(ctx.stmts)

    statements = globalVars.map(v => s"$v = 0;").mkString("\n") + "\n" + statements

    val hoistedVars = localVars.map(v => s"int $v;").mkString("\n")

    val requirePreds = this.requireExprs.mkString(" && ")

    val ensureAsserts = this.ensureExprs.mkString("\n")

    val code = if (requirePreds.isEmpty)
      s"""
         |int $name($formals) {
            |$hoistedVars
            |$statements
            |$ensureAsserts
            |return $returnExpr;
         |}
         |""".stripMargin
      else s"""
         |int $name($formals) {
            |$hoistedVars
            |if ($requirePreds) {
                |$statements
                |$ensureAsserts
            |}
            |return $returnExpr;
         |}
         |""".stripMargin

    this.requireExprs = List.empty[String]
    this.ensureExprs = List.empty[String]
    return code
  }

  override def visitPreposts(preposts: util.List[PrepostContext]): String = {
    preposts.toList.map(visitPrepost)
    return ""
  }

  override def visitRequires(ctx: SimpleCParser.RequiresContext):String= {
    if (ctx == null) return ""
    inRequire = true
    requireExprs = visitExpr(ctx.condition) :: requireExprs
    inRequire = false
    return ""
  }

  override def visitEnsures(ctx: SimpleCParser.EnsuresContext):String = {
    if (ctx != null) {
      val substitutionVisitor = new SubstitutionVisitor("\\result", s"($returnExprString)")
      this.ensureExprs = "assert " + substitutionVisitor.visitExpr(ctx.condition) + ";" :: this.ensureExprs
      oldVars = oldVars union substitutionVisitor.oldVars
    }
    return ""
  }

  override def visitVarDecl(ctx: SimpleCParser.VarDeclContext): String = {
    if (ctx != null && varsAreGlobals) {
      globalVars += ctx.ID().getText()
      return super.visitVarDecl(ctx)
    } else {
      localVars += ctx.ID().getText()
      return ""
    }
  }

  override def visitVarrefExpr(ctx: SimpleCParser.VarrefExprContext):String = {
    if (inRequire) {
      globalVars -= (ctx.ID().getText())
    }
    return super.visitVarrefExpr(ctx)
  }




  }
