package visitors

import java.util

import scala.collection.JavaConversions._
import parser.SimpleCParser.{OldExprContext, PrepostContext}
import parser.SimpleCParser

/**
  * Created by sam_coope on 24/10/2016.
  */
class SimpleCPrePostToCode extends SimpleCCodeVisitor{

  var requireExprs = List[String]()
  var ensureExprs = List[String]()

  var oldVars = Set[String]()

  var returnExprString = ""

  override def visitProcedureDecl(ctx: SimpleCParser.ProcedureDeclContext): String = {
    if (ctx == null) return ""

    val returnExpr =  visitExpr(ctx.returnExpr)
    returnExprString = returnExpr
    visitPreposts(ctx.contract)

    // this line needs changing for ensures
    if (this.requireExprs.isEmpty && this.ensureExprs.isEmpty) return super.visitProcedureDecl(ctx)

    val name =  ctx.name.getText()

    val formals = visitFormalParams(ctx.formals)
    val statements =
      oldVars.map(v => s"int old_$v;\nold_$v=$v;\n").mkString("\n") + visitStatements(ctx.stmts)


    val requirePreds = this.requireExprs.mkString(" && ")

    val ensureAsserts = this.ensureExprs.mkString("\n")

    val code = if(requirePreds.isEmpty) {
      s"""
         |int $name($formals) {
            |$statements
            |$ensureAsserts
            |return $returnExpr;
         |}
         |""".stripMargin
    } else s"""
         |int $name($formals) {
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
    requireExprs = visitExpr(ctx.condition) :: requireExprs
    return ""
  }

  override def visitEnsures(ctx: SimpleCParser.EnsuresContext):String = {
    if (ctx != null) {
      val substitutionVisitor = new SubstitutionVisitor("\\result", returnExprString)
      this.ensureExprs = "assert " + substitutionVisitor.visitExpr(ctx.condition) + ";" :: this.ensureExprs
      oldVars = oldVars union substitutionVisitor.oldVars
    }
    return ""
  }

}
