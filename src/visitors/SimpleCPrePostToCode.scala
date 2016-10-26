package visitors

import java.util
import scala.collection.JavaConversions._
import parser.SimpleCParser.PrepostContext
import parser.SimpleCParser

/**
  * Created by sam_coope on 24/10/2016.
  */
class SimpleCPrePostToCode extends SimpleCCodeVisitor{

  var requireExprs = List[String]()
//  val ensureExprs = List[String]

  var returnExprString = ""

  override def visitProcedureDecl(ctx: SimpleCParser.ProcedureDeclContext): String = {
    if (ctx == null) return ""

    val name =  ctx.name.getText()
    val formals = visitFormalParams(ctx.formals)

    val statements = visitStatements(ctx.stmts)
    val returnExpr =  visitExpr(ctx.returnExpr)

    returnExprString = returnExpr

    return "int " + name + "(" + formals + ")\n" +
      visitPreposts(ctx.contract) +
      " {\n" +
        "if (" + this.requireExprs.mkString(" && ") + "){\n" +
          statements + "\n" +
          "return " + returnExpr + ";" +
        "\n}\n"
      "\n}\n"
  }

  override def visitPreposts(preposts: util.List[PrepostContext]): String =
    preposts.toList.map(visitPrepost).mkString("\n")

  override def visitRequires(ctx: SimpleCParser.RequiresContext):String= {
    if (ctx == null) return ""
    requireExprs = visitExpr(ctx.condition) :: requireExprs
    return ""
  }

  override def visitEnsures(ctx: SimpleCParser.EnsuresContext):String =
    if (ctx == null) ""
    else {
      val substitutionVisitor = new SubstitutionVisitor("\\result", returnExprString)
      "assert " + substitutionVisitor.visitExpr(ctx.condition)
    }

}
