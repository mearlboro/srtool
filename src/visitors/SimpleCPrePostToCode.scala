package visitors

import parser.{SimpleCBaseVisitor, SimpleCParser}

/**
  * Created by sam_coope on 24/10/2016.
  */
class SimpleCPrePostToCode extends SimpleCCodeVisitor{

  var requireExprs = List[String]()
//  val ensureExprs = List[String]

  override def visitProcedureDecl(ctx: SimpleCParser.ProcedureDeclContext): String = {
    if (ctx == null) return ""

    val preposts = visitPreposts(ctx.contract)
    if (this.requireExprs.isEmpty) return super.visitProcedureDecl(ctx)

    val name =  ctx.name.getText()
    val formals = visitFormalParams(ctx.formals)



    val statements = visitStatements(ctx.stmts)
    val returnExpr =  visitExpr(ctx.returnExpr)

    val code = "int " + name + "(" + formals + ")\n" +
      preposts +
      " {\n" +
        "if (" + this.requireExprs.mkString(" && ") + "){\n" +
          statements + "\n" +
        "\n}\n" +
        "return " + returnExpr + ";" +
      "\n}\n"

    this.requireExprs = List[String]()
    return code
  }

  override def visitRequires(ctx: SimpleCParser.RequiresContext):String= {
    if (ctx == null) return ""
    requireExprs = visitExpr(ctx.condition) :: requireExprs
    return ""
  }

//  TODO: this
  override def visitEnsures(ctx: SimpleCParser.EnsuresContext):String=
    if (ctx == null) "" else "ensures " + visitExpr(ctx.condition)

}
