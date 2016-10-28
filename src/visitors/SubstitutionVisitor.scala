package visitors

import parser.SimpleCParser
import parser.SimpleCParser.OldExprContext

/**
  * Created by ms6413 on 25/10/16.
  */
class SubstitutionVisitor(val toReplace: String, val replaceWith: String) extends SimpleCCodeVisitor{

  var oldVars = Set[String]()

  // perform the actual substitution
  override def visitResultExpr(ctx: SimpleCParser.ResultExprContext): String = {
    val ident = ctx.resultTok.getText()
    return (if (ident == toReplace) replaceWith else ident)
  }

  override def visitOldExpr(ctx: OldExprContext): String = {
    if (ctx == null) return ""
    val variableName = ctx.arg.getText()
    this.oldVars += variableName

    return s"olderino_$variableName"
  }

}


