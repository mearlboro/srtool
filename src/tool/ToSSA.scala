package tool

import util.CodeFormat.formatCCode
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}
import parser.{SimpleCLexer, SimpleCParser}
import visitors.{SimpleCtoSSAVisitor}

/**
  * Created by sam_coope on 22/10/2016.
  */
object ToSSA {
  def main(args: Array[String]): Unit = {

    val input: ANTLRInputStream = new ANTLRInputStream(System.in)
    val lexer: SimpleCLexer = new SimpleCLexer(input)
    val tokens: CommonTokenStream = new CommonTokenStream(lexer)
    val parser: SimpleCParser = new SimpleCParser(tokens)
    val ctx: SimpleCParser.ProgramContext = parser.program

    if (parser.getNumberOfSyntaxErrors > 0) System.exit(1)

    val tc: Typechecker = new Typechecker
    tc.visit(ctx)
    tc.resolve()
    if (tc.hasErrors) {
      System.err.println("Typechecker in toSSA has failed.")
      import scala.collection.JavaConversions._
      for (err <- tc.getErrors) {
        System.err.println("  " + err)
      }
      System.exit(1)
    }

    val codeVisitor = new SimpleCtoSSAVisitor
    val thing = codeVisitor.visit(ctx)
    println(formatCCode(thing))

    System.exit(0)
  }
}
