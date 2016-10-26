package tool

import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}
import parser.{SimpleCLexer, SimpleCParser}
import util.{ProcessExec, ProcessTimeoutException}
import visitors.ToSMTVisitor

/**
  * Created by sam_coope on 24/10/2016.
  */
object ToSMT {
  private val TIMEOUT: Int = 30

  def main(args: Array[String]): Unit = {

//    TODO: make this produce SMT lang, then call z3 afterwards.

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
      System.err.println("Typechecker in toSMT has failed.")
      import scala.collection.JavaConversions._
      for (err <- tc.getErrors) {
        System.err.println("  " + err)
      }
      System.exit(1)
    }

    assert(ctx.procedures.size == 1) // For Part 1 of the coursework, this can be assumed
    import scala.collection.JavaConversions._
    for (proc <- ctx.procedures) {
      val visitor: ToSMTVisitor = new ToSMTVisitor
      visitor.visit(proc)
      val smTv2: String = visitor.getSMTv2
      System.err.println(smTv2);
      val process: ProcessExec = new ProcessExec("z3", "-smt2", "-in")
      var queryResult: String = ""
      try {
        queryResult = process.execute(smTv2, TIMEOUT)
        System.err.println(queryResult);

      } catch {
        case e: ProcessTimeoutException => {
          System.out.println("UNKNOWN")
          System.exit(1)
        }
      }
      if (queryResult.startsWith("sat")) {
        System.out.println("INCORRECT")
        System.exit(0)
      }
      if (!queryResult.startsWith("unsat")) {
        System.out.println("UNKNOWN")
        System.exit(1)
      }
    }

    System.out.println("CORRECT")
    System.exit(0)
  }

}
