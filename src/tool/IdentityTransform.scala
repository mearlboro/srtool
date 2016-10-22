package tool

import java.io.FileInputStream

import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}
import org.omg.CORBA.TIMEOUT
import parser.{SimpleCLexer, SimpleCParser}
import util.{ProcessExec, ProcessTimeoutException}
import visitors.SimpleCCodeVisitor

/**
  * Created by sam_coope on 22/10/2016.
  */
object IdentityTransform {
  def main(args: Array[String]): Unit = {

    val filename: String = args(0)
    val input: ANTLRInputStream = new ANTLRInputStream(new FileInputStream(filename))
    val lexer: SimpleCLexer = new SimpleCLexer(input)
    val tokens: CommonTokenStream = new CommonTokenStream(lexer)
    val parser: SimpleCParser = new SimpleCParser(tokens)
    val ctx: SimpleCParser.ProgramContext = parser.program

    if (parser.getNumberOfSyntaxErrors > 0) System.exit(1)

    val tc: Typechecker = new Typechecker
    tc.visit(ctx)
    tc.resolve()
    if (tc.hasErrors) {
      System.err.println("Errors were detected when typechecking " + filename + ":")
      import scala.collection.JavaConversions._
      for (err <- tc.getErrors) {
        System.err.println("  " + err)
      }
      System.exit(1)
    }

    val codeVisitor = new SimpleCCodeVisitor
    val thing = codeVisitor.visit(ctx)
    println(thing)

//    val newcode = cVisitor.visit(ctx)

//      then pipe to std out

//    assert(ctx.procedures.size == 1) // For Part 1 of the coursework, this can be assumed
//    for (proc <- ctx.procedures) {
//      val vcgen: VCGenerator = new VCGenerator(proc)
//      val vc: String = vcgen.generateVC.toString
//      val process: ProcessExec = new ProcessExec("z3", "-smt2", "-in")
//      var queryResult: String = ""
//      try
//        queryResult = process.execute(vc, TIMEOUT)
//
//      catch {
//        case e: ProcessTimeoutException => {
//          System.out.println("UNKNOWN")
//          System.exit(1)
//        }
//      }
//      if (queryResult.startsWith("sat")) {
//        System.out.println("INCORRECT")
//        System.exit(0)
//      }
//      if (!queryResult.startsWith("unsat")) {
//        System.out.println("UNKNOWN")
//        System.out.println(queryResult)
//        System.exit(1)
//      }
//    }
//
//    System.out.println("CORRECT")
    System.exit(0)
  }
}
