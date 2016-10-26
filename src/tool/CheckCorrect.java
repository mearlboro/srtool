package tool;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.LinkedList;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;

import parser.SimpleCLexer;
import parser.SimpleCParser;
import parser.SimpleCParser.ProcedureDeclContext;
import parser.SimpleCParser.ProgramContext;
import util.ProcessExec;
import util.ProcessTimeoutException;
import visitors.SimpleCtoSSAVisitor;
import visitors.*;

public class CheckCorrect  {

    private static final int TIMEOUT = 30;

	public static void main(String[] args) throws IOException, InterruptedException {
        String filename = args[0];
		ANTLRInputStream input = new ANTLRInputStream(new FileInputStream(filename));
        SimpleCLexer lexer = new SimpleCLexer(input);
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		SimpleCParser parser = new SimpleCParser(tokens);
		ProgramContext ctx = parser.program();
		if(parser.getNumberOfSyntaxErrors() > 0) {
			System.exit(1);
		}
		Typechecker tc = new Typechecker();
		tc.visit(ctx);
		tc.resolve();
		if(tc.hasErrors()) {
			System.err.println("Errors were detected when typechecking " + filename + ":");
			for(String err : tc.getErrors()) {
				System.err.println("  " + err);
			}
			System.exit(1);
		}

		assert ctx.procedures.size() == 1; // For Part 1 of the coursework, this can be assumed

		for(ProcedureDeclContext proc : ctx.procedures) {
		//	SimpleCtoSSAVisitor visitor = new SimpleCtoSSAVisitor();
//			SimpleCPrePostToCode visitor = new SimpleCPrePostToCode();

			ToSMTVisitor visitor = new ToSMTVisitor(new LinkedList<>());
            visitor.visit(proc);
//			System.out.println(visitor.visit(proc));
//			System.exit(0);
			String smTv2 = visitor.getSMTv2();
			System.out.println(smTv2);

			ProcessExec process = new ProcessExec("z3", "-smt2", "-in");
			String queryResult = "";
			//	queryResult = process.execute(smTv2, TIMEOUT);
				// System.out.println(queryResult);


			if (queryResult.startsWith("sat")) {
				System.out.println("INCORRECT");
				System.exit(0);
			}

			if (!queryResult.startsWith("unsat")) {
				System.out.println("UNKNOWN");
				// System.out.println(queryResult);
				System.exit(1);
			}
		}

		System.out.println("CORRECT");
		System.exit(0);

    }
}
