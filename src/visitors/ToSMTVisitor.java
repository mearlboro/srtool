package visitors;

import org.antlr.v4.runtime.ParserRuleContext;
import parser.SimpleCBaseVisitor;
import parser.SimpleCParser;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;

import visitors.SimpleCBoolCheckerVisitor;

import javax.swing.text.html.HTMLDocument;

/**
 * Created by Timotej on 15-Oct-16.
 */
public class ToSMTVisitor extends SimpleCBaseVisitor<String> {

    StringBuilder b = new StringBuilder();
    List<String> assertions = new LinkedList<>();
    SimpleCBoolCheckerVisitor boolTypeChecker = new SimpleCBoolCheckerVisitor();

    @Override
    public String visitAssignStmt(SimpleCParser.AssignStmtContext ctx) {
        b.append(String.format("(assert ( = %s %s))\n", ctx.ID(), intify(ctx.expr())));
        return "";
    }

    @Override
    public String visitAddExpr(SimpleCParser.AddExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitAddExpr(ctx);
        return preWeave(
                ctx.mulExpr().stream()
                        .map(c -> intify(c))
                        .collect(Collectors.toList()),
                ctx.ops.stream()
                        .map(o -> o.getText())
                        .collect(Collectors.toList())
        );
    }

    @Override
    public String visitTernExpr(SimpleCParser.TernExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitTernExpr(ctx);
        return String.format("(ite %s %s %s)", boolify(ctx.lorExpr(0)), intify(ctx.lorExpr(1)),intify(ctx.lorExpr(2)));
    }

    @Override
    public String visitMulExpr(SimpleCParser.MulExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitMulExpr(ctx);
        return preWeave(
                ctx.unaryExpr().stream()
                        .map(c -> intify(c))
                        .collect(Collectors.toList()),
                ctx.ops.stream()
                        .map(o -> o.getText())
                        .collect(Collectors.toList())
        );
    }

    @Override
    public String visitShiftExpr(SimpleCParser.ShiftExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitShiftExpr(ctx);
        return preWeave(
                ctx.addExpr().stream()
                        .map(c -> intify(c))
                        .collect(Collectors.toList()),
                ctx.ops.stream()
                        .map(o -> o.getText())
                        .collect(Collectors.toList())
        );
    }

    @Override
    public String visitParenExpr(SimpleCParser.ParenExprContext ctx) {
        return visitExpr(ctx.expr());
    }

    @Override
    public String visitUnaryExpr(SimpleCParser.UnaryExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitUnaryExpr(ctx);

        switch (ctx.ops.get(0).getText()) {
            case "+":  return String.format("%s",intify(ctx.atomExpr()));
            case "-":  return String.format("(bvneg %s)",intify(ctx.atomExpr()));
            case "!":  return String.format("(not %s)",boolify(ctx.atomExpr()));
            case "~":  return String.format("(bvnot %s)",intify(ctx.atomExpr()));
            default: return "invalid unary op " + ctx.ops.get(0).getText();
        }
    }

    @Override
    public String visitNumberExpr(SimpleCParser.NumberExprContext ctx) {
        return String.format("(_ bv%s 32)", ctx.NUMBER());
    }

    @Override
    public String visitVarrefExpr(SimpleCParser.VarrefExprContext ctx) {
        return ctx.ID().toString();
    }

    @Override
    public String visitAssertStmt(SimpleCParser.AssertStmtContext ctx) {
        assertions.add(boolify(ctx.expr()));
        return "";
    }

    @Override
    public String visitLorExpr(SimpleCParser.LorExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitLorExpr(ctx);
        return preWeave(
                ctx.landExpr().stream()
                        .map(c -> boolify(c))
                        .collect(Collectors.toList()),
                ctx.ops.stream()
                        .map(o -> o.getText())
                        .collect(Collectors.toList())
        );
    }

    @Override
    public String visitLandExpr(SimpleCParser.LandExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitLandExpr(ctx);
        return preWeave(
                ctx.borExpr().stream()
                        .map(c -> boolify(c))
                        .collect(Collectors.toList()),
                ctx.ops.stream()
                        .map(o -> o.getText())
                        .collect(Collectors.toList())
        );
    }

    @Override
    public String visitBorExpr(SimpleCParser.BorExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitBorExpr(ctx);
        return preWeave(
                ctx.bxorExpr().stream()
                        .map(c -> intify(c))
                        .collect(Collectors.toList()),
                ctx.ops.stream()
                        .map(o -> o.getText())
                        .collect(Collectors.toList())
        );
    }

    @Override
    public String visitBxorExpr(SimpleCParser.BxorExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitBxorExpr(ctx);
        return preWeave(
                ctx.bandExpr().stream()
                        .map(c -> intify(c))
                        .collect(Collectors.toList()),
                ctx.ops.stream()
                        .map(o -> o.getText())
                        .collect(Collectors.toList())
        );
    }

    @Override
    public String visitBandExpr(SimpleCParser.BandExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitBandExpr(ctx);
        return preWeave(
                ctx.equalityExpr().stream()
                        .map(c -> intify(c))
                        .collect(Collectors.toList()),
                ctx.ops.stream()
                        .map(o -> o.getText())
                        .collect(Collectors.toList())
        );
    }

    @Override
    public String visitEqualityExpr(SimpleCParser.EqualityExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitEqualityExpr(ctx);
//        switch (ctx.ops.get(0).getText()) {
//            case "==": return String.format("(= %s %s)", intify(ctx.relExpr(0)), intify(ctx.relExpr(1)));
//            case "!=": return String.format("(not (= %s %s))", intify(ctx.relExpr(0)), intify(ctx.relExpr(1)));
//            default:   return "INVALID OPSSS EQ"   + ctx.ops.get(0).getText();
//        }


        AtomicInteger i = new AtomicInteger(0);
        return fancyPreWeave(
                ctx.relExpr().stream()
                        .map(c -> intify(c))
                        .collect(Collectors.toList()),
                ctx.ops.stream()
                        .map(o -> o.getText())
                        .collect(Collectors.toList()),
                c -> {
                    if(i.incrementAndGet() > 1) {
                        return String.format("(tobv32 %s)", c);
                    } else return c;

                }
        );
    }

    @Override
    public String visitRelExpr(SimpleCParser.RelExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitRelExpr(ctx);
//        return preWeave(
//                ctx.shiftExpr().stream()
//                               .map(c -> intify(c))
//                               .collect(Collectors.toList()),
//                ctx.ops.stream()
//                       .map(o -> o.getText())
//                       .collect(Collectors.toList())
//        );

        AtomicInteger i = new AtomicInteger(0);
        return fancyPreWeave(
                ctx.shiftExpr().stream()
                        .map(c -> intify(c))
                        .collect(Collectors.toList()),
                ctx.ops.stream()
                        .map(o -> o.getText())
                        .collect(Collectors.toList()),
                c -> {
                    if(i.incrementAndGet() > 1) {
                        return String.format("(tobv32 %s)", c);
                    } else return c;

                }
        );
    }

    @Override
    public String visitVarDecl(SimpleCParser.VarDeclContext ctx) {
        b.append(String.format("(declare-fun %s () (_ BitVec 32))\n", ctx.ID()));
        return "";
    }

    @Override
    public String visitProcedureDecl(SimpleCParser.ProcedureDeclContext ctx) {
        ctx.formalParam().forEach(c -> b.append(String.format("(declare-fun %s () (_ BitVec 32))\n",c.ID())));
        return super.visitProcedureDecl(ctx);
    }

    public ToSMTVisitor(List<SimpleCParser.VarDeclContext> globals) {
        b.append("(set-logic QF_BV)\n");
        b.append("(set-option :produce-models true)\n");
        b.append("(define-fun tobv32 ((p Bool)) (_ BitVec 32) (ite p (_ bv1 32) (_ bv0 32)))\n");
        b.append("(define-fun tobool ((p (_ BitVec 32))) Bool (ite (= p (_ bv0 32)) false true))\n");
        b.append("(define-fun divc ((a (_ BitVec 32)) (b (_ BitVec 32))) (_ BitVec 32) (ite (= b (_ bv0 32) ) a (bvsdiv a b) ))\n");
        globals.forEach(g -> {visitVarDecl(g);});

    }

    public String getSMTv2() {
        b.append(String.format("(assert (not %s))\n", toConjunction(assertions)));


        b.append("(check-sat)\n");
        return b.toString();
    }

    private String toConjunction(List<String> cs) {
        return toTree(cs, "and", "true");
    }

    private String toTree(List<String> cs, String op, String iden) {
        if(cs.size() == 0) return iden;

        String current = cs.remove(0);
        return String.format("(%s %s %s)", op, current, toTree(cs, op, iden));

    }

    private String toBV(String expression) {
        return String.format("(tobv32 %s)", expression);
    }

    private String toBool(String expression) {
        return String.format("(tobool %s)", expression);
    }

    private String preWeave(List<String> a, List<String> ops) {
       return fancyPreWeave(a, ops, c -> c);
    }

    private String fancyPreWeave(List<String> a, List<String> ops, Function<String, String> typeFixxa) {
        if(a.size() == 1) return a.get(0);
        String op = toOp(ops.remove(ops.size() - 1));
        String curr = a.remove(a.size() - 1);
        return String.format(op,  typeFixxa.apply(fancyPreWeave(a, ops, typeFixxa)), curr);
    }

    private String toOp(String s) {
        switch(s) {
            case "==": return "(= %s %s)";
            case "!=": return "not ( = %s %s ) ";
            case ">":  return "(bvsgt %s %s)";
            case ">=": return "(bvsge %s %s)";
            case "<=": return "(bvsle %s %s)";
            case "<":  return "(bvslt %s %s)";
            case "*":  return "(bvmul %s %s)";
            case "/":  return "(divc %s %s)";
            case "%":  return "(bvsrem %s %s)";
            case "+":  return "(bvadd %s %s)";
            case "-":  return "(bvsub %s %s)";
            case ">>":  return "(bvlshr %s %s)";
            case "<<":  return "(bvshl %s %s)";
            case "||":  return "(or %s %s)";
            case "&&":  return "(and %s %s)";
            case "|":  return "(bvor %s %s)";
            case "&":  return "(bvand %s %s)";
            case "^":  return "(bvxor %s %s)";
            default: return "INVALID OPP " + s;
        }

    }


    private String boolify(ParserRuleContext ctx) {
        Boolean isBoolean = (Boolean) this.boolTypeChecker.visit(ctx);
        if (!isBoolean) {
            return toBool(ctx.accept(this));
        } else {
            return ctx.accept(this);
        }

    }

    private String intify(ParserRuleContext ctx) {
        Boolean isBoolean = (Boolean) this.boolTypeChecker.visit(ctx);
        if (isBoolean) {
            return toBV(ctx.accept(this));
        } else {
            return ctx.accept(this);
        }

    }
}
