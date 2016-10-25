package visitors;

import parser.SimpleCBaseVisitor;
import parser.SimpleCParser;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by Timotej on 15-Oct-16.
 */
public class ToSMTVisitor extends SimpleCBaseVisitor<String> {

    StringBuilder b = new StringBuilder();
    List<String> assertions = new LinkedList<>();

    @Override
    public String visitAssignStmt(SimpleCParser.AssignStmtContext ctx) {
        b.append(String.format("(assert ( = %s %s))\n", ctx.ID(), visitExpr(ctx.expr())));
        return "";
    }

    @Override
    public String visitAddExpr(SimpleCParser.AddExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitAddExpr(ctx);
        switch (ctx.ops.get(0).getText()) {
            case "+":  return String.format("(bvadd %s %s)", visitMulExpr(ctx.mulExpr(0)), visitMulExpr(ctx.mulExpr(1)));
            case "-":  return String.format("(bvsub %s %s)", visitMulExpr(ctx.mulExpr(0)), visitMulExpr(ctx.mulExpr(1)));
            default: return "invalid add op " + ctx.ops.get(0).getText();
        }

    }

    @Override
    public String visitTernExpr(SimpleCParser.TernExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitTernExpr(ctx);
        return String.format("(ite %s %s %s)", visit(ctx.lorExpr(0)), visit(ctx.lorExpr(1)),visit(ctx.lorExpr(2)));
    }

    @Override
    public String visitMulExpr(SimpleCParser.MulExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitMulExpr(ctx);
        String left = visitUnaryExpr(ctx.unaryExpr(0));
        String right = visitUnaryExpr(ctx.unaryExpr(1));
        switch (ctx.ops.get(0).getText()) {
            case "*":  return String.format("(bvmul %s %s)", left, right);
            //implements the x / 0 == x semantics in simple c
            case "/":  return String.format("(ite (= %s (_ bv0 32) ) %s (bvsdiv %s %s) )", right, left, left, right);
            case "%":  return String.format("(bvsrem %s %s)", left, right);
            default: return "invalid mull op " + ctx.ops.get(0).getText();
        }
    }

    @Override
    public String visitShiftExpr(SimpleCParser.ShiftExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitShiftExpr(ctx);
        switch (ctx.ops.get(0).getText()) {
            //this is an logical right shift, should be arithmetic i think. Seems not supported by z3
            case ">>":  return String.format("(bvlshr %s %s)", visitAddExpr(ctx.addExpr(0)), visitAddExpr(ctx.addExpr(1)));
            case "<<":  return String.format("(bvshl %s %s)", visitAddExpr(ctx.addExpr(0)), visitAddExpr(ctx.addExpr(1)));
            default: return "invalid shift op " + ctx.ops.get(0).getText();
        }
    }

    @Override
    public String visitParenExpr(SimpleCParser.ParenExprContext ctx) {
        return visitExpr(ctx.expr());
    }

    @Override
    public String visitUnaryExpr(SimpleCParser.UnaryExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitUnaryExpr(ctx);

        switch (ctx.ops.get(0).getText()) {
            case "+":  return String.format("%s",visitAtomExpr(ctx.atomExpr()));
            case "-":  return String.format("(bvneg %s)",visitAtomExpr(ctx.atomExpr()));
            case "!":  return String.format("(not %s)",visitAtomExpr(ctx.atomExpr()));
            case "~":  return String.format("(bvnot %s)",visitAtomExpr(ctx.atomExpr()));
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
        assertions.add(super.visitExpr(ctx.expr()));
        return "";
    }


    @Override
    public String visitLorExpr(SimpleCParser.LorExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitLorExpr(ctx);
        return String.format("(or %s %s)", visitLandExpr(ctx.landExpr(0)), visitLandExpr(ctx.landExpr(1)));
    }

    @Override
    public String visitLandExpr(SimpleCParser.LandExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitLandExpr(ctx);
        return String.format("(and %s %s)", visitBorExpr(ctx.borExpr(0)) , visitBorExpr(ctx.borExpr(1)));
    }

    @Override
    public String visitBorExpr(SimpleCParser.BorExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitBorExpr(ctx);
        return String.format("(bvor %s %s)", visitBxorExpr(ctx.bxorExpr(0)) , visitBxorExpr(ctx.bxorExpr(1)));
    }

    @Override
    public String visitBxorExpr(SimpleCParser.BxorExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitBxorExpr(ctx);
        return String.format("(bvxor %s %s)", visitBandExpr(ctx.bandExpr(0)) , visitBandExpr(ctx.bandExpr(1)));
    }

    @Override
    public String visitBandExpr(SimpleCParser.BandExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitBandExpr(ctx);
        return String.format("(bvor %s %s)", visitEqualityExpr(ctx.equalityExpr(0)) , visitEqualityExpr(ctx.equalityExpr(1)));
    }

    @Override
    public String visitEqualityExpr(SimpleCParser.EqualityExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitEqualityExpr(ctx);
        switch (ctx.ops.get(0).getText()) {
            case "==": return String.format("(= %s %s)", visitRelExpr(ctx.relExpr(0)), visitRelExpr(ctx.relExpr(1)));
            case "!=": return String.format("(not (= %s %s))", visitRelExpr(ctx.relExpr(0)), visitRelExpr(ctx.relExpr(1)));
            default:   return "INVALID OPSSS EQ"   + ctx.ops.get(0).getText();
        }
    }

    @Override
    public String visitRelExpr(SimpleCParser.RelExprContext ctx) {
        if (ctx.getChildCount() == 1) return super.visitRelExpr(ctx);

        switch (ctx.ops.get(0).getText()) {
            case ">":  return String.format("(bvsgt %s %s)", visitShiftExpr(ctx.shiftExpr(0)), visitShiftExpr(ctx.shiftExpr(1)));
            case ">=": return String.format("(bvsge %s %s)", visitShiftExpr(ctx.shiftExpr(0)), visitShiftExpr(ctx.shiftExpr(1)));
            case "<=": return String.format("(bvsle %s %s)", visitShiftExpr(ctx.shiftExpr(0)), visitShiftExpr(ctx.shiftExpr(1)));
            case "<":  return String.format("(bvslt %s %s)", visitShiftExpr(ctx.shiftExpr(0)), visitShiftExpr(ctx.shiftExpr(1)));
            default:   return "INVALID OPSSS REL " + ctx.ops.get(0).getText() ;
        }
    }

    @Override
    public String visitVarDecl(SimpleCParser.VarDeclContext ctx) {
        b.append(String.format("(declare-fun %s () (_ BitVec 32))\n", ctx.ID()));
        return "";
    }

    public ToSMTVisitor() {
        b.append("(set-logic QF_BV)\n");
        b.append("(set-option :produce-models true)\n");
    }

    public String getSMTv2() {
        b.append(String.format("(assert (not %s))\n", toConjunction(assertions)));


        b.append("(check-sat)\n");
//        b.append("(get-value (x1))");
        return b.toString();
    }

    private String toConjunction(List<String> cs) {
        if(cs.size() == 0) return "true";

        String current = cs.remove(0);
        return String.format("(and %s %s)", current, toConjunction(cs));
    }
}
