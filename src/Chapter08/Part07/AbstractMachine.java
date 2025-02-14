package Part07;

import java.util.LinkedList;
import java.util.List;

public class AbstractMachine {
    /* EXPRESSIONS */
    sealed interface Expr permits Val, Add {
    }

    record Val(int value) implements Expr {
    }

    record Add(Expr left, Expr right) implements Expr {
    }

    /* OPERATIONS */
    sealed interface Op permits EVAL, ADD {
    }

    record EVAL(Expr expr) implements Op {
    }

    record ADD(int value) implements Op {
    }

    static int eval(Expr expr, List<Op> cont) {
        return switch (expr) {
            case Val n -> exec(n.value, cont);
            case Add e -> {
                cont.addFirst(new EVAL(e.right));
                yield eval(e.left, cont);
            }
        };
    }

    static int exec(int n, List<Op> cont) {
        if (cont.isEmpty())
            return n;

        return switch (cont.removeFirst()) {
            case EVAL op -> {
                cont.addFirst(new ADD(n));
                yield eval(op.expr, cont);
            }
            case ADD op -> exec(n + op.value, cont);
        };
    }

    static int value(Expr expr) {
        return eval(expr, new LinkedList<>());
    }

    public static void main(String[] args) {
        Expr expr = new Add(
                new Add(
                        new Val(2),
                        new Val(3)),
                new Val(4));

        int result = value(expr);

        assert (result == 9); // Add the `-ea` JVM option to activate assertions
        System.out.println("Result: " + result);
    }
}