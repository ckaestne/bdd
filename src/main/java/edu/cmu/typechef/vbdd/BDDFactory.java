package edu.cmu.typechef.vbdd;

import java.lang.ref.WeakReference;
import java.util.*;
import java.util.function.Supplier;

/**
 * Created by ckaestne on 6/23/2016.
 */
public class BDDFactory {

    public class BDD {
        private final String v;
        private final BDD low;
        private final BDD high;
        private final int hash;

        private BDD(String v, BDD low, BDD high) {
            this.v = v;
            this.low = low;
            this.high = high;
            this.hash = v.hashCode() + 31 * (low == null ? 0 : low.hashCode()) + 27 * (high == null ? 0 : high.hashCode());
        }

        @Override
        public int hashCode() {
            return hash;
        }

        @Override
        public boolean equals(Object a) {
            if (a instanceof BDD) {
                BDD that = (BDD) a;
                return this.low == that.low && this.high == that.high && this.v.equals(that.v);
            }
            return false;
        }

        public BDD and(BDD that) {
            return apply(AND, this, that);
        }

        public BDD or(BDD that) {
            return apply(OR, this, that);
        }

        public BDD not() {
            return BDDFactory.this.not(this);
        }

        public boolean isSatisfiable() {
            return true;
        }

        public boolean isTautology() {
            return false;
        }

        public boolean isContradiction() {
            return false;
        }

        public boolean isTerminal() {
            return false;
        }

        @Override
        public String toString() {
            List<String> vars = getVars(this);
            Map<String, Integer> set = new HashMap<>();
            StringBuffer sb = new StringBuffer();
            bdd_printset_rec(sb, this, set, vars);
            return sb.toString();
        }

    }

    public BDD TRUE = new BDD("true", null, null) {
        @Override
        public String toString() {
            return "1";
        }

        @Override
        public boolean isTautology() {
            return true;
        }

        @Override
        public boolean isTerminal() {
            return true;
        }
    };

    public BDD FALSE = new BDD("false", null, null) {
        @Override
        public String toString() {
            return "0";
        }

        @Override
        public boolean isSatisfiable() {
            return false;
        }

        @Override
        public boolean isContradiction() {
            return true;
        }

        @Override
        public boolean isTerminal() {
            return true;
        }
    };

    private List<String> getVars(BDD bdd) {
        List<String> result = new ArrayList<>();
        Set<BDD> seen = new HashSet<>();
        List<BDD> queue = new LinkedList<>();
        queue.add(bdd);
        while (!queue.isEmpty()) {
            BDD a = queue.remove(0);
            if (!seen.contains(a)) {
                seen.add(a);
                if (!a.isTerminal()) {
                    result.add(a.v);
                    queue.add(a.low);
                    queue.add(a.high);
                }
            }
        }
        Collections.sort(result);
        return result;
    }

    private void bdd_printset_rec(StringBuffer sb, BDD r, Map<String, Integer> set, List<String> vars) {
        if (r.isTautology()) {
            sb.append('<');
            boolean first = true;
            for (String var : vars) {
                int k = set.getOrDefault(var, 0);
                if (k > 0) {
                    if (!first) sb.append(", ");
                    first = false;
                    sb.append(var);
                    sb.append(":");
                    sb.append(k == 2 ? 1 : 0);
                }
            }
            sb.append(">");

        } else if (r.isSatisfiable()) {
            set.put(r.v, 1);
            bdd_printset_rec(sb, r.low, set, vars);
            set.put(r.v, 2);
            bdd_printset_rec(sb, r.high, set, vars);
            set.remove(r.v);
        }
    }

    private static abstract class Op {
        public abstract boolean apply(boolean left, boolean right);

        abstract WeakHashMap<BDDPair, WeakReference<BDD>> cache();

    }

    private static Op AND = new Op() {

        @Override
        public boolean apply(boolean left, boolean right) {
            return left && right;
        }

        private WeakHashMap<BDDPair, WeakReference<BDD>> cache = new WeakHashMap<>();

        @Override
        WeakHashMap<BDDPair, WeakReference<BDD>> cache() {
            return cache;
        }
    };

    private static Op OR = new Op() {

        @Override
        public boolean apply(boolean left, boolean right) {
            return left || right;
        }

        private WeakHashMap<BDDPair, WeakReference<BDD>> cache = new WeakHashMap<>();

        @Override
        WeakHashMap<BDDPair, WeakReference<BDD>> cache() {
            return cache;
        }
    };

    private static class BDDPair {
        private final BDD low, high;

        private BDDPair(BDD low, BDD high) {
            this.low = low;
            this.high = high;
        }

        public int hashCode() {
            return low.hashCode() + high.hashCode();
        }

        public boolean equals(Object t) {
            if (t instanceof BDDPair) {
                BDDPair that = (BDDPair) t;
                return this.high == that.high && this.low == that.low;
            }
            return false;
        }
    }

    private WeakHashMap<BDD, WeakReference<BDD>> bddTable = new WeakHashMap<>();
    private WeakHashMap<BDD, WeakReference<BDD>> notCache = new WeakHashMap<>();

    public BDD option(String name) {
        return mk(name, FALSE, TRUE);
    }

    private BDD mk(String v, BDD low, BDD high) {
        if (low == high)
            return low;
        BDD newNode = new BDD(v, low, high);
        return lookupCache(bddTable, newNode, () -> newNode);
    }

    private <K, V> V lookupCache(WeakHashMap<K, WeakReference<V>> cache, K key, Supplier<V> newValue) {
        WeakReference<V> v = cache.get(key);
        V val = null;
        if (v != null) {
            val = v.get();
        }
        if (val != null)
            return val;

        val = newValue.get();
        cache.put(key, new WeakReference(val));
        return val;
    }

    public BDD apply(Op op, BDD left, BDD right) {
        return lookupCache(op.cache(), new BDDPair(left, right), new Apply(op, left, right));
    }

    private class Apply implements Supplier<BDD> {
        private final Op op;
        private final BDD left, right;
        private final Map<BDDPair, BDD> cache = new HashMap<>();

        private Apply(Op op, BDD left, BDD right) {
            this.op = op;
            this.left = left;
            this.right = right;
        }


        @Override
        public BDD get() {
            return app(left, right);
        }

        private BDD app(BDD left, BDD right) {
            BDDPair pair = new BDDPair(left, right);
            BDD cached = cache.get(pair);
            if (cached != null)
                return cached;

            BDD u;
            if (left.isTerminal() && right.isTerminal())
                u = op.apply(left == TRUE, right == TRUE) ? TRUE : FALSE;
            else if (left.v.equals(right.v))
                u = mk(left.v, app(left.low, right.low), app(left.high, right.high));
            else if (right.isTerminal() || (left.v.compareTo(right.v) > 0))
                u = mk(left.v, app(left.low, right), app(left.high, right));
            else
                u = mk(right.v, app(left, right.low), app(left, right.high));

            cache.put(pair, u);
            return u;
        }
    }


    public BDD not(BDD bdd) {
        return lookupCache(notCache, bdd, new Not(bdd));
    }

    private class Not implements Supplier<BDD> {
        private final BDD bdd;
        private final Map<BDD, BDD> cache = new HashMap<>();

        private Not(BDD bdd) {
            this.bdd = bdd;
        }


        @Override
        public BDD get() {
            return not_(bdd);
        }

        private BDD not_(BDD bdd) {
            if (bdd == TRUE) return FALSE;
            if (bdd == FALSE) return TRUE;
            BDD cached = cache.get(bdd);
            if (cached != null)
                return cached;
            BDD result = mk(bdd.v, not_(bdd.low), not_(bdd.high));
            cache.put(bdd, result);
            return result;
        }
    }


    private int nodeId(BDD b) {
        if (b == TRUE) return 1;
        else if (b == FALSE) return 0;
        else return Math.abs(b.hashCode());
    }

    public void printDot(BDD bdd) {


        System.out.println("digraph G {");
        System.out.println("0 [shape=box, label=\"FALSE\", style=filled, shape=box, height=0.3, width=0.3];");
        System.out.println("1 [shape=box, label=\"TRUE\", style=filled, shape=box, height=0.3, width=0.3];");
        Set<BDD> seen = new HashSet<>();
        LinkedList<BDD> queue = new LinkedList<>();
        queue.add(bdd);

        while (!queue.isEmpty()) {
            BDD b = queue.remove();
            if (!(seen.contains(b)) && (b != TRUE) && (b != FALSE)) {
                seen.add(b);
                System.out.println(nodeId(b) + " [label=\"" + b.v + "\"];");
                System.out.println(nodeId(b) + " -> " + nodeId(b.low) + " [style=dotted];");
                System.out.println(nodeId(b) + " -> " + nodeId(b.high) + " [style=filled];");
                queue.add(b.low);
                queue.add(b.high);
            }

        }

        System.out.println("}");
    }


}

