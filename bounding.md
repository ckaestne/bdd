# Bounding BDDs

The assumption is that we only want to analyze interactions
up to a fixed degree *D*. 
Specifically, we do not care about truth values of assignments
that assign more than *D* variables to *true*. Note that
the number of variables that we may need to assign to *false*
is not bound.

## Bounding Strategy

The key idea is to ignore all decisions that could require
more than *D* *true* assignments and instead default to the *false*
value for those decisions. This gives us a canonical form
again, where there is only a single representation for
every formula and where two formulas that differ only in
interactions beyond *D* variables yield the same representation.

For example, for D=1, the decision tree `A(B(1,2), B(3, 4))`
gets reduced to `A(2, B(3, 4))` because `1` can only
be reached when both `A` and `B` are *true* and are thus
excluded.
For the same reason `A(B(1,2), B(3, 4)) = A(B(5,2), B(3, 4))`
because they are both reduced to `A(2, B(3, 4))` (i.e.,
same values for all combinations without the bound).
Note that the trees can be further collapsed, say
`A(B(true, false), B(false, false)) = A(false, false) = false`.

The bounded BDDs always accurately represent values within
the bound and default to a given alternative value (deterministic)
beyond the bound, thus producing a normal form.

## Representation

The bound can only be measured top down, whereas BDDs are 
constructed bottom up (for caching). For the purpose
of the algorithm, each decision node has a *depth* that
counts how many *true* decisions are remaining until
the bound is reached.
That is, the top-level node has depth *D*, it's right
child has depth *D-1* (one *true* decision taken) and 
its left child depth *D* (one *false* decision taken,
thus all *true* decisions left).

If trying to construct a decision node with depth *0*,
instead the left child is returned. By definition
all nodes with depth *0* are terminal values, not
decisions.

Two trees can be structurally identical but used at
different depth. To foster sharing, depth is not stored
in the decision node itself, but used for caching:
Function `mk(var, depth, low, high)` to create a node receives
the depth variable and looks up existing combinations of
`(var, low, high)` at the same or *higher* depth values.
This is necessary to avoid reusing structures with
eliminated subtrees due to depth limits reached in some 
sometrees but not in others. Caching
will be more efficient if the right parts of the trees
are created first, because they result in lower depth
values that can be reused on the left hand side.

For example, for D=1, the subtree `B(T,F)` cannot 
be shared in `A(B(T,F), B(T,F))` (left side has depth 0,
right side has depth 1), leading to the
canonical representation `A(F, B(T,F))` rather
than `B(T,F)`. However, the sharing can still be achieved
at D=2 if the right part is created first and then cached.