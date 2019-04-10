# Bounded BDD Library written in Scala

[![Build Status](https://travis-ci.org/ckaestne/bdd.svg?branch=bounded)](https://travis-ci.org/ckaestne/bdd)

Bounded library that drops parts of the formula that require more than
*d* activated variables, but still achieves a canonical form.

As the base implementation, uses weak hashmaps for garbage collection.

# Bounding BDDs

The assumption is that we only want to analyze interactions
up to a fixed degree *D*. 
Specifically, we do not care about truth values of assignments
that assign more than *D* variables to *true*. Note that
the number of variables that we may need to assign to *false*
is not bound. 

Note that this makes the assumption that all variables default
to *false* and we bound only the variables assigned to *true*.
This means that interactions among variables assigned to
*false* are not bound.
This may not be suitable for traditional forms as combinatorial
interaction testing which explores positive and negative 
interactions within a bound.


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
`a&&b -> A(B(true, false), B(false, false)) = A(false, false) = false`.

The bounded BDDs always accurately represent values within
the bound and default to a given alternative value (deterministic)
beyond the bound, thus producing a normal form.

## Representation and Construction

The bound can only be measured top down, whereas BDDs are 
constructed bottom up (for caching). For the purpose
of the algorithm, each decision node has a *depth* that
counts how many *true* decisions are remaining until
the bound is reached.
That is, the top-level node has depth *D*, it's *high*
child has depth *D-1* (one *true* decision taken) and 
its *low* child depth *D* (one *false* decision taken,
thus all *true* decisions left).

If trying to construct a decision node with depth *0*,
instead the left child is returned. By definition
all nodes with depth *0* are terminal values, not
decisions.

When constructing a node `v(low, high)` at depth *d*,
the usual BDD strategies of caching and simplifications
apply with some extensions:
 * If the combination of `v`, `low`, and `high` has been 
   created before (independent of depth), we reuse the resulting object
 * As usual, if `low == high` one of these is returned instead of creating a new node
 * If `d == 0` low is returned, because `high` cannot be reached within the bound
 * If `reduce(low, d-1) == high` where `reduce` computes the representation
   of `low` at a lower depth, `low` is returned, because in this case
   the choice of `v` does not affect the outcome within the bound.
   
The last step is important to achieve a canonical form, but requires
additional construction overhead, since we need to create `low` both
at depth `d` and `d-1`. This step could be skipped when a canonical
form is not required to speed up construction.   
   
Note that caching is independent of the depth of a node, hence
sharing is still efficient.   
   
## Properties

Equivalence: For all formulas, a bounded BDD at bound *d* evaluates
to the same outcomes as a traditional BDD or a bounded
BDD with bound *e, e>=d* for all assignments in which
at most *d* variables are set to *true*.

Canonical: If two bounded BDDs at bound *d* evaluate
to the same outcomes for all assignments in which at most
*d* variables are set to *true*, they have the same structure
(and are indeed represented by the same object)

The evaluation of a bounded BDD at bound *d* for an
assignment with more than *d* variables set to *true* 
is not defined. It is technically possible to receive 
a (stable, deterministic) value, but that value
has no meaning. (Note, it may be worth defining
an evaluation function that rejects such assignments.) 