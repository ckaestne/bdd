package fosd.net.bdd

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.ref.WeakReference

class BDDFactory(bound: Int) {


  val TRUE = new BDD(-1, null, null, this) {
    override def toString = "true"

    override def isTautology(): Boolean = true
  }
  val FALSE = new BDD(-2, null, null, this) {
    override def toString = "false"

    override def isSatisfiable() = false

    override def isContradiction(): Boolean = true
  }

  private val bddTable: mutable.WeakHashMap[BDD, WeakReference[BDD]] = new mutable.WeakHashMap()
  private val opCache: mutable.WeakHashMap[(Op, BDD, BDD), WeakReference[BDD]] = new mutable.WeakHashMap()
  private val notCache: mutable.WeakHashMap[BDD, WeakReference[BDD]] = new mutable.WeakHashMap()


  private[bdd] var features: mutable.Map[String, Int] = mutable.Map()

  def varNum = features.size

  private def getFeatureId(s: String) =
    features.getOrElseUpdate(s, features.size)


  def feature(s: String) = mk(getFeatureId(s), FALSE, TRUE)

  def mk(v: Int, low: BDD, high: BDD): BDD =
    if (low eq high)
      low
    else {
      assert(v < features.size, "unknown variable id")
      val newNode = new BDD(v, low, high, this)
      lookupCache(bddTable, newNode, newNode)
    }

  private def isTerminal(bdd: BDD) = (bdd eq TRUE) || (bdd eq FALSE)


  trait Op {
    def apply(a: Boolean, b: Boolean): Boolean
  }

  val AND = new Op {
    override def apply(a: Boolean, b: Boolean) = a && b
  }
  val OR = new Op {
    override def apply(a: Boolean, b: Boolean) = a || b
  }
  //  val EQUIV = new Op {
  //    override def apply(a: Boolean, b: Boolean) = a == b
  //  }


  private def lookupCache[K, V <: AnyRef](cache: mutable.WeakHashMap[K, WeakReference[V]], k: K, gen: => V): V = {
    val v = cache.get(k).flatMap(_.get)
    if (v.isDefined) v.get
    else {
      val x: V = gen
      cache.put(k, WeakReference.apply(x))
      x
    }
  }

  def equivalent(u1: BDD, u2: BDD): Boolean = {

    def app(depth: Int, u1: BDD, u2: BDD): Boolean = {
      if (u1 == u2) return true
      //      val cached = cache.get((depth, u1, u2))
      //      if (cached.nonEmpty)
      //        return cached.get

      def mk_(v: Int, low: () => Boolean, high: () => Boolean): Boolean =
        if (depth == 0) low() else low() && high()

      val u =
        if (isTerminal(u1) && isTerminal(u2))
          u1 == u2
        else if (isTerminal(u2))
          mk_(u1.v, () => app(depth, u1.low, u2), () => app(depth - 1, u1.high, u2))
        else if (isTerminal(u1))
          mk_(u2.v, () => app(depth, u1, u2.low), () => app(depth - 1, u1, u2.high))
        else if (u1.v < u2.v)
          mk_(u1.v, () => app(depth, u1.low, u2), () => app(depth - 1, u1.high, u2))
        else if (u1.v > u2.v)
          mk_(u2.v, () => app(depth, u1, u2.low), () => app(depth - 1, u1, u2.high))
        else //if (u1.v==u2.v)
          mk_(u1.v, () => app(depth, u1.low, u2.low), () => app(depth - 1, u1.high, u2.high))

      //      cache += ((depth, u1, u2) -> u)
      u
    }

    app(bound, u1, u2)
  }

  def apply(op: Op, u1: BDD, u2: BDD): BDD = lookupCache(opCache, (op, u1, u2), {
    var cache: Map[(Int, BDD, BDD), BDD] = Map()

    def app(depth: Int, u1: BDD, u2: BDD): BDD = {
      val cached = cache.get((depth, u1, u2))
      if (cached.nonEmpty)
        return cached.get

      def mk_(v: Int, low: () => BDD, high: () => BDD): BDD =
        if (depth == 0) low() else {
          val l = low()
          val rl = reduceDepth(depth - 1, l)
          val h = high()
          if (h eq rl) l
          else
            mk(v, l, h)
        }

      val u =
        if (isTerminal(u1) && isTerminal(u2))
          if (op(u1 eq TRUE, u2 eq TRUE)) TRUE else FALSE
        else if (isTerminal(u2))
          mk_(u1.v, () => app(depth, u1.low, u2), () => app(depth - 1, u1.high, u2))
        else if (isTerminal(u1))
          mk_(u2.v, () => app(depth, u1, u2.low), () => app(depth - 1, u1, u2.high))
        else if (u1.v < u2.v)
          mk_(u1.v, () => app(depth, u1.low, u2), () => app(depth - 1, u1.high, u2))
        else if (u1.v > u2.v)
          mk_(u2.v, () => app(depth, u1, u2.low), () => app(depth - 1, u1, u2.high))
        else //if (u1.v==u2.v)
          mk_(u1.v, () => app(depth, u1.low, u2.low), () => app(depth - 1, u1.high, u2.high))

      cache += ((depth, u1, u2) -> u)
      u
    }

    app(bound, u1, u2)
  })


  private[bdd] def reduceDepth(depth: Int, bdd: BDD): BDD = {
    var cache: Map[(Int, BDD), BDD] = Map()

    def red(depth: Int, bdd: BDD): BDD = {
      val cached = cache.get((depth, bdd))
      if (cached.nonEmpty)
        return cached.get

      val r = if (isTerminal(bdd)) bdd
      else if (depth <= 0) reduceDepth(depth, bdd.low)
      else {
        val lowNew = reduceDepth(depth, bdd.low)
        val highNew = reduceDepth(depth - 1, bdd.high)
        val lowNewReduced = reduceDepth(depth - 1, bdd.low)
        if (lowNewReduced eq highNew) lowNew
        else if ((lowNew eq bdd.low) && (highNew eq bdd.high)) bdd
        else mk(bdd.v, lowNew, highNew)

      }
      cache += ((depth, bdd) -> r)
      r
    }

    red(depth, bdd)
  }


  //  def build(t: BDD) = {
  //    build_(t, 0)
  //  }
  //
  //  private def build_(t: BDD, v: Int): BDD =
  //    if ((v >= features.size) || (t eq TRUE) || (t eq FALSE)) t
  //    else {
  //      val b0 = build_(sub(t, v, false), v + 1)
  //      val b1 = build_(sub(t, v, true), v + 1)
  //      mk(v, b0, b1)
  //    }
  //
  //  def sub(bdd: BDD, v: Int, byTrue: Boolean): BDD = if ((bdd eq TRUE) || (bdd eq FALSE)) bdd
  //  else if (bdd.v == v) {
  //    if (byTrue) bdd.high else bdd.low
  //  } else mk(bdd.v, sub(bdd.low, v, byTrue), sub(bdd.high, v, byTrue))
  //
  //
  def not(bdd: BDD): BDD = lookupCache(notCache, bdd, {
    var rewritten: Map[BDD, BDD] = Map()

    def not_(bdd: BDD): BDD =
      if (bdd eq TRUE) FALSE
      else if (bdd eq FALSE) TRUE
      else if (rewritten contains bdd) rewritten(bdd)
      else {
        val negbdd = mk(bdd.v, not_(bdd.low), not_(bdd.high))
        rewritten += (bdd -> negbdd)
        negbdd
      }

    not_(bdd)
  })

  def printDot(bdd: BDD): Unit = {

    def nodeId(b: BDD) =
      if (b == TRUE) 1 else if (b == FALSE) 0 else Math.abs(b.hashCode)

    println("digraph G {")
    println("x [style=filled, color=\"black\"];")
    println("0 [shape=box, label=\"FALSE\", style=filled, shape=box, height=0.3, width=0.3];")
    println("1 [shape=box, label=\"TRUE\", style=filled, shape=box, height=0.3, width=0.3];")
    var seen: Set[BDD] = Set()
    var queue: Queue[BDD] = Queue() enqueue bdd
    var bounds: Map[BDD, Int] = Map(bdd -> bound)

    println("x -> " + nodeId(bdd) + " [label=" + bound + "];")

    while (queue.nonEmpty) {
      val (b, newQueue) = queue.dequeue
      queue = newQueue
      if (!(seen contains b) && (b != TRUE) && (b != FALSE)) {
        seen = seen + b
        val node_bound = bounds(b)
        println(nodeId(b) + " [label=\"v" + features.find(_._2 == b.v).map(_._1).getOrElse("unknown_" + b.v) + "\"];")
        println(nodeId(b) + " -> " + nodeId(b.low) + " [style=dotted, label=" + node_bound + "];")
        println(nodeId(b) + " -> " + nodeId(b.high) + " [style=filled, label=" + (node_bound - 1) + "];")
        queue = queue enqueue b.low enqueue b.high
        bounds += (b.low -> node_bound)
        bounds += (b.high -> (node_bound - 1))
      }

    }

    println("}")
  }


}
