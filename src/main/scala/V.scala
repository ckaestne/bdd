package fosd.net.bdd

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.ref.WeakReference


trait V[+T] {

}

trait VNode[+T] extends V[T] {
  def v: Int

  def low: VNode[T]

  def high: VNode[T]

  def isValue: Boolean

  def isNoValue: Boolean


  def select(ctx: VNode[Boolean]): VNode[T]

  def union[U >: T](that: VNode[U]): VNode[U]

  def map[U](f: T => U): VNode[U]

  def when(f: T => Boolean): VNode[Boolean] = map(f)

  def configSpace(): VNode[Boolean]

  def flatMap[U](f: T => VNode[U]): VNode[U]

  //update a value in a context
  def set[U >: T](ctx: VNode[Boolean], value: U): VNode[U]

  //overwrites the current value with the given value in the (partial) configuration space in which value is defined
  def overwrite[U >: T](value: VNode[U]): VNode[U]

}

trait VValue[+T] extends VNode[T] {
  def value: T
}

class VFactory {


  class Node[+T](val v: Int, val low: VNode[T], val high: VNode[T]) extends VNode[T] {
    lazy val hash = v + 31 * (if (low eq null) 0 else low.hashCode) + 27 * (if (high eq null) 0 else high.hashCode)

    override def equals(that: Any): Boolean = that match {
      case that: VNode[T] => (this.v == that.v) && (this.low eq that.low) && (this.high eq that.high)
      case _ => false
    }

    override def hashCode: Int = hash

    def select(ctx: VNode[Boolean]): VNode[T] = apply[Boolean, T, T](
      (ctx, v) => if (ctx.value) v else NOVALUE.asInstanceOf[VValue[T]]
      , ctx, this)

    def union[U >: T](that: VNode[U]): VNode[U] = apply[T, U, U](
      (left, right) => if (left eq NOVALUE) right else if (right eq NOVALUE) left else throw new VNodeException("attempting union of two overlapping configuration spaces")
      , this, that)


    def map[U](f: T => U): VNode[U] = VFactory.this.map(this, f)


    def flatMap[U](f: T => VNode[U]): VNode[U] = VFactory.this.flatMap(this, f)

    //update a value in a context
    def set[U >: T](ctx: VNode[Boolean], value: U): VNode[U] = overwrite(createValue[U](value).select(ctx))

    //overwrites the current value with the given value in the (partial) configuration space in which value is defined
    def overwrite[U >: T](value: VNode[U]): VNode[U] = apply[T, U, U](
      (oldV, newV) => if (newV == NOVALUE) oldV else newV
      , this, value)


    def isValue = false

    def isNoValue = false

    override def configSpace() = mapValue[T, Boolean](this, x => createValue(x != NOVALUE))
  }


  class BooleanNode(val s: VNode[Boolean]) {
    def not: VNode[Boolean] = map[Boolean, Boolean](s, x => !x)

    def and(that: VNode[Boolean]): VNode[Boolean] =
      lookupCache(boolOpCache, (true, s, that), {
        apply[Boolean, Boolean, Boolean]((a, b) => if (a.value && b.value) TRUE else FALSE, s, that)
      })

    def or(that: VNode[Boolean]): VNode[Boolean] =
      lookupCache(boolOpCache, (false, s, that), {
        apply[Boolean, Boolean, Boolean]((a, b) => if (a.value || b.value) TRUE else FALSE, s, that)
      })
  }

  implicit def booleanOperations(s: VNode[Boolean]) = new BooleanNode(s)

  class Value[T](var value: T) extends Node[T](Int.MaxValue, null, null) with VValue[T] {
    override def isValue = true

    override def hashCode: Int = value.hashCode()

    override def equals(that: Any): Boolean = that match {
      case that: VValue[T] => (that != NOVALUE) && (this.value == that.value)
      case _ => super.equals(that)
    }
  }

  val NOVALUE: VValue[Nothing] = new VValue[Nothing]() {
    override def hashCode: Int = -1

    override def equals(that: Any): Boolean = that match {
      case that: VValue[_] => that eq this
      case _ => false
    }

    override def value = ???

    override def high = null

    override def v = Int.MaxValue

    override def low = null

    override def isValue = true

    override def isNoValue = true

    override def select(ctx: VNode[Boolean]): VNode[Nothing] = NOVALUE

    override def flatMap[U](f: (Nothing) => VNode[U]): VNode[U] = NOVALUE

    override def set[U >: Nothing](ctx: VNode[Boolean], value: U) = createValue(value).select(ctx)

    override def overwrite[U >: Nothing](value: VNode[U]) = value

    override def union[U >: Nothing](that: VNode[U]) = that

    override def map[U](f: (Nothing) => U): VNode[U] = NOVALUE

    override def configSpace() = FALSE
  }

  private def lookupCache[K, V <: AnyRef](cache: mutable.WeakHashMap[K, WeakReference[V]], k: K, gen: => V): V = {
    val v = cache.get(k).flatMap(_.get)
    if (v.isDefined) v.get
    else {
      val x: V = gen
      cache.put(k, WeakReference.apply(x))
      x
    }
  }

  private val valueCache: mutable.WeakHashMap[Any, WeakReference[VValue[Any]]] = new mutable.WeakHashMap()

  def createValue[T](x: T): VValue[T] = {
    val v = valueCache.get(x).flatMap(_.get)
    if (v.isDefined) v.get.asInstanceOf[VValue[T]]
    else {
      val xv = new Value[T](x)
      valueCache.put(x, WeakReference.apply[VValue[Any]](xv))
      xv
    }
  }


  def createChoice[T](ctx: VNode[Boolean], a: VNode[T], b: VNode[T]): VNode[T] = ite(ctx, a, b)

  //    a.select(ctx) union b.select(ctx.not)

  def apply[A, B, C](op: (VValue[A], VValue[B]) => VValue[C], left: VNode[A], right: VNode[B]): VNode[C] = {
    var cache: Map[(VNode[A], VNode[B]), VNode[C]] = Map()

    def app(u1: VNode[A], u2: VNode[B]): VNode[C] = {
      val cached = cache.get((u1, u2))
      if (cached.nonEmpty)
        return cached.get

      val u =
        if (u1.isValue && u2.isValue)
          op(u1.asInstanceOf[VValue[A]], u2.asInstanceOf[VValue[B]])
        else if (u2.isValue)
          mk(u1.v, app(u1.low, u2), app(u1.high, u2))
        else if (u1.isValue)
          mk(u2.v, app(u1, u2.low), app(u1, u2.high))
        else if (u1.v < u2.v)
          mk(u1.v, app(u1.low, u2), app(u1.high, u2))
        else if (u1.v > u2.v)
          mk(u2.v, app(u1, u2.low), app(u1, u2.high))
        else //if (u1.v==u2.v)
          mk(u1.v, app(u1.low, u2.low), app(u1.high, u2.high))

      cache += ((u1, u2) -> u)
      u
    }

    app(left, right)
  }


  def ite[T](f: VNode[Boolean], g: VNode[T], h: VNode[T]): VNode[T] = {
    var cache: Map[(VNode[Boolean], VNode[T], VNode[T]), VNode[T]] = Map()

    def ite_(f: VNode[Boolean], g: VNode[T], h: VNode[T]): VNode[T] = {
      if (f eq TRUE) g
      else if (f eq FALSE) h
      else if (g eq h) h
      //      else if ((g eq TRUE) && (h eq FALSE)) f
      //      else if ((g eq FALSE) && (h eq TRUE)) f.not
      else {
        if ((g eq TRUE) && (h eq FALSE)) println("TODO missed opportunity for booleans")
        if ((g eq FALSE) && (h eq TRUE)) println("TODO missed opportunity for booleans")
        val cached = cache.get((f, g, h))
        if (cached.nonEmpty)
          return cached.get

        import Math._
        val v = min(f.v, min(g.v, h.v))

        val t = ite_(if (v == f.v) f.high else f, if (v == g.v) g.high else g, if (v == h.v) h.high else h)
        val e = ite_(if (v == f.v) f.low else f, if (v == g.v) g.low else g, if (v == h.v) h.low else h)
        if (t eq e) return e
        val result = mk(v, e, t)
        cache += ((f, g, h) -> result)
        result
      }
    }

    ite_(f, g, h)
  }

  def flatMap[T, U](node: VNode[T], f: T => VNode[U]): VNode[U] = flatMap[T, U](node, (_: VNode[Boolean], x: T) => f(x))

  def flatMap[T, U](node: VNode[T], f: (VNode[Boolean], T) => VNode[U]): VNode[U] = {
    val oldValueNodes = valueNodeIterator(node)
    var result: VNode[U] = NOVALUE

    for (oldValueNode <- oldValueNodes; if oldValueNode != NOVALUE) {
      val ctx = node.when(_ == oldValueNode.value)
      val newValue = f(ctx, oldValueNode.value)

      result = result union newValue.select(ctx)
    }
    result
  }



  private val bddTable: mutable.WeakHashMap[VNode[_], WeakReference[VNode[_]]] = new mutable.WeakHashMap()

  def mk[T](v: Int, low: VNode[T], high: VNode[T]): VNode[T] =
    if (low eq high)
      low
    else {
      assert(v < features.size, "unknown variable id")
      val newNode = new Node[T](v, low, high)
      lookupCache(bddTable, newNode, newNode).asInstanceOf[VNode[T]]
    }

  private val notCache: mutable.WeakHashMap[VNode[Boolean], WeakReference[VNode[Boolean]]] = new mutable.WeakHashMap()
  private val boolOpCache: mutable.WeakHashMap[(Boolean, VNode[Boolean], VNode[Boolean]), WeakReference[VNode[Boolean]]] = new mutable.WeakHashMap()

  def not(bdd: VNode[Boolean]): VNode[Boolean] = lookupCache(notCache, bdd, {
    map[Boolean, Boolean](bdd, x => !x)
  })


  def map[T, U](bdd: VNode[T], f: T => U): VNode[U] = mapValue[T, U](bdd,
    x => if (x == NOVALUE) NOVALUE else createValue(f(x.value))
  )

  def mapValue[T, U](bdd: VNode[T], f: VValue[T] => VValue[U]): VNode[U] = {
    var rewritten: Map[VNode[T], VNode[U]] = Map()

    def map_(bdd: VNode[T]): VNode[U] =
      if (rewritten contains bdd) rewritten(bdd)
      else {
        val newNode =
          if (bdd.isValue)
            f(bdd.asInstanceOf[VValue[T]])
          else mk(bdd.v, map_(bdd.low), map_(bdd.high))
        rewritten += (bdd -> newNode)
        newNode
      }

    map_(bdd)
  }

  def mapPair[T, U, V](a: VNode[T], b: VNode[U], f: (T, U) => V): VNode[V] =
    apply[T, U, V]((aa, bb) => createValue[V](f(aa.value, bb.value)), a, b)

  private var features: mutable.Map[String, Int] = mutable.Map()

  def varNum = features.size

  private def getFeatureId(s: String) =
    features.getOrElseUpdate(s, features.size)

  val TRUE = createValue(true)
  val FALSE = createValue(false)

  def feature(s: String) = mk[Boolean](getFeatureId(s), FALSE, TRUE)


  def foreachNode[T](bdd: VNode[T], f: VNode[T] => Unit): Unit =
    nodeIterator(bdd).foreach(f)

  def foreachValue[T](bdd: VNode[T], f: T => Unit): Unit =
    valueIterator(bdd).foreach(f)

  def nodeIterator[T](bdd: VNode[T]): Iterator[VNode[T]] = new VNodeIterator[T](bdd)

  def valueIterator[T](bdd: VNode[T]): Iterator[T] = nodeIterator(bdd).filter(x => x.isValue && x != NOVALUE).map(_.asInstanceOf[VValue[T]].value)

  def valueNodeIterator[T](bdd: VNode[T]): Iterator[VValue[T]] = nodeIterator(bdd).filter(x => x.isValue).map(_.asInstanceOf[VValue[T]])

  private class VNodeIterator[T](bdd: VNode[T]) extends Iterator[VNode[T]] {
    var seen: Set[VNode[T]] = Set()
    var queue: Queue[VNode[T]] = Queue() enqueue bdd

    override def hasNext = {
      while (queue.nonEmpty && (seen contains queue.head))
        queue = queue.dequeue._2
      queue.nonEmpty
    }

    override def next() = {
      while (queue.nonEmpty && (seen contains queue.head))
        queue = queue.dequeue._2

      val (b, newQueue) = queue.dequeue
      seen = seen + b
      if (b.low != null)
        queue = queue enqueue b.low
      if (b.high != null)
        queue = queue enqueue b.high
      b
    }
  }


  def printDot[T](bdd: VNode[T]): Unit = {

    def nodeId(b: VNode[T]) = Math.abs(b.hashCode)

    println("digraph G {")

    foreachNode[T](bdd, b => {
      if (b.isValue)
        println(nodeId(b) + " [shape=box, label=\"" + b.asInstanceOf[VValue[T]].value + "\", style=filled, shape=box, height=0.3];")
      else
        println(nodeId(b) + " [label=\"" + features.find(_._2 == b.v).map(_._1).getOrElse("unknown_" + b.v) + "\"];")

      if (b.low != null) {
        println(nodeId(b) + " -> " + nodeId(b.low) + " [style=dotted];")
      }
      if (b.high != null) {
        println(nodeId(b) + " -> " + nodeId(b.high) + " [style=filled];")
      }
    })



    println("}")
  }

}

class VNodeException(msg: String) extends Exception(msg)