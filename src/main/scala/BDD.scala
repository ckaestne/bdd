package fosd.net.bdd


/**
  * Created by ckaestne on 6/18/2016.
  */
class BDD(val v: Int, val low: BDD, val high: BDD, private val factory: BDDFactory) {

  lazy val hash = v + 31 * (if (low eq null) 0 else low.hashCode) + 27 * (if (high eq null) 0 else high.hashCode)

  override def equals(that: Any): Boolean = that match {
    case that: BDD => (this.v == that.v) && (this.low eq that.low) && (this.high eq that.high)
    case _ => false
  }

  override def hashCode: Int = hash

  def and(that: BDD): BDD = {
    assert(this.factory eq that.factory, "cannot mix BDDs from different factories")
    this.factory.apply(this.factory.AND, this, that)
  }

  def or(that: BDD): BDD = {
    assert(this.factory eq that.factory, "cannot mix BDDs from different factories")
    this.factory.apply(this.factory.OR, this, that)
  }

  def equivalentTo(that: BDD): Boolean =
    this == that ||
      this.factory.equivalent(this, that)


  def not(): BDD = factory.not(this)

  def isSatisfiable(): Boolean = true

  def isContradiction(): Boolean = false

  def isTautology(): Boolean = false

  override def toString: String = {
    val set: Array[Int] = new Array[Int](factory.varNum)
    val sb: StringBuffer = new StringBuffer
    bdd_printset_rec(sb, this, set)
    return sb.toString
  }

  private def bdd_printset_rec(sb: StringBuffer, r: BDD, set: Array[Int]) {
    var first: Boolean = false
    if (r.isTautology()) {
      sb.append('<')
      first = true
      for (n <- 0 until set.length)
        if (set(n) > 0) {
          if (!first) sb.append(", ")
          first = false
          sb.append(n)
          sb.append(':')
          sb.append(if (set(n) == 2) 1 else 0)
        }
      sb.append('>')
    }
    else if (r.isSatisfiable()) {
      set(r.v) = 1
      val rl: BDD = r.low
      bdd_printset_rec(sb, rl, set)
      set(r.v) = 2
      val rh: BDD = r.high
      bdd_printset_rec(sb, rh, set)
      set(r.v) = 0
    }
  }


}
