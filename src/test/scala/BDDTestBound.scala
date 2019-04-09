package fosd.net.bdd

import org.scalatest.FunSuite

/**
  * Created by ckaestne on 6/18/2016.
  */
class BDDTestBound extends FunSuite {


  test("mk") {
    val f = new BDDFactory(1)

    val v1 = f.feature("a").v
    val v2 = f.feature("b").v
    val b1 = f.mk(v1, f.TRUE, f.FALSE)
    val b2 = f.mk(v1, f.TRUE, f.FALSE)
    assert(b1 == b2)
    assert(b1 eq b2)

    val taut = f.mk(v1, f.TRUE, f.TRUE)
    assert(taut eq f.TRUE)

    val taut2 = f.mk(v1, b1, b1)
    assert(taut2 eq b1)

    val c1 = f.mk(v2, b1, f.FALSE)
    val c2 = f.mk(v2, b1, f.FALSE)
    assert(c1 == c2)
    assert(c1 eq c2)

  }
  test("logic and issat") {
    val f = new BDDFactory(1)

    val a = f.feature("a")
    val b = f.feature("b")

    assert(a.isSatisfiable())
    assert(!(a and b).isSatisfiable())
    assert(!(a and a.not()).isSatisfiable())
  }


  test("bounding 1") {
    val f = new BDDFactory(1)

    val a = f.feature("a")
    val b = f.feature("b")
    val c = f.feature("c")

    assert((a and b) == f.FALSE)
    assert((a or b) == ((a and b.not) or (a.not and b)))
    //this cannot be simplified, requiring a canonical form
    assert(((a and b) or (a.not and b)) != b)
    assert(((a and b) or (a.not and b)) == (a.not and b))
    assert((a or b or c) == ((a and b.not and c.not) or (a.not and b and c.not) or (a.not and b.not and c)))


  }

  test("bounding 2") {
    val f = new BDDFactory(2)

    val a = f.feature("a")
    val b = f.feature("b")
    val c = f.feature("c")

    f.printDot(a and b and c)

  }


  trait Expr {
    def mkBdd(factory: BDDFactory): BDD

    def eval(assignment: Config): Boolean
  }

  case class And(a: Expr, b: Expr) extends Expr {
    override def mkBdd(factory: BDDFactory): BDD = a.mkBdd(factory) and b.mkBdd(factory)

    override def eval(assignment: Config): Boolean = a.eval(assignment) && b.eval(assignment)
  }

  case class Or(a: Expr, b: Expr) extends Expr {
    override def mkBdd(factory: BDDFactory): BDD = a.mkBdd(factory) or b.mkBdd(factory)

    override def eval(assignment: Config): Boolean = a.eval(assignment) || b.eval(assignment)
  }

  case class Not(a: Expr) extends Expr {
    override def mkBdd(factory: BDDFactory): BDD = a.mkBdd(factory).not

    override def eval(assignment: Config): Boolean = !a.eval(assignment)
  }

  case class Var(s: String) extends Expr {
    override def mkBdd(factory: BDDFactory): BDD = factory.feature(s)

    override def eval(assignment: Config): Boolean = assignment contains this
  }

  val vars = Seq("a", "b", "c", "d").map(Var.apply)
  val exprs: Seq[Expr] = {
    var e1 = vars ++ vars.map(Not.apply)

    var e2 = (for (a <- e1; b <- e1) yield Seq(And(a, b), Or(a, b))).flatten
    var e3 = (for (a <- e2; b <- e2) yield Seq(And(a, b), Or(a, b))).flatten
    var e4 = e3 ++ e3.map(Not.apply)
    var e5 = (for (a <- e4; b <- e1) yield Seq(And(a, b), Or(a, b))).flatten

    e1 ++ e2 ++ e4 ++ e5
  }
  type Config = Set[Var]
  type ConfigInt = Set[Int]
  val configs: List[Config] = for (a <- List(Set[Var](), Set(vars(0)));
                                   b <- List(Set[Var](), Set(vars(1)));
                                   c <- List(Set[Var](), Set(vars(2)));
                                   d <- List(Set[Var](), Set(vars(3)))) yield a ++ b ++ c ++ d

  //evaluate a bdd for a given assignment
  def eval(f: BDDFactory, bdd: BDD, assignment: ConfigInt): Boolean =
    if (bdd == f.FALSE) false
    else if (bdd == f.TRUE) true
    else if (assignment contains bdd.v)
      eval(f, bdd.high, assignment)
    else
      eval(f, bdd.low, assignment)


  test("bdd always evaluates to correct value within bound") {
    for (bound <- List(1, 2, 3, 4)) {
      println(s"checking bound $bound")

      for (e <- exprs) {
        checkExprEval(e, bound)
      }
    }
  }

  private def checkExprEval(e: Expr, bound: Int): Unit = {
    val f = new BDDFactory(bound)
    val bdd = e.mkBdd(f)
    //check all configs within bound
    for (config <- configs; if config.size <= bound) {
      val configInt = config.map(v => f.feature(v.s).v)
      assert(eval(f, bdd, configInt) == e.eval(config), s"$bdd != $e for assignment $config")
    }
  }


  //set of all assignments (within bounds) that evaluate to true
  type TruthTable = Set[Config]


  //this test cannot succeed with the current implementation
  ignore("all expressions with same truth table within bound share exactly same bdd structure") {
    for (bound <- List(1, 2, 3, 4)) {
      println(s"checking bound $bound")
      val f = new BDDFactory(bound)
      var repr: Map[TruthTable, (Expr, BDD)] = Map()

      for (e <- exprs) {

        val bdd = e.mkBdd(f)
        //check all configs within bound
        var truthTable: TruthTable = Set()
        for (config <- configs; if config.size <= bound) {
          val configInt = config.map(v => f.feature(v.s).v)
          if (eval(f, bdd, configInt))
            truthTable += config
        }

        val existingRepr = repr.get(truthTable)
        if (existingRepr.isDefined)
          assert(existingRepr.get._2 == bdd, s"prior different representation for $truthTable, \nwas ${existingRepr.get._2} for ${existingRepr.get._1}, \nnow $bdd for $e")
        repr += (truthTable -> (e, bdd))
      }
    }
  }

  test("all expressions with same truth table within bound are also equivalent") {
    for (bound <- List(1, 2, 3, 4)) {
      println(s"checking bound $bound")
      val f = new BDDFactory(bound)
      var repr: Map[TruthTable, (Expr, BDD)] = Map()

      for (e <- exprs) {

        val bdd = e.mkBdd(f)
        //check all configs within bound
        var truthTable: TruthTable = Set()
        for (config <- configs; if config.size <= bound) {
          val configInt = config.map(v => f.feature(v.s).v)
          if (eval(f, bdd, configInt))
            truthTable += config
        }

        val existingRepr = repr.get(truthTable)
        if (existingRepr.isDefined)
          assert(f.equivalent(existingRepr.get._2, bdd), s"prior different representation for $truthTable, \nwas ${existingRepr.get._2} for ${existingRepr.get._1}, \nnow $bdd for $e")
        repr += (truthTable -> (e, bdd))
      }
    }
  }


  test("reduceDepth") {
    val f = new BDDFactory(2)
    val a = f.feature("a")
    val b = f.feature("b")
    val c = f.feature("c")

    assert(f.reduceDepth(1, a and b) == f.FALSE)
    assert(f.reduceDepth(1, a or b) == (a or b))
    assert(f.reduceDepth(1, a and b.not) == a)
    assert(f.reduceDepth(1, (a and c) or (a.not and b and c)) == f.FALSE)
    assert(f.reduceDepth(1, (a and c) or (a.not and b.not and c)) == (a.not and b.not and c))
  }

//
//  test("t") {
//    val bound = 1
//    val f = new BDDFactory(bound)
  ////    val a = "a"
  ////    val b = "b"
  ////    val x = Or(Var(a), Or(Var(b), Or(Not(Var("c")), Var("d"))))
  ////
  ////    val y = Or(Or(And(Var(a), Var(a)), Or(Var(a), Var(a))), Not(Var(b)))
  ////    val z = Or(And(And(Var(a), Var(a)), And(Var(a), Var(b))), Not(Var(b)))
  ////
  ////    println(f.printDot(y.mkBdd(f)))
  ////    println(f.printDot(z.mkBdd(f)))
  ////
  ////
  ////    val bdd = x.mkBdd(f)
  ////    var truthTable: TruthTable = Set()
  ////    for (config <- configs; if config.size <= bound) {
  ////      val configInt = config.map(v => f.feature(v.s).v)
  ////      if (eval(f, bdd, configInt))
  ////        truthTable += config
  ////    }
  ////
  ////    println(truthTable)
  ////    println(f.printDot(bdd))
  ////    println(bdd)
  ////    //   val y = And(And(And(Var(a),Var(a)),And(Var(a),Var(a))),Var(b))
  ////    //
  ////    //   println(x.mkBdd(f))
  ////    //   println(y.mkBdd(f))
  //
  //
  //    val a = f.feature("a")
  //    val b = f.feature("b")
  //    val c = f.feature("c")
  //
  //    f.printDot(
  //      a or b.not
  //    )
  //    f.printDot(
  //        b.not
  //    )
  //
  //    println(f.equivalent(a or b.not, b.not))
  //
//  }


}
