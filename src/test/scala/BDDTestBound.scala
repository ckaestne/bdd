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


//  trait Expr {
//    def mkBdd(factory: BDDFactory): BDD
//  }
//  case class And(a: Expr, b: Expr) extends Expr{
//    override def mkBdd(factory: BDDFactory): BDD = a.mkBdd(factory) and b.mkBdd(factory)
//  }
//  case class Or(a: Expr, b: Expr) extends Expr{
//    override def mkBdd(factory: BDDFactory): BDD = a.mkBdd(factory) or b.mkBdd(factory)
//  }
//  case class Not(a: Expr) extends Expr{
//    override def mkBdd(factory: BDDFactory): BDD = a.mkBdd(factory).not
//  }
//  case class Var(s: String) extends Expr{
//    override def mkBdd(factory: BDDFactory): BDD = factory.feature(s)
//  }
//
//  val exprs: List[Expr] = {
//    val vars = List("a","b","c","d").map(Var.apply)
//    var e1 = vars ++ vars.map(Not.apply)
//
//    var e2 = (for (a<-e1; b<-e1) yield And(a, b) :: Or(a, b)::Nil).flatten
//    var e3 = (for (a<-e2; b<-e2) yield And(a, b) :: Or(a, b)::Nil).flatten
//    var e4 = e3 ++ e3.map(Not.apply)
//    var e5  = (for (a<-e4; b<-e1) yield And(a, b) :: Or(a, b)::Nil).flatten
//
//    println(e5.size)
//    e5.take(100).foreach(println)
//
//
//
//  }
//
//  test("mkbdd bound 1") {
//    val f = new BDDFactory(1)
//
//    for (e<-exprs) {
//      val bdd = e.mkBdd(f)
//    }
//  }




}
