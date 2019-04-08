package fosd.net.bdd

import org.scalatest.FunSuite

/**
  * Created by ckaestne on 6/18/2016.
  */
class BDDTest extends FunSuite {



  test("mk") {
    val f = new BDDFactory(100)

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

//  test("build") {
//    val f = new BDDFactory(100)
//    val v1 = f.feature("a").v
//    val v2 = f.feature("b").v
//
//    val b1 = f.mk(v1, f.TRUE, f.FALSE)
//    val b2 = f.mk(v2, b1, f.FALSE)
//
//    val rebuilt = f.build(b2)
//
//    //    println(b2)
//    //    println(rebuilt)
//
//    assert(rebuilt.v==v1)
//
//
//  }

  test("apply") {

    val f = new BDDFactory(100)

    val b1 = f.feature("a")
    val b2 = f.feature("b")

    val b1AndB2 = f.apply(f.AND, b1, b2)
    //    println(b1AndB2)
    assert(b1AndB2 == f.mk(b1.v, f.FALSE, f.mk(b2.v, f.FALSE, f.TRUE)))

    val b1OrB2 = f.apply(f.OR, b1, b2)
    //    println(b1OrB2)
    assert(b1OrB2 == f.mk(b1.v, f.mk(b2.v, f.FALSE, f.TRUE), f.TRUE))

    val b1OrB2Not = b1OrB2.not()
    //    println(b1OrB2Not)
    assert(b1OrB2Not == f.mk(b1.v, f.mk(b2.v, f.TRUE, f.FALSE), f.FALSE))
  }

  test("logic and issat") {
    val f = new BDDFactory(100)

    val a = f.feature("a")
    val b = f.feature("b")

    assert(a.isSatisfiable())
    assert((a and b).isSatisfiable())
    assert(!(a and a.not()).isSatisfiable())
  }

}
