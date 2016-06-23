package fosd.net.bdd

import org.scalatest.FunSuite

/**
  * Created by ckaestne on 6/18/2016.
  */
class VTest extends FunSuite {


  test("mk") {
    val f = new VFactory()

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

    val x1 = f.createValue(1)
    val x2 = f.createValue(2)
    val x3 = f.createValue(3)
    val y1 = f.mk(v1, x1, x2)
    val y2 = f.mk(v2, y1, x3)
    val y1_ = f.mk(v1, x1, x2)
    val y2_ = f.mk(v2, y1, x3)
    assert(y1 == y1_)
    assert(y1 eq y1_)
    assert(y2 == y2_)
    assert(y2 eq y2_)


    f.printDot(f.mk(v2, y1, f.mk(v1, x1, x3)))

  }

  test("union") {
    val f = new VFactory()

    val a = f.feature("a").v
    val b = f.feature("b").v

    val x = f.mk(a, f.createValue(1), f.NOVALUE)
    val y = f.mk(a, f.NOVALUE, f.createValue(2))
    assert((x union y) eq f.mk(a, f.createValue(1), f.createValue(2)))
  }

  test("select") {
    val f = new VFactory()
    import f.booleanOperations

    val a = f.feature("a").v
    val b = f.feature("b").v

    assert(f.createValue(1).select(f.TRUE) eq f.createValue(1))
    assert(f.createValue(1).select(f.feature("a")) eq f.mk(a, f.NOVALUE, f.createValue(1)))
    assert(f.mk(a, f.createValue(1), f.createValue(2)).select(f.feature("a").not) eq f.mk(a, f.createValue(1), f.NOVALUE))

    assert(f.createValue(1).select(f.TRUE).configSpace() eq f.TRUE)
    assert(f.createValue(1).select(f.feature("a")).configSpace() eq f.feature("a"))
    assert(f.mk(a, f.createValue(1), f.createValue(2)).select(f.feature("a").not).configSpace() eq f.feature("a").not)

  }


  test("choice") {
    val f = new VFactory()

    val a = f.feature("a")
    val b = f.feature("b")

    f.printDot(f.createChoice(a, f.createValue(1), f.createValue(2)))
    f.printDot(f.createChoice(b, f.createChoice(a, f.createValue(1), f.createValue(2)), f.createChoice(a, f.createValue(2), f.createValue(3))))
    assert(f.createChoice(a, f.createValue(1), f.createValue(2)) eq f.mk(a.v, f.createValue(2), f.createValue(1)))
    assert(f.createChoice(b, f.createChoice(a, f.createValue(1), f.createValue(2)), f.createChoice(a, f.createValue(2), f.createValue(3))) eq
      f.mk(a.v, f.mk(b.v, f.createValue(3), f.createValue(2)), f.mk(b.v, f.createValue(2), f.createValue(1))))

  }

  test("map") {
    val f = new VFactory()
    import f.booleanOperations
    val a = f.feature("a")
    assert(f.createChoice(a, f.createValue(1), f.createValue(2)).map(_ + 1) eq f.createChoice(a, f.createValue(2), f.createValue(3)))
    assert(f.createChoice(a, f.createValue(1), f.createValue(2)).when(_ > 1) eq a.not)
  }

  test("mapPair") {
    val f = new VFactory()
    val a = f.feature("a")
    val b = f.feature("b")

    var counter = 0
    val r = f.mapPair[Int, Int, Int](f.createChoice(a, f.createValue(1), f.createValue(2)), f.createChoice(a, f.createValue(1), f.createValue(2)), (a, b) => {
      counter += 1;
      a + b
    })
    assert(r == f.createChoice(a, f.createValue(2), f.createValue(4)))
    assert(counter == 2)

    val s = f.mapPair[Int, Int, Int](
      f.createChoice(a, f.createValue(1), f.createValue(2)),
      f.createChoice(b, f.createValue(4), f.createValue(8)),
      _ + _)
    f.printDot(s)
    assert(s == f.createChoice(b, f.createChoice(a, f.createValue(5), f.createValue(6)), f.createChoice(a, f.createValue(9), f.createValue(10))))

    val t = f.mapPair[Int, Int, Int](
      f.createChoice(a, f.createValue(1), f.createValue(2)),
      f.createChoice(b, f.createValue(1), f.createValue(2)),
      _ + _)
    f.printDot(t)
    assert(t == f.createChoice(b, f.createChoice(a, f.createValue(2), f.createValue(3)), f.createChoice(a, f.createValue(3), f.createValue(4))))
  }

  test("set") {
    val f = new VFactory()
    import f.booleanOperations
    val a = f.feature("a")
    val b = f.feature("b")

    val x = f.createValue(1)
    val y = x.set(a, 3)
    assert(y eq f.createChoice(a, f.createValue(3), f.createValue(1)))

    val z = x.set(a and b, 3)
    assert(z eq f.createChoice(a and b, f.createValue(3), f.createValue(1)))
    f.printDot(z)
  }

  test("ite") {
    val f = new VFactory()
    import f.booleanOperations
    val a = f.feature("a")
    val b = f.feature("b")
    val c = f.feature("c")

    f.printDot(f.ite(a, b, c))

    assert(f.ite(a, b, c) eq f.createChoice(a, b, c))
    assert(f.ite(a, b, c) eq ((a and b) or (a.not and c)))

  }

  test("flatMap") {
    val f = new VFactory()
    val a = f.feature("a")
    val b = f.feature("b")
    val c = f.feature("c")
    val d = f.feature("d")

    assert(f.createValue(1).flatMap(x => f.createChoice(a, f.createValue(x + 1), f.createValue(x))) eq
      f.createChoice(a, f.createValue(2), f.createValue(1)))

    assert(f.createChoice(b, f.createValue(1), f.createValue(2)).flatMap(x => f.createChoice(a, f.createValue(x + 1), f.createValue(x))) eq
      f.createChoice(b, f.createChoice(a, f.createValue(2), f.createValue(1)), f.createChoice(a, f.createValue(3), f.createValue(2))))

    val x = f.createChoice(b, f.createChoice(d, f.createValue(2), f.createValue(1)), f.createChoice(a, f.createValue(3), f.createValue(2)))
    val y = f.createChoice(c, f.createChoice(a, f.createValue(6), f.createValue(9)), f.createChoice(b, f.createValue(6), f.createValue(10)))

    f.printDot(x.flatMap(xx => y.map(yy => xx + yy)))

    assert(x.flatMap(xx => y.map(yy => xx + yy)) eq f.mapPair[Int, Int, Int](x, y, _ + _))


  }

}
