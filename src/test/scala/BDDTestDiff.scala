package fosd.net.bdd {

  import de.fosd.typechef.featureexpr.bdd.{BDDFeatureExpr, BDDHelper}
  import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
  import org.scalacheck.Gen._
  import org.scalacheck.Prop._
  import org.scalacheck._

  /**
    * differential testing against bdd library in FeatureExprLib
    */

  class BDDTestDiff extends Properties("BDD") {

    val bddFactory: BDDFactory = new BDDFactory()
    bddFactory.feature("x")
    FeatureExprFactory.setDefault(FeatureExprFactory.bdd)

    def feature(a: String) = (bddFactory.feature(a), FeatureExprFactory.createDefinedExternal(a))

    val featureNames = List("a", "b", "c", "d", "e", "f")
    val a = feature("a")
    val b = feature("b")
    val c = feature("c")
    val d = feature("d")
    val e = feature("e")
    val f = feature("f")

    val genAtomicFeatureWithDeadAndBase: Gen[(BDD, FeatureExpr)] =
      oneOf((bddFactory.TRUE, FeatureExprFactory.True) ::(bddFactory.FALSE, FeatureExprFactory.False) :: featureNames.map(feature(_)))
    val genAtomicFeatureWithoutDeadAndBase: Gen[(BDD, FeatureExpr)] =
      oneOf(featureNames.map(x => (bddFactory.feature(x), FeatureExprFactory.createDefinedExternal(x))))

    implicit def arbFeatureExpr: Arbitrary[(BDD, FeatureExpr)] = Arbitrary {
      genFeatureExpr(genAtomicFeatureWithDeadAndBase, {
        x => x
      })
    }

    def getNonDeadFeatureExpr: Gen[(BDD, FeatureExpr)] = genFeatureExpr(genAtomicFeatureWithoutDeadAndBase, {
      x => 3
    })

    private def genFeatureExpr(genAtomicFeature: Gen[(BDD, FeatureExpr)], size: (Int) => Int) = {
      def genCompoundFeature(size: Int): Gen[(BDD, FeatureExpr)] = oneOf(
        for {
          a <- genFeatureExpr(size)
          b <- genFeatureExpr(size)
        } yield (a._1 and b._1, a._2 and b._2),
        for {
          a <- genFeatureExpr(size)
          b <- genFeatureExpr(size)
        } yield (a._1 or b._1, a._2 or b._2),
        for {
          a <- genFeatureExpr(size)
        } yield (a._1.not, a._2.not))

      def genFeatureExpr(size: Int): Gen[(BDD, FeatureExpr)] =
        if (size <= 0) genAtomicFeature
        else Gen.frequency((1, genAtomicFeature), (3, genCompoundFeature(size / 2)))
      Gen.sized(sz => genFeatureExpr(sz))

    }


    property("isSatisfiable") = Prop.forAll((a: (BDD, FeatureExpr)) => a._1.isSatisfiable == a._2.isSatisfiable())
    property("same print") = Prop.forAll((a: (BDD, FeatureExpr)) => {
      println(a._1)
      println(a._2)
      a._1.toString.replace("false","").replace("true","<>") == BDDHelper.getBDD(a._2.asInstanceOf[BDDFeatureExpr]).toString()
    })
    //  property("and1") = Prop.forAll((a: (BDD, FeatureExpr)) => a._1.toString==a._2.toString())

  }

}



package de.fosd.typechef.featureexpr.bdd {


  object BDDHelper{
    def getBDD(b: BDDFeatureExpr): net.sf.javabdd.BDD =
      b.bdd
  }
}