package edu.cmu.typechef.vbdd;

import edu.cmu.typechef.vbdd.BDDFactory.BDD;
import org.junit.Test;

/**
 * Created by ckaestne on 6/23/2016.
 */
public class BDDTest {

    @Test
    public void test() {
        BDDFactory f = new BDDFactory();

        BDD x = f.option("x");
        BDD y = f.option("y");
        BDD z = f.option("z");
        System.out.println(x.and(y));
        f.printDot(x.and(y).or(z).not());

    }
}
