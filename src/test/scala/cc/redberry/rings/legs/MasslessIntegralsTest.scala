package cc.redberry.rings.legs

import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._
import org.junit.Test

/**
  *
  */
class MasslessIntegralsTest {

  @Test
  def test1(): Unit = {
    val calculator = new MasslessIntegrals(Z, databaseFile = None)
    implicit val ring = calculator.ring
    import calculator.momentums._
    val i3 = calculator._I3(1, 1, 1, dim + 2, s12, s23, s34)
    println(i3.stringify)
    calculator.close()
  }

  //  @Test
  //  def test1(): Unit = {
  //    val calculator = new MasslessIntegrals(Z, databaseFile = Some("/Users/poslavskysv/Downloads/huitsen"))
  //
  //    implicit val ring = calculator.ring
  //    val (p12, p23, p34, p45, p15) = ring("p12", "p23", "p34", "p45", "p15")
  //    val (p13, p14, p24, p25, p35) = ring("p13", "p14", "p24", "p25", "p35")
  //    val d = ring("d")
  //
  //    val i5 = calculator.I5(2, 2, 1, 1, 1, d + 2, p12, p23, p34, p45, p15, p13, p14, p24, p25, p35)
  //    calculator.close()
  //
  //    val out = new FileWriter("/Users/poslavskysv/Downloads/i5_rings")
  //    out.write(i5.stringify())
  //    out.close()
  //
  //    i5.terms.foreach { case (k, v) =>
  //      val decomposition: FactorDecomposition[Rational[MultivariatePolynomial[IntZ]]] = ring.factor(v)
  //      println(s"${k.stringify()} => ${ring.stringify(decomposition)}")
  //    }
  //  }
  //
  //  @Test
  //  def test012(): Unit = {
  //    val calculator = new MasslessIntegrals(Z, databaseFile = Some("/Users/poslavskysv/Downloads/huitasen"))
  //
  //    try {
  //      implicit val ring: Frac[MultivariatePolynomial[BigInteger]] = calculator.ring
  //
  //      val (p12, p23, p34, p45, p15) = ring("p12", "p23", "p34", "p45", "p15")
  //      val (p13, p14, p24, p25, p35) = ring("p13", "p14", "p24", "p25", "p35")
  //      val d = ring("d")
  //
  //      val expr = calculator.I5(2, 2, 1, 1, 1, d + 2, p12, p23, p34, p45, p15, p13, p14, p24, p25, p35)
  //
  //      expr.terms.foreach { case (k, v) =>
  //        println(s"${k.stringify()} => ${ring.stringify(ring.factor(v))}")
  //      }
  //
  //    } finally
  //      calculator.close()
  //  }
  //
  //  @Test
  //  def test1231(): Unit = {
  //
  //    val calculator = new MasslessIntegrals(Z)
  //    implicit val ring: Frac[MultivariatePolynomial[BigInteger]] = calculator.ring
  //
  //    val (p12, p23, p34, p45, p15) = ring("p12", "p23", "p34", "p45", "p15")
  //    val (p13, p14, p24, p25, p35) = ring("p13", "p14", "p24", "p25", "p35")
  //    val d = ring("d")
  //
  //    val expr = calculator.I5(2, 2, 1, 1, 1, d + 2, p12, p23, p34, p45, p15, p13, p14, p24, p25, p35)
  //
  //
  //
  //    //    import java.io.PrintWriter
  //    //    new PrintWriter("/Users/Stanislav/Projects_tmp/I5.txt") {
  //    //      write(expr.stringify()); close
  //    //    }
  //
  //    //    println("done write")
  //
  //    //    expr.terms.foreach {case (k, v) =>
  //    //      val fname = k.stringify().replace(",","_").replace("[", "_").replace("]","")
  //    //      println(fname)
  //    //      new PrintWriter(s"/Users/Stanislav/Projects_tmp/I5/$fname.txt") {
  //    //        write(v.toStringFactors(ring.coder)); close
  //    //      }
  //    //    }
  //
  //    expr.terms.foreach { case (k, v) =>
  //      println(s"${k.stringify()} => ${ring.stringify(ring.factor(v))}")
  //    }
  //  }
}
