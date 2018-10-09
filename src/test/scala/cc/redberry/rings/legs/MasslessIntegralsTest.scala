package cc.redberry.rings.legs

import cc.redberry.rings.bigint.BigInteger
import cc.redberry.rings.scaladsl
import cc.redberry.rings.scaladsl.{Frac, MultivariatePolynomial}
import cc.redberry.rings.scaladsl.syntax._
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.sym.MasslessIntegrals
import cc.redberry.rings.util.ZipUtil
import org.junit.Test

/**
  *
  */
class MasslessIntegralsTest {

  @Test
  def test0(): Unit = {
    val calculator = new MasslessIntegrals(Z, cacheFile = Some("/Users/poslavskysv/Downloads/huitasen"))
    implicit val ring: Frac[MultivariatePolynomial[BigInteger]] = calculator.ring

    val (p12, p23, p34, p45, p15) = ring("p12", "p23", "p34", "p45", "p15")
    val (p13, p14, p24, p25, p35) = ring("p13", "p14", "p24", "p25", "p35")
    val d = ring("d")

    println(ZipUtil.compress(ring))
    if(true) return

    val expr = calculator.I5(2, 2, 1, 1, 1, d + 2, p12, p23, p34, p45, p15, p13, p14, p24, p25, p35)

    expr.terms.foreach { case (k, v) =>
      println(s"${k.stringify()} => ${ring.stringify(ring.factor(v))}")
    }

    calculator.persistCache()
  }

  @Test
  def test1(): Unit = {

    val calculator = new MasslessIntegrals(Z)
    implicit val ring: Frac[MultivariatePolynomial[BigInteger]] = calculator.ring

    val (p12, p23, p34, p45, p15) = ring("p12", "p23", "p34", "p45", "p15")
    val (p13, p14, p24, p25, p35) = ring("p13", "p14", "p24", "p25", "p35")
    val d = ring("d")

    val expr = calculator.I5(2, 2, 1, 1, 1, d + 2, p12, p23, p34, p45, p15, p13, p14, p24, p25, p35)



    //    import java.io.PrintWriter
    //    new PrintWriter("/Users/Stanislav/Projects_tmp/I5.txt") {
    //      write(expr.stringify()); close
    //    }

    //    println("done write")

    //    expr.terms.foreach {case (k, v) =>
    //      val fname = k.stringify().replace(",","_").replace("[", "_").replace("]","")
    //      println(fname)
    //      new PrintWriter(s"/Users/Stanislav/Projects_tmp/I5/$fname.txt") {
    //        write(v.toStringFactors(ring.coder)); close
    //      }
    //    }

    expr.terms.foreach { case (k, v) =>
      println(s"${k.stringify()} => ${ring.stringify(ring.factor(v))}")
    }
  }
}
