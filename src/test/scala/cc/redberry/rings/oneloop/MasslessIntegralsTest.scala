package cc.redberry.rings.oneloop

import java.io.{FileOutputStream, PrintStream}
import java.nio.file.Files

import cc.redberry.rings.oneloop.Definitions.FactorizedIntegralVal.Factor
import cc.redberry.rings.oneloop.Definitions.{FactorizedIntegralVal, IntegralVal, PrintFormat, PrintFormatter}
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._
import org.junit.{Ignore, Test}
import TestUtil._

/**
  *
  */
class MasslessIntegralsTest {

  @Test
  def test1(): Unit = {
    val calculator = new MasslessIntegrals(Z, databaseFile = None)
    implicit val ring: Frac[MultivariatePolynomial[IntZ]] = calculator.ring

    val (s12, s23, s34, s45, s15) = ring("s12", "s23", "s34", "s45", "s15")
    val (s13, s14, s24, s25, s35) = ring("s13", "s14", "s24", "s25", "s35")
    val dim = ring("d")

    val i3 = calculator.I3(1, 1, 1, 2, s12, s23, s34, factorized = true)
    println(i3._2.right.get.print(Console.out, PrintFormatter(PrintFormat.MMA, true)))
    calculator.close()
  }

  @Test
  def test2(): Unit = {
    val calculator = new MasslessIntegrals(Z, databaseFile = None)
    implicit val ring: Frac[MultivariatePolynomial[IntZ]] = calculator.ring

    val (s12, s23, s34, s45, s15) = ring("s12", "s23", "s34", "s45", "s15")
    val (s13, s14, s24, s25, s35) = ring("s13", "s14", "s24", "s25", "s35")
    val dim = ring("d")

    val i3 = calculator.I3(2, 2, 2, 2, s12 + s23, s23 - s12, s12, factorized = true)
    println(i3._2.right.get.print(Console.out, PrintFormatter(PrintFormat.MMA, true)))
    calculator.close()
  }

  @Test
  def test3(): Unit = {
    val vars = Seq("x", "y", "z")
    val calculator = new MasslessIntegrals(Z, databaseFile = None, usedVariables = vars)
    implicit val ring: Frac[MultivariatePolynomial[IntZ]] = calculator.ring

    val (x, y, z) = ring("x", "y", "z")
    val dim = ring("d")

    val i3 = calculator.I3(2, 2, 2, 2, x, y, z, factorized = true)
    println(i3._2.right.get.print(Console.out, PrintFormatter(PrintFormat.MMA, true)))
    calculator.close()
  }


  @Test
  def test4(): Unit = {
    val calculator = new MasslessIntegrals(Z, databaseFile = Some(mkTempFile()))
    try {
      implicit val ring: Frac[MultivariatePolynomial[IntZ]] = calculator.ring

      val (s12, s23, s34, s45, s15) = ring("s12", "s23", "s34", "s45", "s15")
      val (s13, s14, s24, s25, s35) = ring("s13", "s14", "s24", "s25", "s35")
      val dim = ring("d")

      //run i5 --n1 2 --n2 2 --n3 1 --n4 1 --n5 2 --di 2  --s12 0 --s34 s23 -t -f
      val i5 = calculator.I5(2, 2, 1, 1, 2, 2,
        ring(0), s23, ring(0), s15, s15, ring(0), s14, s14, s25, s35, factorized = true)

      //      i5._2.left.get.print(Console.out, PrintFormatter(PrintFormat.MMA, true))
      i5._2.right.get.print(Console.out, PrintFormatter(PrintFormat.MMA, true))
    } finally
      calculator.close()
  }

  @Test
  def test5(): Unit = {
    def i3(cache: Boolean, withZ: Boolean = false) = {
      val calculator = new MasslessIntegrals(Z,
        databaseFile = if (cache) Some(mkTempFile()) else None,
        usedVariables = Seq("x", "y", "z"),
        doLog = false)
      try {
        implicit val ring: Frac[MultivariatePolynomial[IntZ]] = calculator.ring
        val (x, y, z) = ring("x", "y", "z")
        calculator.I3(2, 2, 1, 2, x, if (withZ) z else ring(0), y)._2.left.get
      } finally
        calculator.close()
    }

    assert(i3(cache = true, withZ = false) == i3(cache = false, withZ = false))
    assert(i3(cache = false, withZ = true) == i3(cache = true, withZ = true))
  }

  @Test
  def test6(): Unit = {
    for (dbFile <- Seq(None, Some(mkTempFile())))
      for (di <- Seq(0, 2, 4))
        for (n1 <- Seq(1, 2))
          for (n2 <- Seq(1, 2))
            for (n3 <- Seq(1, 2)) {
              val calculator = new MasslessIntegrals(Z, databaseFile = dbFile, doLog = false)
              try {
                implicit val ring: Frac[MultivariatePolynomial[IntZ]] = calculator.ring
                val (s12, s23, s13) = ring("s12", "s23", "s13")

                import calculator._

                val i3 = calculator.I3(n1, n2, n3, di, s12, s13, s23)._2.left.get
                for (zv <- Seq(s12, s13, s23)) {
                  val zeroVar = zv.numerator().univariateVariable()

                  val limit: IntegralVal[Expr] = try {
                    calculator.findLimit(i3, Seq(zv), Seq(ring.getZero))
                  } catch {
                    case _: ArithmeticException => IntegralVal.zero[Expr]
                  }

                  val (_s12, _s13, _s23) = (s12.map(_.evaluateAtZero(zeroVar)), s13.map(_.evaluateAtZero(zeroVar)), s23.map(_.evaluateAtZero(zeroVar)))
                  val plain: IntegralVal[Expr] =
                    try {
                      calculator.I3(n1, n2, n3, di, _s12, _s13, _s23)._2.left.get
                    } catch {
                      case _: ArithmeticException => IntegralVal.zero[Expr]
                    }

                  assert(plain.isZero || (plain - limit).isZero,
                    s"""
                       |actual:   ${plain.stringify()}
                       |expected: ${limit.stringify()}
                     """.stripMargin)
                }
              } finally
                calculator.close()
            }
  }

  @Test
  def test7(): Unit = {
    for (dbFile <- Seq(Some(mkTempFile())))
      for (di <- Seq(0, 2, 4))
        for (n1 <- Seq(1, 2))
          for (n2 <- Seq(1, 2))
            for (n3 <- Seq(1, 2))
              for (n4 <- Seq(1, 2)) {
                val calculator = new MasslessIntegrals(Z, databaseFile = dbFile, doLog = false)
                try {
                  implicit val ring: Frac[MultivariatePolynomial[IntZ]] = calculator.ring
                  val (s12, s13, s14, s23, s24, s34) = ring("s12", "s13", "s14", "s23", "s24", "s34")

                  import calculator._

                  val i4 = calculator.I4(n1, n2, n3, n4, di, s12, s13, s14, s23, s24, s34)._2.left.get
                  for (zv <- Seq(s12, s13, s14, s23, s24, s34)) {
                    Console.err.println((n1, n2, n3, n4, di, ring.stringify(zv)))

                    val zeroVar = zv.numerator().univariateVariable()

                    val limit: IntegralVal[Expr] = try {
                      calculator.findLimit(i4, Seq(zv), Seq(ring.getZero))
                    } catch {
                      case _: ArithmeticException => IntegralVal.zero[Expr]
                    }

                    val Seq(_s12, _s13, _s14, _s23, _s24, _s34) = Seq(s12, s13, s14, s23, s24, s34).map(_.map(_.evaluateAtZero(zeroVar)))
                    val plain: IntegralVal[Expr] =
                      try {
                        calculator.I4(n1, n2, n3, n4, di, _s12, _s13, _s14, _s23, _s24, _s34)._2.left.get
                      } catch {
                        case _: ArithmeticException => IntegralVal.zero[Expr]
                      }

                    assert(plain.isZero || (plain - limit).isZero,
                      s"""
                         |actual:   ${plain.stringify()}
                         |expected: ${limit.stringify()}
                     """.stripMargin)
                  }
                } finally
                  calculator.close()
              }
  }

  @Test
  @Ignore
  def test8(): Unit = {
    val calculator = new MasslessIntegrals(Z, databaseFile = None) //Some("/Users/poslavskysv/Downloads/integrals___.db")
    try {
      implicit val ring: Frac[MultivariatePolynomial[IntZ]] = calculator.ring
      val (s12, s23, s34, s45, s15) = ring("s12", "s23", "s34", "s45", "s15")
      val (s13, s14, s24, s25, s35) = ring("s13", "s14", "s24", "s25", "s35")
      val dim = ring("d")
      import calculator._

      var i5: IntegralVal[Expr] = null

      i5 = calculator.I5(2, 2, 1, 1, 1, 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)._2.left.get
      i5.print(
        new PrintStream(new FileOutputStream("/Users/poslavskysv/Downloads/i5integral.ri")),
        PrintFormatter(PrintFormat.MMA, false))

      i5 = calculator.I5(2, 2, 1, 1, 1, 2, ring(0), s23, s34, s45, s15, s13, s14, s24, s25, s35)._2.left.get
      i5.print(
        new PrintStream(new FileOutputStream("/Users/poslavskysv/Downloads/i5integral_s12e0.ri")),
        PrintFormatter(PrintFormat.MMA, false))

      i5 = calculator.I5(2, 2, 1, 1, 1, 2, ring(0), s23, s34, s45, s34, s13, s14, s24, s25, s35)._2.left.get
      i5.print(
        new PrintStream(new FileOutputStream("/Users/poslavskysv/Downloads/i5integral_s12e0_s15es34.ri")),
        PrintFormatter(PrintFormat.MMA, false))
    } finally
      calculator.close()
  }

  @Test
  def test9(): Unit = {
    val calculator = new MasslessIntegrals(Z, Some(mkTempFile())) //Some("/Users/poslavskysv/Downloads/integrals___.db")
    try {
      implicit val ring: Frac[MultivariatePolynomial[IntZ]] = calculator.ring
      val (s12, s23, s34, s45, s15) = ring("s12", "s23", "s34", "s45", "s15")
      val (s13, s14, s24, s25, s35) = ring("s13", "s14", "s24", "s25", "s35")
      val dim = ring("d")

      val i5def = calculator.I4(2, 2, 1, 1, 2, s12, s23, s34, s13, s14, s24)._1
      //      val i5def = calculator.I3(2, 1, 1, 0, s12, s23, s13)._1

      val indices = Seq("a", "b")
      val e = calculator.evaluate(i5def, indices, true)
      println(e)
    } finally
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
