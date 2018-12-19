package cc.redberry.rings.oneloop

import cc.redberry.core.tensor.Tensor
import cc.redberry.core.tensor.Tensors._
import cc.redberry.core.transformations.DifferentiateTransformation.differentiate
import cc.redberry.core.transformations.expand.ExpandTransformation
import cc.redberry.rings.oneloop.Definitions.IntegralDef
import cc.redberry.rings.oneloop.TestUtil._
import cc.redberry.rings.scaladsl.{Frac, IntZ, MultivariatePolynomial, Z}
import org.junit.Test

/**
  *
  */
class TensorMasslessIntegralsTest {

  /** create tensor reduction formula */
  def mkTensorReduction(nPropagators: Int, indices: Seq[String]): Tensor = {
    var expArg = parse("0")
    for (i <- 0 until (nPropagators - 1)) {
      expArg = sum(expArg, parse(s"a_{m}*p^{m} * alpha[$i]"))
    }
    expArg = subtract(expArg, parse("a_{m}*a^{m} / 4"))
    println(expArg)
    val tOp = parse(s"Exp[I * ($expArg) * d]")

    var diff = tOp
    for (i <- indices) {
      val dvar = parseSimple(s"a^$i")
      diff = differentiate(diff, dvar)
    }

    val zSub = expression(parse("a_i"), parse("0"))
    val res = zSub.transform(diff)
    ExpandTransformation.expand(res)
  }


  @Test
  def test1: Unit = {
    println(mkTensorReduction(4, Seq("m", "n", "s", "t")))
  }


  @Test
  def test2: Unit = {
    val calculator = new MasslessIntegrals(Z, databaseFile = Some(mkTempFile()))
    try {
      implicit val ring: Frac[MultivariatePolynomial[IntZ]] = calculator.ring

      val (s12, s23, s34, s45, s15) = ring("s12", "s23", "s34", "s45", "s15")
      val (s13, s14, s24, s25, s35) = ring("s13", "s14", "s24", "s25", "s35")
      val dim = ring("d")

      val reducer = new MasslessReducer(calculator)
      //      val i5 = IntegralDef("I5", Seq(2, 2, 1, 1, 1), Seq(ring(2), s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))
      val i4 = IntegralDef("I4", Seq(2, 2, 1, 1), Seq(ring(2), s12, s23, ring(0), s13, s14, s24))

      println(reducer.reduce(i4, Seq("m", "n"), factorize = true))
      //run i5 --n1 2 --n2 2 --n3 1 --n4 1 --n5 2 --di 2  --s12 0 --s34 s23 -t -f
      //      val i5 = calculator.I5(2, 2, 1, 1, 2, 2,
      //        ring(0), s23, ring(0), s15, s15, ring(0), s14, s14, s25, s35, factorized = true)
      //
      //      //      i5._2.left.get.print(Console.out, PrintFormatter(PrintFormat.MMA, true))
      //      i5._2.right.get.print(Console.out, PrintFormatter(PrintFormat.MMA, true))
    } finally
      calculator.close()
  }
}
