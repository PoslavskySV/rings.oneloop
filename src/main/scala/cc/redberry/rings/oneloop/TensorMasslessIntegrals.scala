package cc.redberry.rings.oneloop

import cc.redberry.core.tensor.{Sum, Tensor}
import cc.redberry.core.tensor.Tensors.{expression, parse, parseSimple}
import cc.redberry.core.transformations.DifferentiateTransformation.differentiate
import cc.redberry.core.utils.TensorUtils
import cc.redberry.rings.oneloop.Definitions.{IntegralDef, IntegralVal, normalize}
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.{Frac, MultivariateRing}

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import cc.redberry.rings.scaladsl.syntax._

case class TensorIntegral[I](terms: Map[Tensor, I]) {
  def *(i: I)(implicit ring: Ring[I]): TensorIntegral[I] =
    TensorIntegral(terms.map { case (k, v) => (k, v * i) })

  def +(i: TensorIntegral[I])(implicit ring: Ring[I]): TensorIntegral[I] = {

    import scalaz._
    import Scalaz._

    implicit val semigroup: Semigroup[I] = (f1, f2) => ring.add(f1, f2)
    TensorIntegral(terms |+| i.terms)
  }
}

class MasslessReducer[E](val msms: MasslessIntegrals[E]) {
  implicit val ring = msms.ring

  def applyReduction(formula: Tensor,
                     integral: IntegralDef[msms.Expr],
                     factorize: Boolean)
  : TensorIntegral[msms.Result] = {
    val terms: Seq[Tensor] =
      if (formula.isInstanceOf[cc.redberry.core.tensor.Sum])
        formula.asScala.toSeq
      else
        Seq(formula)

    TensorIntegral(terms.map { term =>
      assert(term.isInstanceOf[cc.redberry.core.tensor.Product])
      val dExponent = TensorUtils.Exponent(term, parse("d"))
      val tPart = expression(parse("d"), parse("1")).transform(term)
      val iPart =
        msms.evaluate(
          if (dExponent != 0) {
            val args = ArrayBuffer(integral.args: _*)
            import cc.redberry.rings.scaladsl.syntax._
            args(0) = args(0) + 2
            integral.copy(args = args)
          } else {
            integral
          })
      (tPart, iPart._2)
    }.toMap)
  }

  def reduce(integral: IntegralDef[msms.Expr], indices: Seq[String], factorize: Boolean)
  : TensorIntegral[msms.Result] =
    applyReduction(MasslessReducer.mkReduction(indices), integral, factorize)

  def applyReduction(formula: Tensor,
                     integral: IntegralVal[msms.Expr])
  : TensorIntegral[msms.Result] = {
    val t = integral.terms.map { case (int, cf: msms.Expr) => {
      val summand = applyReduction(formula, int, false)
    }
      //      (cf, applyReduction(formula, int, false).terms.map { case (k, v) => (k, v.left.get) })
    }

    ???
  }

}
/**
  *
  */
object MasslessReducer {
  def mkReduction(indices: Seq[String]): Tensor = {
    val tOp = parse("Exp[i * a_{m}*a^{m} / 4 * d]")

    var diff = tOp
    for (i <- indices) {
      val dvar = parseSimple(s"a^$i")
      diff = differentiate(diff, dvar)
    }

    val zSub = expression(parse("a_i"), parse("0"))
    val res = zSub.transform(diff)
    res
  }
}
