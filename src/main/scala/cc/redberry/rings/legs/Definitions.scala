package cc.redberry.rings.sym

import scala.language.implicitConversions

object Definitions {

  import cc.redberry.rings.scaladsl._
  import cc.redberry.rings.scaladsl.syntax._

  /**
    * Generic integral definition I[ {n1, n2, ...} , {s12, s13, ...}]
    *
    * @param id      string name of integral ("head")
    * @param indices indices of integral
    * @param args    generic kinematic arguments
    */
  case class IntegralDef[E](id: String, indices: Seq[Int], args: Seq[E]) {
    override def toString: String = id + "[" + indices.mkString(",") + ", " + args.mkString(", ") + "]"

    def stringify()(implicit ring: Ring[E]): String = id + "[" + indices.mkString(",") + ", " + args.map(ring.stringify).mkString(",") + "]"

    def map(func: E => E): IntegralDef[E] = IntegralDef(id, indices, args.map(func))
  }

  /** Sum of integrals with polynomial coefficients */
  case class IntegralVal[E](terms: Map[IntegralDef[E], E]) {
    def +(oth: IntegralVal[E])(implicit ring: Ring[E]): IntegralVal[E] = {
      import scalaz._
      import Scalaz._

      implicit val semigroup: Semigroup[E] = (f1, f2) => ring.add(f1, f2)
      IntegralVal(terms |+| oth.terms)
    }

    def -(oth: IntegralVal[E])(implicit ring: Ring[E]): IntegralVal[E] =
      this + IntegralVal(oth.mapValues(ring.negate))

    def *(oth: E)(implicit ring: Ring[E]): IntegralVal[E] = {
      IntegralVal(terms.mapValues(_ * oth))
    }

    def *(oth: Int)(implicit ring: Ring[E]): IntegralVal[E] = {
      this * ring(oth)
    }

    def map(func: E => E): IntegralVal[E] = {
      IntegralVal(terms.map { case (f, c) => (f.map(func), func(c)) })
    }

    override def toString: String = terms.map { case (k, v) => "(" + v + ") * " + k }.mkString("+")

    def stringify()(implicit ring: Ring[E]): String = terms.map { case (k, v) =>
      "(" + ring.stringify(v) + ") * " + k.stringify()
    }.mkString("+")
  }

  object IntegralVal {
    def apply[E](f: IntegralDef[E])(implicit ring: Ring[E]): IntegralVal[E] = IntegralVal(Map[IntegralDef[E], E](f -> ring(1)))

    def apply[E](str: String, indices: Seq[Int], args: Seq[E])(implicit ring: Ring[E]): IntegralVal[E] = apply(IntegralDef(str, indices, args))

    def zero[E] = IntegralVal(Map.empty[IntegralDef[E], E])

    implicit def asMap[E](sum: IntegralVal[E]): Map[IntegralDef[E], E] = sum.terms
  }

  case class RingElementOps[E](self: E)(implicit ring: Ring[E]) {
    def *(sym: IntegralVal[E]): IntegralVal[E] = sym.*(self)
  }

  implicit def ringElementOps[E](el: E)(implicit ring: Ring[E]): RingElementOps[E] = RingElementOps(el)
}
