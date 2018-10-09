package cc.redberry.rings.sym

import scala.language.implicitConversions

object Definitions {

  import cc.redberry.rings.scaladsl._
  import cc.redberry.rings.scaladsl.syntax._

  /**
    * Generic integral definition I[n1,n2,s12...]
    *
    * @param id   string name of integral ("head")
    * @param args generic arguments
    */
  case class IntegralDef[E](id: String, args: Seq[E]) {
    override def toString: String = id + "[" + args.mkString(", ") + "]"

    def stringify()(implicit ring: Ring[E]): String = id + "[" + args.map(ring.stringify).mkString(",") + "]"

    def map(func: E => E): IntegralDef[E] = IntegralDef(id, args.map(func))
  }

  /** Sum of integrals */
  case class IntegralValue[E](terms: Map[IntegralDef[E], E]) {
    def +(oth: IntegralValue[E])(implicit ring: Ring[E]): IntegralValue[E] = {
      import scalaz._
      import Scalaz._

      implicit val semigroup: Semigroup[E] = (f1, f2) => ring.add(f1, f2)
      IntegralValue(terms |+| oth.terms)
    }

    def -(oth: IntegralValue[E])(implicit ring: Ring[E]): IntegralValue[E] =
      this + IntegralValue(oth.mapValues(ring.negate))

    def *(oth: E)(implicit ring: Ring[E]): IntegralValue[E] = {
      IntegralValue(terms.mapValues(_ * oth))
    }

    def *(oth: Int)(implicit ring: Ring[E]): IntegralValue[E] = {
      this * ring(oth)
    }

    def map(func: E => E): IntegralValue[E] = {
      IntegralValue(terms.map { case (f, c) => (f.map(func), func(c)) })
    }

    override def toString: String = terms.map { case (k, v) => "(" + v + ") * " + k }.mkString("+")

    def stringify()(implicit ring: Ring[E]): String = terms.map { case (k, v) =>
      "(" + ring.stringify(v) + ") * " + k.stringify()
    }.mkString("+")
  }

  object IntegralValue {
    def apply[E](str: String, args: E*)(implicit ring: Ring[E]): IntegralValue[E] = IntegralValue(IntegralDef(str, args))

    def apply[E](f: IntegralDef[E])(implicit ring: Ring[E]): IntegralValue[E] = IntegralValue(Map[IntegralDef[E], E](f -> ring(1)))

    def zero[E] = IntegralValue(Map.empty[IntegralDef[E], E])

    implicit def asMap[E](sum: IntegralValue[E]): Map[IntegralDef[E], E] = sum.terms
  }

  case class EnrichedRingElement[E](self: E)(implicit ring: Ring[E]) {
    def *(sym: IntegralValue[E]): IntegralValue[E] = sym.*(self)
  }

  implicit def enrichedRingElementSyntax[E](el: E)(implicit ring: Ring[E]) = EnrichedRingElement(el)
}
