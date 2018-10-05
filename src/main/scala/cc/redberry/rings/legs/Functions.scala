package cc.redberry.rings.sym

/**
  * Created by Stanislav on 05.08.2018.
  */
object Functions {

  import cc.redberry.rings.scaladsl._
  import cc.redberry.rings.scaladsl.syntax._


  /**
    * Symbolic function F(x,y,z...)
    *
    * @param id   string name of function
    * @param args arguments
    */
  @SerialVersionUID(1L)
  case class SymFunc[E](id: String, args: Seq[E]) extends Serializable {
    override def toString: String = id + "[" + args.mkString(", ") + "]"

    def stringify()(implicit ring: Ring[E]): String = id + "[" + args.map(ring.stringify).mkString(",") + "]"

    def map(func: E => E): SymFunc[E] = SymFunc(id, args.map(func))
  }

  object SymFunc {
    implicit def asSum[E](f: SymFunc[E])(implicit ring: Ring[E]) = SymFuncSum(Map[SymFunc[E], E](f -> ring(1)))
  }

  case class FuncMethods(str: String) {
    def func[E](args: E*)(implicit ring: Ring[E]): SymFuncSum[E] = SymFuncSum(SymFunc(str, args))
  }

  implicit def funcMethods(str: String): FuncMethods = FuncMethods(str)

  /**
    * Sum of symbolic functions with coefficients
    */
  @SerialVersionUID(1L)
  case class SymFuncSum[E](terms: Map[SymFunc[E], E]) extends Serializable {
    def +(oth: SymFuncSum[E])(implicit ring: Ring[E]): SymFuncSum[E] = {
      import scalaz._
      import Scalaz._

      implicit val semigroup: Semigroup[E] = (f1, f2) => ring.add(f1, f2)
      SymFuncSum(terms |+| oth.terms)
    }

    def -(oth: SymFuncSum[E])(implicit ring: Ring[E]): SymFuncSum[E] =
      this.+(SymFuncSum(oth.mapValues(ring.negate)))

    def *(oth: E)(implicit ring: Ring[E]): SymFuncSum[E] = {
      SymFuncSum(terms.mapValues(_ * oth))
    }

    def *(oth: Int)(implicit ring: Ring[E]): SymFuncSum[E] = {
      this.*(ring(oth))
    }

    def map(func: E => E): SymFuncSum[E] = {
      SymFuncSum(terms.map { case (f, c) => (f.map(func), func(c)) })
    }

    override def toString: String = terms.map { case (k, v) => "(" + v + ") * " + k }.mkString("+")

    def stringify()(implicit ring: Ring[E]): String = terms.map { case (k, v) =>
      "(" + ring.stringify(v) + ") * " + k.stringify()
    }.mkString("+")
  }

  object SymFuncSum {
    def apply[E](func: SymFunc[E])(implicit ring: Ring[E]): SymFuncSum[E] = SymFunc.asSum(func)

    def zero[E] = SymFuncSum(Map.empty[SymFunc[E], E])

    implicit def asMap[E](sum: SymFuncSum[E]): Map[SymFunc[E], E] = sum.terms
  }

  case class EnrichedRingElement[E](self: E)(implicit ring: Ring[E]) {
    def *(sym: SymFuncSum[E]): SymFuncSum[E] = sym.*(self)
  }

  implicit def enrichedRingElementSyntax[E](el: E)(implicit ring: Ring[E]) = EnrichedRingElement(el)
}
