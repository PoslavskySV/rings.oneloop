package cc.redberry.rings.oneloop

import java.io.PrintStream

import scala.language.implicitConversions

object Definitions {

  import cc.redberry.rings.scaladsl._
  import cc.redberry.rings.scaladsl.syntax._

  object PrintFormat extends Enumeration {
    type PrintFormat = Value
    val MMA, Maple, FORM = Value

    def byName(s: String): Option[Value] = values.find(_.toString.equalsIgnoreCase(s))

    private
    def formPrint[E](expr: E)(implicit ring: Ring[E]): String = expr match {
      case frac: Rational[_] => _formPrint(frac.asInstanceOf[Rational[_]])(ring.asInstanceOf[Ring[Any]])
      case _ => ring stringify expr
    }

    private
    def _formPrint[E](expr: Rational[E])(implicit ring: Ring[Any]): String = {
      val cRing = ring.asInstanceOf[Frac[E]].ring
      val num = expr.numerator()
      val den = expr.denominator()
      if (cRing.isOne(den))
        cRing stringify num
      else
        s"frac(${cRing stringify num}, ${cRing stringify den})"
    }

    protected case class Stringifier(fmt: Value) {
      def stringify[E](e: E)(implicit ring: Ring[E]) = fmt match {
        case MMA | Maple => ring stringify e
        case FORM => formPrint(e)
      }
    }

    implicit def value2stringifier(fmt: Value) = Stringifier(fmt)
  }

  import PrintFormat._

  /**
    * Helper data for output printing
    *
    * @param fmt        output format
    * @param tablePrint whether to print in table (multiline) form
    */
  case class PrintFormatter(fmt: PrintFormat, tablePrint: Boolean)

  /**
    * Generic integral definition I[ {n1, n2, ...} , {d, s12, s13, ...}]
    *
    * @param id      string name of integral ("head")
    * @param indices indices of integral
    * @param args    generic kinematic arguments
    */
  case class IntegralDef[E](id: String, indices: Seq[Int], args: Seq[E]) {
    override def toString: String = id + "[" + indices.mkString(",") + ", " + args.mkString(", ") + "]"

    def stringify()(implicit ring: Ring[E]): String = id + "[" + indices.mkString(",") + "," + args.map(ring.stringify).mkString(",") + "]"

    def map(func: E => E): IntegralDef[E] = IntegralDef(id, indices, args.map(func))

    /** Prints expression to a stream */
    def print(stream: PrintStream, formatter: PrintFormatter)(implicit ring: Ring[E]): Unit = {
      stream.print(id)
      stream.print(formatter.fmt match { case MMA => "[" case Maple | FORM => "(" })
      stream.print(indices.mkString(","))
      stream.print(",")
      stream.print(args.map(e => formatter.fmt.stringify(e)(ring)).mkString(","))
      stream.print(formatter.fmt match { case MMA => "]" case Maple | FORM => ")" })
    }
  }

  private
  def normalize[K, E](terms: Map[K, E])(implicit ring: Ring[E]) =
    terms.filterNot(t => ring.isZero(t._2))

  /** Sum of integrals with polynomial coefficients */
  case class IntegralVal[E](terms: Map[IntegralDef[E], E]) {
    def +(oth: IntegralVal[E])(implicit ring: Ring[E]): IntegralVal[E] = {
      import scalaz._
      import Scalaz._

      implicit val sg: Semigroup[E] = (f1, f2) => ring.add(f1, f2)
      IntegralVal(normalize(terms |+| oth.terms))
    }

    def -(oth: IntegralVal[E])(implicit ring: Ring[E]): IntegralVal[E] =
      this + IntegralVal(oth.mapValues(ring.negate))

    def *(oth: E)(implicit ring: Ring[E]): IntegralVal[E] = {
      if (ring.isZero(oth) || isZero)
        IntegralVal.zero
      else
        IntegralVal(normalize(terms.mapValues(_ * oth)))
    }

    def *(oth: Int)(implicit ring: Ring[E]): IntegralVal[E] = {
      this * ring(oth)
    }

    def map(func: E => E): IntegralVal[E] = {
      IntegralVal(terms.map { case (f, c) => (f.map(func), func(c)) })
    }

    def isZero: Boolean = terms.isEmpty

    override def toString: String = terms.map { case (k, v) => "(" + v + ") * " + k }.mkString("+")

    def stringify()(implicit ring: Ring[E]): String =
      if (terms.isEmpty)
        "0"
      else
        terms.map { case (k, v) =>
          "(" + ring.stringify(v) + ") * " + k.stringify()
        }.mkString(" + ")

    /** Prints expression to a stream */
    def print(stream: PrintStream, formatter: PrintFormatter)(implicit ring: Ring[E]): Unit = {
      if (terms.isEmpty)
        stream.print("0")
      else
        terms.toSeq.foldLeft(0) { case (i, (integral, coefficient)) =>
          if (i != 0 || formatter.tablePrint)
            stream.print(" + ")

          integral.print(stream, formatter)
          stream.print(" * ")
          if (formatter.fmt == PrintFormat.FORM)
            stream.print("factor")

          stream.print("(")
          stream.print(formatter.fmt.stringify(coefficient))
          stream.print(")")

          if (formatter.tablePrint)
            stream.print("\n")
          i + 1
        }
    }
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

  /** Sum of integrals with polynomial coefficients */
  case class FactorizedIntegralVal[E](terms: Map[IntegralDef[E], Seq[(E, Int)]]) {
    def map(func: E => E): FactorizedIntegralVal[E] = {
      FactorizedIntegralVal(terms.map { case (f, seq) => (f.map(func), seq.map(e => (func(e._1), e._2))) })
    }

    def +(oth: FactorizedIntegralVal[E])(implicit ring: Ring[E]): FactorizedIntegralVal[E] = {
      if (terms.isEmpty)
        return oth
      if (oth.terms.isEmpty)
        return this

      import scalaz._
      import Scalaz._

      def toE(seq: Seq[(E, Int)]) = ring.multiply(seq.map { case (v, e) => ring.pow(v, e) }: _*)

      implicit val sg: Semigroup[Seq[(E, Int)]] =
        (f1, f2) =>
          if (f1.isEmpty)
            f2
          else if (f2.isEmpty)
            f1
          else
            ring.factor(ring.add(toE(f1), toE(f2))).toSeq

      FactorizedIntegralVal(terms |+| oth.terms)
    }

    def *(oth: E)(implicit ring: Ring[E]): FactorizedIntegralVal[E] =
      FactorizedIntegralVal(terms.mapValues(_ ++ Seq((oth, 1))))


    def print(stream: PrintStream, of: PrintFormatter)(implicit ring: Ring[E]): Unit = {
      if (terms.isEmpty)
        stream.print("0")
      else
        terms.toSeq.foldLeft(0) { case (i, (integral, coefficient)) =>
          if (i != 0 || of.tablePrint)
            stream.print(" + ")
          integral.print(stream, of)
          stream.print(" * ")

          coefficient.foldLeft(0) { case (j, (factor, exp)) =>
            if (j != 0)
              stream.print(" * ")
            if (of.fmt == PrintFormat.FORM)
              stream.print("factor")
            stream.print("(")
            stream.print(of.fmt.stringify(factor))
            stream.print(")")
            if (exp != 1)
              stream.print(s" ^ $exp")
            j + 1
          }

          if (of.tablePrint)
            stream.print("\n")
          i + 1
        }
    }

    def stringify()(implicit ring: Ring[E]) =
      if (terms.isEmpty)
        "0"
      else
        terms.map { case (k, v) =>
          v.map(t => "(" + ring.stringify(t._1) + ")" + (if (t._2 == 1) "" else ("^" + t._2))).mkString(" * ") + " * " + k.stringify()
        }.mkString(" + ")
  }

  object FactorizedIntegralVal {
    def Factor[E](iVal: IntegralVal[E])(implicit ring: Ring[E]) =
      FactorizedIntegralVal(iVal.terms.map { case (k, v) => (k, ring.factor(v).toSeq) })

    def zero[E]()(implicit ring: Ring[E]) = Factor(IntegralVal.zero[E])
  }
}
